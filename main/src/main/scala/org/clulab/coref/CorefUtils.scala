package org.clulab.coref

import org.clulab.odin.Mention
import org.clulab.reach.grounding.ReachKBConstants
import org.clulab.reach.mentions._
import org.clulab.reach.utils.DependencyUtils._
import org.clulab.struct.Interval

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Utility functions for use with CorefMentions
 * User: danebell
 * Date: 11/10/15
 */
object CorefUtils {
  /**
   * Alternate hash taking antecedents (and antecedents of arguments) into account for proper "distinct"
   */
  def altHash(m: CorefMention): Int = {
    m.hashCode() * 42 * 42 +
      m.arguments.values.flatten.map(a => altHash(a.toCorefMention)).sum * 42 +
      m.antecedents.map(a => altHash(a.asInstanceOf[CorefMention])).sum
  }

  /**
   * Basically the same as "distinct", but using CorefMention's antecedents
   */
  def corefDistinct(ms: Seq[CorefMention]): Seq[CorefMention] = {
    var b: Seq[CorefMention] = Nil
    val seen = mutable.Set[Int]()
    for (x <- ms) {
      val lu = altHash(x)
      if (!seen(lu)) {
        b = b :+ x
        seen += lu
      }
    }
    b
  }

  /**
   * Is the mention generic, e.g. "it", or does it have an argument containing a generic mention,
   * e.g. "It is phosphorylated"?
   */
  def genericInside (m: CorefMention): Boolean = {
    @tailrec def genericInsideRec(ms: Seq[CorefMention]): Boolean = {
      if (ms.exists(m => m.isGeneric || m.hasGenericMutation)) true
      else {
        val (tbs, others) = ms.partition(mention => mention.isInstanceOf[CorefTextBoundMention])
        if (others.isEmpty) false
        else genericInsideRec(others.flatMap(_.arguments.values.flatten.map(_.toCorefMention)))
      }
    }
    m.isGeneric || genericInsideRec(Seq(m))
  }

  /**
   * Are the arguments for this mention complete? Phosphorylations require a theme, etc.
   */
  def argsComplete(args: Map[String,Seq[CorefMention]], lbls: Seq[String]): Boolean = {
    lbls match {
      case binding if lbls contains "Binding" =>
        args.contains("theme") && args("theme").length >= 2 // Binding is not required to be binary anymore (see binding_token_5)
      case simple if (lbls contains "SimpleEvent" )|| (lbls contains "Association") =>
        ((args.contains("theme") && args("theme").nonEmpty) ||
          (args.contains("substrate") && args("substrate").nonEmpty &&
            args.contains("product") && args("product").nonEmpty))
      case complex if lbls contains "ComplexEvent" =>
        args.contains("controller") && args.contains("controlled") &&
          args("controller").nonEmpty && args("controlled").nonEmpty
      case _ => true
    }
  }

  /**
    * From a mention, use the dependency graph to expand the interval to the noun phrase the mention is a part of
    */
  def expand(mention: Mention): Interval = {
    val sent = mention.document.sentences(mention.sentence)
    val graph = sent.dependencies.getOrElse(return mention.tokenInterval)

    val localHead = findHeadStrict(mention.tokenInterval, sent).getOrElse(mention.tokenInterval.end - 1)

    var npHead = localHead

    var searchingHead = true

    // keep traversing incomingEdges until you reach the head of the NP
    while (searchingHead) {
      val newHead = try {
        graph.getIncomingEdges(npHead).find(edge => edge._2 == "nn")
      } catch {
        case e: Throwable => None
      }
      if (newHead.isDefined) npHead = newHead.get._1
      else searchingHead = false
    }

    subgraph(Interval(npHead), sent).getOrElse(mention.tokenInterval)
  }

  def compatibleMutants(a: CorefMention, b: CorefMention): Boolean = {
    val sameMutants = a.mutants.filterNot(_.isGeneric).forall(am => b.mutants.exists(_.text == am.text)) &&
      b.mutants.filterNot(_.isGeneric).forall(bm => a.mutants.exists(_.text == bm.text))
    val subsetMutants = a.mutants.filterNot(_.isGeneric).forall(am => b.mutants.exists(_.text == am.text)) ||
      b.mutants.filterNot(_.isGeneric).forall(bm => a.mutants.exists(_.text == bm.text))
    if (sameMutants && !a.hasGenericMutation && !b.hasGenericMutation) true
    else if (subsetMutants && a.hasGenericMutation && b.mutants.nonEmpty) true
    else if (subsetMutants && b.hasGenericMutation && a.mutants.nonEmpty) true
    else false
  }

  /**
    * Do two mentions have groundings that match? E.g. 'H-Ras' (a family) and 'S135' (a site)
    * are not compatible because they don't have the same labels
    *
    * @param a
    * @param b
    */
  def compatibleGrounding(a: CorefMention, b: CorefMention): Boolean = {
    a.isInstanceOf[CorefTextBoundMention] && b.isInstanceOf[CorefTextBoundMention] &&
      a.label == b.label &&
      a.nonGeneric && b.nonGeneric &&
      compatibleContext(a, b) &&
      a.isGrounded && b.isGrounded
  }

  /**
    * Do two mentions have contexts that match?
    *
    * @param a
    * @param b
    */
  def compatibleContext(a: CorefMention, b: CorefMention): Boolean = {
    val aContext = a.context.getOrElse(Map[String,Seq[String]]())
    val bContext = b.context.getOrElse(Map[String,Seq[String]]())
    a.label == b.label &&
      aContext.keySet.intersect(bContext.keySet)
        .forall(k => aContext(k).toSet.intersect(bContext(k).toSet).nonEmpty) // FIXME: Too strict?
  }

  def depth(m: CorefMention): Int = m match {
    case tbm: CorefTextBoundMention => 0

    case evt: CorefEventMention =>
      val argDepths = evt
      .antecedentOrElse(evt)
      .arguments
      .values
      .flatten
      .map(arg => depth(arg.toCorefMention))
      .toList

      if (argDepths.isEmpty) 0 else 1 + argDepths.max

    case rel: CorefRelationMention =>
      val argDepths = rel
      .antecedentOrElse(rel)
      .arguments
      .values
      .flatten
      .map(arg => depth(arg.toCorefMention))
      .toList

      if (argDepths.isEmpty) 0 else 1 + argDepths.max

    case impossible => 0
  }

  /**
    * Return all the text-bound mentions of this mention's arguments (recursively)
    */
  def collapseArguments(mention: Mention): Set[Mention] = {
    if (mention.isInstanceOf[CorefTextBoundMention]) return Set(mention)
    mention.arguments.values.flatten.flatMap(m => collapseArguments(m)).toSet
  }

  /** Return true if at least one event has both as (nested) arguments, given two [[Mention]]s */
  def coArguments(generic: Mention, nonGeneric: Mention, evts: Seq[Mention]): Boolean = {
    val argSets = evts.map(collapseArguments(_))
    argSets.exists(args =>
      args.contains(generic) &&
      args.exists(a => a.toBioMention.sharesGroundingWith(nonGeneric.toBioMention))
    )
  }
}
