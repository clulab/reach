package org.clulab.polarity

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.Mention
import org.clulab.reach.mentions.BioEventMention
import org.clulab.struct.DirectedGraph
import org.clulab.utils.DependencyUtils

object LinguisticPolarityEngine extends PolarityEngine with LazyLogging{

  // These are used to detect semantic inversions of regulations/activations. See DarpaActions.countSemanticNegatives
  // since I need to use this list in other methods, I make it public
  val SEMANTIC_NEGATIVE_PATTERN = "(?i)(^(attenu|block|deactiv|decreas|deficien|degrad|delet|deplet|diminish|disrupt|dominant-negative|impair|imped|inhibit|knockdown|knockout|limit|loss|lower|negat|reduc|reliev|repress|restrict|revers|silenc|shRNA|siRNA|slow|starv|suppress|supress|turnover|off)|-KD$)".r

  private val MODIFIER_LABELS = "amod".r

  private val NOUN_LABELS = "compound".r

  private val OF_LABELS = "nmod_of".r

  private val PARTICLE_LABELS = "compound:prt".r

  /**
    * Computes polarity by using the linguistically informed approach
    *
    * @param evt BioEventMention to be operated on
    * @return Positive or Negative polarity instance
    */
  override def computePolarity(evt: BioEventMention): Polarity = {

    if(evt matches "ComplexEvent"){
      println("!!!!!!!!!!")
      scala.io.StdIn.readLine()

      val trigger = evt.trigger
      //val arguments = ce.arguments
      val arguments = for{
        k <- evt.arguments.keys
        v <- evt.arguments(k)
      } yield (k, v)

      // get token indices to exclude in the negation search
      // do not exclude args as they may involve regulations
      val excluded = trigger.tokenInterval.toSet /*| (arguments flatMap (_._2.tokenInterval)).toSet*/

      // tokens with an incoming  prepc_by dependency
      val deps = evt.sentenceObj.dependencies.get
      val prepc_byed = (evt.tokenInterval filter (tok => deps.getIncomingEdges(tok).map(_._2).contains("advcl_by"))).toSet
      // count total number of negatives between trigger and each argument
      val numNegatives = arguments.flatMap{
        case (relation, arg) =>
          countSemanticNegatives(trigger, arg, if(relation == "controller") excluded else excluded ++ prepc_byed)
      }.toSeq.distinct.length

      logger.debug(s"Total negatives: $numNegatives")

      val initialPolarity = getPolarityFromLabel(evt)

      // does the label need to be flipped?
      if (numNegatives % 2 != 0) {
        initialPolarity match {
          case PositivePolarity => NegativePolarity
          case NegativePolarity => PositivePolarity
          case NeutralPolarity =>
            logger.debug("Flipping polarity of a neutral-polarity complex event.")
            NeutralPolarity
        }
      } else
        initialPolarity

    }
    else
      NeutralPolarity
  }

  /** Gets a trigger, an argument and a set of tokens to be ignored.
    * Returns the number of semantic negatives found in the shortest possible path
    * between the trigger and the argument.
    */
  private def countSemanticNegatives(trigger: Mention, arg: Mention, excluded: Set[Int]): Seq[Int] = {
    // it is possible for the trigger and the arg to be in different sentences because of coreference
    if (trigger.sentence != arg.sentence) return Nil
    val deps = trigger.sentenceObj.dependencies.get
    // find shortestPath between the trigger head token and the argument head token
    val shortestPath: Option[Seq[Int]] = for {
      triggerHead <- DependencyUtils.findHeadStrict(trigger.tokenInterval, trigger.sentenceObj)
      argHead <- DependencyUtils.findHeadStrict(arg.tokenInterval, arg.sentenceObj)
    } yield deps.shortestPath(triggerHead, argHead, ignoreDirection = true)

    //println("Trigger: " + trigger.start + " -> " + trigger.end + " " + trigger.label)
    //println("Argument: " + arg.start + " -> " + arg.end + " " + arg.label)
    //println(s"Shortest path: ${shortestPath.get.mkString(", ")} in sentence ${trigger.sentenceObj.words.mkString(", ")}")

    shortestPath match {
      case None => Nil
      case Some(path) =>
        val shortestPathWithAdjMods = addAdjectivalModifiers(path, deps)
        val nnMods = nounModifiers(arg.tokenInterval, deps)
        val ofMods = ofModifiers(arg.tokenInterval, deps)
        val prpMods = particleModifiers(path, deps)
        // get all tokens considered negatives
        val negatives = for {
          tok <- (shortestPathWithAdjMods ++ nnMods ++ ofMods ++ prpMods).distinct // a single token can't negate twice
          if !excluded.contains(tok)
          lemma = trigger.sentenceObj.lemmas.get(tok)
          if SEMANTIC_NEGATIVE_PATTERN.findFirstIn(lemma).isDefined
        } yield {
          logger.debug(s"Negative lexical unit: $lemma")
          tok
        }
        // return number of negatives
        negatives
    }
  }

  /**
    * Adds adjectival modifiers to all elements in the given path
    * This is necessary so we can properly inspect the semantic negatives,
    *   which are often not in the path, but modify tokens in it,
    *   "*decreased* PTPN13 expression increases phosphorylation of EphrinB1"
    */
  def addAdjectivalModifiers(tokens: Seq[Int], deps: DirectedGraph[String]): Seq[Int] = for {
    t <- tokens
    token <- t +: getModifiers(t, deps)
  } yield token

  def getModifiers(token: Int, deps: DirectedGraph[String]): Seq[Int] = for {
    (tok, dep) <- deps.getOutgoingEdges(token)
    if MODIFIER_LABELS.findFirstIn(dep).isDefined
  } yield tok

  def nounModifiers(tokens: Seq[Int], deps: DirectedGraph[String]): Seq[Int] = for {
    t <- tokens
    token <- t +: getNounModifiers(t, deps)
  } yield token

  def getNounModifiers(token: Int, deps: DirectedGraph[String]): Seq[Int] = for {
    (tok, dep) <- deps.getIncomingEdges(token) ++ deps.getOutgoingEdges(token) // NB: *Incoming* edges, for e.g. "Stat3 siRNA"
    if NOUN_LABELS.findFirstIn(dep).isDefined
  } yield tok

  def particleModifiers(tokens:Seq[Int], deps: DirectedGraph[String]): Seq[Int] = for {
    t <- tokens
    token <- t +: getParticleModifiers(t, deps)
  } yield token

  def getParticleModifiers(token: Int, deps: DirectedGraph[String]): Seq[Int] = for {
    (tok, dep) <- deps.getOutgoingEdges(token)
    if PARTICLE_LABELS.findFirstIn(dep).isDefined
  } yield tok

  def ofModifiers(tokens: Seq[Int], deps: DirectedGraph[String]): Seq[Int] = for {
    t <- tokens
    token <- t +: getOfModifiers(t, deps)
  } yield token

  def getOfModifiers(token: Int, deps: DirectedGraph[String]): Seq[Int] = for {
    (tok, dep) <- deps.getIncomingEdges(token) // NB: *Incoming* edges, for e.g. "knockdown of Stat3"
    if OF_LABELS.findFirstIn(dep).isDefined
  } yield tok
}
