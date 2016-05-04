package edu.arizona.sista.assembly.sieves

import edu.arizona.sista.assembly.AssemblyManager
import edu.arizona.sista.assembly.representations.{Complex, Event, SimpleEntity}
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.RuleReader
import edu.arizona.sista.reach.display._

/**
 * Contains all sieves of the signature (mentions: Seq[Mention], manager: AssemblyManager) => AssemblyManager.
 * @param mentions a Seq of Odin Mentions
 */
class Sieves(mentions: Seq[Mention]) {

  import Constraints._
  import SieveUtils._

  val reachMentions = mentions

  /**
    * Populates an AssemblyManager with mentions (default behavior of AssemblyManager)
    *
    * @param mentions a sequence of Odin Mentions
    * @param manager  an AssemblyManager
    * @return an AssemblyManager
    */
  def trackMentions(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {
    val am = AssemblyManager()
    am.trackMentions(mentions)
    am
  }

  /**
    * Rule-based method for detecting precedence relations
    *
    * @param mentions a sequence of Odin Mentions
    * @param manager  an AssemblyManager
    * @return an AssemblyManager
    */
  def ruleBasedPrecedence(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    val p = "/edu/arizona/sista/assembly/grammars/precedence.yml"

    val name = "ruleBasedPrecedence"
    // find rule-based PrecedenceRelations
    for {
      rel <- assemblyViaRules(p, mentions)
      before = rel.arguments.getOrElse(beforeRole, Nil)
      after = rel.arguments.getOrElse(afterRole, Nil)
      if before.nonEmpty && after.nonEmpty
      // both "before" and "after" should have single mentions
      b = before.head
      a = after.head
      // cannot be an existing regulation
      if notAnExistingComplexEvent(rel) && noExistingPrecedence(a, b, manager)
    } {
      // store the precedence relation
      manager.storePrecedenceRelation(b, a, Set(rel), name)
    }

    manager
  }

  /**
    * Rule-based method using grammatical tense and aspect to establish precedence
    *
    * @param mentions a sequence of Odin Mentions
    * @param manager  an AssemblyManager
    * @return an AssemblyManager
    */
  def tamPrecedence(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    def getTam(ev: Mention, tams: Seq[Mention], label: String): Option[Mention] = {
      val relevant: Set[Mention] = tams.filter(tam =>
        (tam matches label) && (tam.tokenInterval overlaps ev.tokenInterval)).toSet
      // rules should produce at most one tense or aspect mention
      // TODO: This is debugging, so only log when debugging
      // if (relevant.size > 1 ) {
      //   println(s"""Found TAMs of ${relevant.map(_.label).mkString(", ")} for ${ev.text}""")
      // }
      relevant.headOption
    }

    def tamLabel(tam: Option[Mention]): String = {
      tam match {
        case None => "none"
        case _ => tam.get.label
      }
    }

    def getReichenbach(e1t: String, e1a: String, e2t: String, e2a: String): String = {
      (e1t, e1a, e2t, e2a) match {
        case ("PastTense", "none", "PastTense", "Perfective") => "after"
        case ("PastTense", "none", "FutureTense", "none") => "before"
        case ("PastTense", "none", "FutureTense", "Perfective") => "before"
        //
        case ("PastTense", "Perfective", "PastTense", "none") => "before"
        case ("PastTense", "Perfective", "PresentTense", "none") => "before"
        case ("PastTense", "Perfective", "PresentTense", "Perfective") => "before"
        case ("PastTense", "Perfective", "FutureTense", "none") => "before"
        case ("PastTense", "Perfective", "FutureTense", "Perfective") => "before"
        //
        case ("PresentTense", "none", "PastTense", "Perfective") => "after"
        case ("PresentTense", "none", "FutureTense", "none") => "before"
        //
        case ("PresentTense", "Perfective", "PastTense", "Perfective") => "after"
        case ("PresentTense", "Perfective", "FutureTense", "none") => "before"
        case ("PresentTense", "Perfective", "FutureTense", "Perfective") => "before"
        //
        case ("FutureTense", "none", "PastTense", "none") => "after"
        case ("FutureTense", "none", "PastTense", "Perfective") => "after"
        case ("FutureTense", "none", "PresentTense", "none") => "after"
        case ("FutureTense", "none", "PresentTense", "Perfective") => "after"
        //
        case ("FutureTense", "Perfective", "PastTense", "none") => "after"
        case ("FutureTense", "Perfective", "PastTense", "Perfective") => "after"
        case ("FutureTense", "Perfective", "PresentTense", "Perfective") => "after"
        case _ => "none"
      }
    }

    val name = "tamPrecedence"
    val tam_rules = "/edu/arizona/sista/assembly/grammars/tense_aspect.yml"

    // TODO: only look at events with verbal triggers
    val evs = mentions.filter(isEvent)
    val tams = assemblyViaRules(tam_rules, mentions)
    val tenseMentions = tams.filter(_ matches "Tense")
    val aspectMentions = tams.filter(_ matches "Aspect")

    for {
      events <- evs.groupBy(_.document).values
      e1 <- events
      e2 <- events
      if isValidRelationPair(e1, e2) && noExistingPrecedence(e1, e2, manager)

      e1tense = getTam(e1, tenseMentions, "Tense")
      e1aspect = getTam(e1, aspectMentions, "Aspect")
      e2tense = getTam(e2, tenseMentions, "Tense")
      e2aspect = getTam(e2, aspectMentions, "Aspect")
      pr = getReichenbach(tamLabel(e1tense), tamLabel(e1aspect), tamLabel(e2tense), tamLabel(e2aspect))
    } {
      pr match {
        case "before" =>
          manager.storePrecedenceRelation(e1, e2, Seq(e1tense, e1aspect, e2tense, e2aspect).flatten.toSet, name)
        case "after" =>
          manager.storePrecedenceRelation(e2, e1, Seq(e1tense, e1aspect, e2tense, e2aspect).flatten.toSet, name)
        case _ => ()
      }
    }

    manager
  }


  /**
    * Rule-based method for detecting inter-sentence precedence relations
    *
    * @param mentions a sequence of Odin Mentions
    * @param manager  an AssemblyManager
    * @return an AssemblyManager
    */
  def betweenRbPrecedence(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    // If the full Event is nested within another mention, pull it out
    def correctScope(m: Mention): Mention = {
      m match {
        // arguments will have at most one "event" argument
        case nested if nested.arguments contains "event" => nested.arguments("event").head
        case flat => flat
      }
    }

    val p = "/edu/arizona/sista/assembly/grammars/intersentential.yml"

    val name = "betweenRbPrecedence"
    // find rule-based inter-sentence PrecedenceRelations
    for {
      rel <- assemblyViaRules(p, mentions)
      // TODO: Decide whether to restrict matches more, e.g. to last of prior sentence
      other <- mentions.filter(m => isEvent(m) && m.document == rel.document && m.sentence == rel.sentence - 1)
      (before, after) = rel.label match {
        case "InterAfter" => (Seq(other), rel.arguments("after"))
        case "InterBefore" => (rel.arguments("before"), Seq(other))
        case _ => (Nil, Nil)
      }
      if before.nonEmpty && after.nonEmpty
      b = correctScope(before.head)
      a = correctScope(after.head)

      if isValidRelationPair(a, b) && noExistingPrecedence(a, b, manager)
    } {
      // store the precedence relation
      manager.storePrecedenceRelation(b, a, Set(rel), name)
    }

    manager
  }

}



/**
 * Utilities commonly used by Sieves
 */
object SieveUtils {


  // the label used to identify Mentions modelling precedence relations
  val precedenceMentionLabel = "Precedence"

  /**
   * Applies a set of assembly rules to provide mentions (existingMentions).
   * Care is taken to apply the rules to each set of mentions from the same Document.
   * @param rulesPath a path to a rule file (under resources)
   * @param existingMentions a Seq of Odin Mentions
   * @return a Seq of RelationMentions
   */
  def assemblyViaRules(rulesPath: String, existingMentions: Seq[Mention]): Seq[Mention] = {

    // read rules and initialize state with existing mentions
    val rules:String = RuleReader.readResource(rulesPath)
    val ee = ExtractorEngine(rules)

    // since we break a paper into sections, we'll need to group the mentions by doc
    // rule set only produces target RelationMentions

    val assembledMentions: Iterable[Mention] =
      for {
        // NOTE: Odin expects all mentions in the state to belong to the same doc!
        (doc, mentionsFromReach) <- existingMentions.groupBy(_.document)
        // create a new state with just the mentions from a particular doc
        // note that a doc is as granular as a section of a paper
        oldState = State(mentionsFromReach)
        // initialize a state with the subset of mentions
        // belonging to the same doc.
        m <- ee.extractFrom(doc, oldState)
        // _ = if (m matches precedenceMentionLabel) displayMention(m)
        // ensure that mention is one related to Assembly
        // we don't want to return things from the old state
        if m matches precedenceMentionLabel
      } yield m

    assembledMentions
      .toSeq
  }

  /**
    * Returns true if the mention is an Event and therefore a candidate for precedence
    * @param m an Odin Mention
    * @return a Boolean
    */
  def isEvent(m:Mention) = m.matches("Event") && !m.isInstanceOf[TextBoundMention]
}
