package edu.arizona.sista.assembly

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.RuleReader

class Sieves(mentions: Seq[Mention]) {

  val reachMentions = mentions

  /**
   * populates an AssemblyManager with mentions (default behavior of AssemblyManager)
   * @param mentions a sequence of Odin Mentions
   * @param manager an AssemblyManager
   * @return an AssemblyManager with representations for each relevant mention
   */
  def trackMentions(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {
    val am = AssemblyManager()
    am.trackMentions(mentions)
    am
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
        // ensure that mention is one related to Assembly
        // we don't want to return things from the old state
        if m matches precedenceMentionLabel
      } yield m

    assembledMentions
      .toSeq
  }
}
