package org.clulab.assembly

import org.clulab.assembly.export.{Equivalence, CausalPrecedence}
import org.clulab.odin.Mention


/**
  * Assembler for reach output
  * @param mns a sequence of Odin-style Mentions
  *   Written by: Gus Hahn-Powell. 5/9/2016.
  *   Last Modified: Comment out extraneous outputs and anticipated methods.
  */
class Assembler(mns: Seq[Mention]) {
  // keep only the valid mentions
  val mentions = mns.filter(AssemblyManager.isValidMention)
  val am = AssemblyRunner.applySieves(mentions)

  val causalPredecessors: Map[Mention, Set[CausalPrecedence]] = {
    val links = for {
      m <- mentions
      eer = am.getEER(m)
      eh = eer.equivalenceHash
      pr <- am.getPrecedenceRelations(eh)
      after = pr.after
      // current mention should be the successor
      if after == eh
      e <- pr.evidence
      // only consider links with well-formed evidence
      if e.arguments.contains("before") && e.arguments.contains("after")
      b <- e.arguments("before")
      a <- e.arguments("after")
    } yield CausalPrecedence(before = b, after = a, pr.foundBy)
    // build map from pairs
    links.groupBy(_.after).mapValues(_.toSet)
  }

  /**
   * Returns the set of CausalPrecedence links where m is the [[CausalPrecedence.after]]
   * @param m an Odin-style Mention
   */
  def getCausalPredecessors(m: Mention): Set[CausalPrecedence] =
    causalPredecessors.getOrElse(m, Set.empty[CausalPrecedence])

  val equivalenceLinks: Map[Mention, Set[Equivalence]] = {
    val links = for {
      m <- mentions
      if AssemblyManager.isValidMention(m)
      eer = am.getEER(m)
      equivMentions = am.getEvidence(eer)
      // remove m from its own equivalence set
      e <- equivMentions - m
    } yield Equivalence(m, e, "AssemblyManager")
    // build map from pairs
    links.groupBy(_.m1).mapValues(_.toSet)
  }

  def getEquivalenceLinks(m: Mention): Set[Equivalence] =
    equivalenceLinks.getOrElse(m, Set.empty[Equivalence])

}
