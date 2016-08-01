package org.clulab.assembly

import org.clulab.assembly.export.{Equivalence, CausalPrecedence}
import org.clulab.odin.Mention


/**
  * Assembler for reach output
  * @param mns a sequence of Odin-style Mentions
  *   Written by: Gus Hahn-Powell. 5/9/2016.
  *   Last Modified: Add method to get input features by participants.
  */
class Assembler(mns: Seq[Mention]) {
  // keep only the valid mentions
  val mentions = mns.filter(AssemblyManager.isValidMention)
  val am = AssemblyRunner.applySieves(mentions)

  private val participantFeatureTracker = new ParticipantFeatureTracker(am)

  val causalPredecessors: Map[Mention, Set[CausalPrecedence]] = {
    val links = for {
      m <- mentions
      eer = am.getEER(m)
      pr <- am.getPrecedenceRelationsFor(eer)
      after = pr.after
      // current mention should be the successor
      if after.equivalenceHash == eer.equivalenceHash
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
    * For the given parent event mention, find the features for the event's participants
    * and return them in a map, keyed by each participant of the given parent event.
    */
  def getInputFeaturesByParticipants (parent: Mention): Map[Mention,RoleWithFeatures] = {
    val features = getInputFeaturesForParticipants(parent)
    features.map(rwfs => rwfs.participant -> rwfs).toMap
  }

  /**
    * For each participant of an event, retrieve the set union of relevant PTMs (i.e., those specific to the participant) <br>
    * from the event's causal predecessors.
    * @param parent an event mention
    * @return a Seq[[RoleWithFeatures]]
    */
  def getInputFeaturesForParticipants(parent: Mention): Seq[RoleWithFeatures] = {
    val rwfs = for {
      (role, mns) <- parent.arguments
      participant <- mns
      ptms = participantFeatureTracker.getInputFeatures(participant, parent)
    } yield RoleWithFeatures(role, participant, parent, ptms)
    rwfs.toSeq
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
