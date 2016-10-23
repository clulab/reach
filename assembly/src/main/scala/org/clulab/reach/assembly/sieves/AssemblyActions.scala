package org.clulab.reach.assembly.sieves

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.{Actions, Mention, RelationMention, State}
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.taxonomy


// TODO: how to handle state queries for CrossSentenceMentions?

class AssemblyActions extends Actions with LazyLogging {

  val BEFORE = SieveUtils.beforeRole
  val AFTER = SieveUtils.afterRole
  val PRECEDENCE = SieveUtils.precedenceMentionLabel
  val precedenceMentionLabels = taxonomy.hypernymsFor(PRECEDENCE)
  logger.info(s"precedenceMentionLabels: $precedenceMentionLabels")

  def identityAction(mentions: Seq[Mention], state: State): Seq[Mention] = mentions

  private def showBeforeAfter(mention: Mention): String = {
    val before = mention.arguments(SieveUtils.beforeRole).head
    val after = mention.arguments(SieveUtils.afterRole).head
    s"""found-by:\t${mention.foundBy}
       |cross-sentence?:\t${before.sentence != after.sentence}
       |before (${before.label}):\t"${before.text}"
       |after  (${after.label}):\t"${after.text}"
     """.stripMargin
  }

  def validatePrecedenceRelations(mentions: Seq[Mention], state: State): Seq[Mention] = {

    val am = AssemblyManager(mentions)

    val validCandidates = for {
      m <- mentions
      if m.arguments contains BEFORE
      if m.arguments contains AFTER
      b <- m.arguments(BEFORE)
      a <- m.arguments(AFTER)
      // a should not be equivalent to b
      if ! Constraints.areEquivalent(b, a)
      if Constraints.shareArg(b, a)
      if Constraints.isValidRelationPair(b, a)
    } yield new RelationMention(
      labels = precedenceMentionLabels,
      arguments = Map(BEFORE -> Seq(b), AFTER -> Seq(a)),
      sentence = m.sentence,
      document = m.document,
      keep = m.keep,
      foundBy = m.foundBy
    )

    if (validCandidates.nonEmpty) {
      logger.info(s"validatePrecedenceRelations found ${validCandidates.size} matches\n ${validCandidates.map(showBeforeAfter).mkString("\n")}")
    }
    validCandidates
  }

  def shareControlleds(mentions: Seq[Mention], state: State): Seq[Mention] = for {
    m <- validatePrecedenceRelations(mentions, state)
    b <- m.arguments(BEFORE)
    a <- m.arguments(AFTER)
    if Constraints.shareControlleds(b, a)
  } yield m

  def expandArgs(mentions: Seq[Mention], state: State): Seq[Mention] = {

    // Find mentions in the state that overlap with the provided mention
    def getOverlappingEvents(m: Mention): Seq[Mention] = state.mentionsFor(m.sentence, m.tokenInterval).filter(_ matches "Event")

    val expanded = for {
      m <- mentions
      if m.arguments contains BEFORE
      if m.arguments contains AFTER
      b <- m.arguments(BEFORE).flatMap(getOverlappingEvents)
      a <- m.arguments(AFTER).flatMap(getOverlappingEvents)
    } yield new RelationMention(
      labels = precedenceMentionLabels,
      arguments = Map(BEFORE -> Seq(b), AFTER -> Seq(a)),
      sentence = m.sentence,
      document = m.document,
      keep = m.keep,
      foundBy = m.foundBy
    )

    if (expanded.nonEmpty) {
      logger.info(s"expandArgs found ${expanded.size} candidates\n ${expanded.map(showBeforeAfter).mkString("\n")}")
    }

    validatePrecedenceRelations(expanded, state)
  }

}
