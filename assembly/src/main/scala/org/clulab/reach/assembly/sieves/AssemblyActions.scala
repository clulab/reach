package org.clulab.reach.assembly.sieves

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.taxonomy


// TODO: how to handle state queries for CrossSentenceMentions?

class AssemblyActions extends Actions with LazyLogging {

  val BEFORE = SieveUtils.beforeRole
  val AFTER = SieveUtils.afterRole
  val PRECEDENCE = SieveUtils.precedenceMentionLabel
  val precedenceMentionLabels = taxonomy.hypernymsFor(PRECEDENCE)

  def identityAction(mentions: Seq[Mention], state: State): Seq[Mention] = mentions

  /** Create a precedence mention for a before and after pair */
  private def mkPrecedenceMention(parent: Mention, before: Mention, after: Mention): Mention = before.sentence == after.sentence match {
    case true =>
      new CrossSentenceMention(
        labels = precedenceMentionLabels,
        anchor = before,
        neighbor = after,
        arguments = Map(BEFORE -> Seq(before), AFTER -> Seq(after)),
        document = parent.document,
        keep = parent.keep,
        foundBy = parent.foundBy
      )
    case false =>
      new RelationMention(
        labels = precedenceMentionLabels,
        arguments = Map(BEFORE -> Seq(before), AFTER -> Seq(after)),
        sentence = parent.sentence,
        document = parent.document,
        keep = parent.keep,
        foundBy = parent.foundBy
      )
  }

  def showBeforeAfter(mention: Mention): String = {
    val before = mention.arguments(SieveUtils.beforeRole).head
    val after = mention.arguments(SieveUtils.afterRole).head
    before.sentence == after.sentence match {
      case true =>
        s"""found-by:\t${mention.foundBy}
            |s1:\t'${before.sentenceObj.getSentenceText}'
            |before (${before.label}):\t"${before.text}"
            |after  (${after.label}):\t"${after.text}"
     """.stripMargin

      case false =>
        s"""found-by:\t${mention.foundBy}
            |s1:\t'${before.sentenceObj.getSentenceText}'
            |s2:\t'${after.sentenceObj.getSentenceText}'
            |before (${before.label}):\t"${before.text}"
            |after  (${after.label}):\t"${after.text}"
     """.stripMargin
    }
  }

  def validatePrecedenceRelations(mentions: Seq[Mention], state: State): Seq[Mention] = {

    val validCandidates = for {
      m <- mentions
      if m.arguments contains BEFORE
      if m.arguments contains AFTER
      b <- m.arguments(BEFORE)
      a <- m.arguments(AFTER)
      // a should not be equivalent to b
      if !Constraints.areEquivalent(b, a)
      if Constraints.shareArg(b, a)
      if Constraints.isValidRelationPair(b, a)
    } yield mkPrecedenceMention(parent = m, before = b, after = a)

    val distinctCandidates = validCandidates.distinct

    if (distinctCandidates.nonEmpty) {
      logger.debug(s"validatePrecedenceRelations found ${distinctCandidates.size} matches\n ${distinctCandidates.map(showBeforeAfter).mkString("\n")}")
    }
    distinctCandidates
  }

  def validatePrecedenceRelations2(mentions: Seq[Mention], state: State): Seq[Mention] = {

    val validCandidates = for {
      m <- mentions
      if m.arguments contains BEFORE
      if m.arguments contains AFTER
      b <- m.arguments(BEFORE)
      a <- m.arguments(AFTER)
      // a should not be equivalent to b
      if !Constraints.areEquivalent(b, a)
      if Constraints.shareArg(b, a)
      if Constraints.isValidRelationPair(b, a)
    } yield mkPrecedenceMention(parent = m, before = b, after = a)

    if (validCandidates.nonEmpty) {
      logger.debug(s"validatePrecedenceRelations found ${validCandidates.size} matches\n ${validCandidates.map(showBeforeAfter).mkString("\n")}")
    }
    validCandidates
  }

  def shareControlleds(mentions: Seq[Mention], state: State): Seq[Mention] = for {
    m <- validatePrecedenceRelations(mentions, state)
    b <- m.arguments(BEFORE)
    a <- m.arguments(AFTER)
    if Constraints.shareControlleds(b, a)
  } yield mkPrecedenceMention(parent = m, before = b, after = a)

  def expandArgs(mentions: Seq[Mention], state: State): Seq[Mention] = {

    // Find mentions in the state that overlap with the provided mention
    def getOverlappingEvents(m: Mention): Seq[Mention] = state.mentionsFor(m.sentence, m.tokenInterval).filter(_ matches "Event")

    val expanded = for {
      m <- mentions
      if m.arguments contains BEFORE
      if m.arguments contains AFTER
      b <- m.arguments(BEFORE).flatMap(getOverlappingEvents)
      a <- m.arguments(AFTER).flatMap(getOverlappingEvents)
      if a != b
    } yield mkPrecedenceMention(parent = m, before = b, after = a)

    if (expanded.nonEmpty) {
      logger.debug(s"expandArgs found ${expanded.size} candidates\n ${expanded.map(showBeforeAfter).mkString("\n")}")
    }

    validatePrecedenceRelations(expanded, state)
  }

  def afterArgResolvesToBefore(mentions: Seq[Mention], state: State): Seq[Mention] = {

    def resolvesToBefore(before: Mention, afterArgs: Seq[Mention]): Boolean = {
      val am = AssemblyManager(afterArgs :+ before)
      // val afterArgsSet: Set[Mention] = afterArgs.map(AssemblyManager.getResolvedForm).toSet
      val isTrue: Boolean = afterArgs.map(am.getEER).map(_.equivalenceHash).toSet contains am.getEER(before).equivalenceHash
      if (isTrue) {
        logger.info(s"afterArgResolvesToBefore passes for before (${before.label}) with text: '${before.text}'")
      }
      isTrue

    }

    for {
      m <- mentions
      if m.arguments contains BEFORE
      if m.arguments contains AFTER
      b <- m.arguments(BEFORE)
      a <- m.arguments(AFTER)
      if resolvesToBefore(b, a.arguments.values.flatten.toSeq)
    } yield mkPrecedenceMention(parent = m, before = b, after = a)
  }

}
