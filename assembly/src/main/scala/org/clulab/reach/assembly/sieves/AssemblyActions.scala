package org.clulab.reach.assembly.sieves

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.taxonomy


// TODO: how to handle state queries for CrossSentenceMentions?

class AssemblyActions extends Actions with LazyLogging {

  import AssemblyActions._

  def identityAction(mentions: Seq[Mention], state: State): Seq[Mention] = mentions

  def bindingFilter(mentions: Seq[Mention], state: State): Seq[Mention] = for {
    m <- validatePrecedenceRelations(mentions, state)
    b <- m.arguments(BEFORE)
    a <- m.arguments(AFTER)
    // binding must involve more than one distinct participant
    if Constraints.hasMultipleInputs(b)
    if Constraints.hasMultipleInputs(a)
  } yield mkPrecedenceMention(parent = m, before = b, after = a)

  def validatePrecedenceRelations(mentions: Seq[Mention], state: State): Seq[Mention] = {

//    if (mentions.nonEmpty && mentions.head.foundBy == "cross-sentence-next-step") {
//      logger.info(s"${mentions.size} cross-sentence-next-step candidates found")
//    }
    val validCandidates = for {
      m <- mentions
      if m.arguments contains BEFORE
      if m.arguments contains AFTER
      b <- m.arguments(BEFORE)
      a <- m.arguments(AFTER)
      // neither reaction should be negated
      if ! Constraints.isNegated(b)
      if ! Constraints.isNegated(a)
      // "a" should not be strictly equivalent to "b"
      if ! Constraints.areEquivalent(b, a, ignoreMods = false)
      if Constraints.shareEntityGrounding(b, a)
      if Constraints.isValidRelationPair(b, a)
    } yield mkPrecedenceMention(parent = m, before = b, after = a)

//    if (mentions.nonEmpty && mentions.head.foundBy == "cross-sentence-next-step") {
//      logger.info(s"${validCandidates.size} cross-sentence-next-step valid candidates remaining (${validCandidates.distinct.size} distinct)")
//      logger.info(s"\n${validCandidates.map(AssemblyActions.summarizeBeforeAfter).mkString("\n")}")
//    }

    validCandidates.distinct
  }

  def shareControlleds(mentions: Seq[Mention], state: State): Seq[Mention] = for {
    m <- validatePrecedenceRelations(mentions, state)
    b <- m.arguments(BEFORE)
    a <- m.arguments(AFTER)
    if Constraints.shareControlleds(b, a, ignoreMods = true)
    if ! Constraints.areEquivalent(b, a, ignoreMods = true)
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
      logger.debug(s"expandArgs found ${expanded.size} candidates\n ${expanded.map(summarizeBeforeAfter).mkString("\n")}")
    }

    validatePrecedenceRelations(expanded, state)
  }

  def afterArgResolvesToBefore(mentions: Seq[Mention], state: State): Seq[Mention] = {

    def resolvesToBefore(before: Mention, afterArgs: Seq[Mention]): Boolean = {
      val am = AssemblyManager(afterArgs :+ before)
      // val afterArgsSet: Set[Mention] = afterArgs.map(AssemblyManager.getResolvedForm).toSet
      val isTrue: Boolean = afterArgs.map(am.getEER).map(_.equivalenceHash(ignoreMods = false)).toSet contains am.getEER(before).equivalenceHash(ignoreMods = false)
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

  def examineIntercedingMarkers(mentions: Seq[Mention], state: State): Seq[Mention] = {

    val precedenceRelations: Seq[Option[Mention]] = for {
      m <- mentions
      if m matches UNKNOWN
      e1 <- m.arguments("E1")
      e2 <- m.arguments("E2")
      // ignore equivalent mentions
      if ! Constraints.areEquivalent(e1, e2, ignoreMods = true)
      // retrieve precedence markers
      e1e2Markers = getMentionsInRange(e1.sentence, e2.sentence, state, Some(E1PrecedesE2Marker))
      e2e1Markers = getMentionsInRange(e1.sentence, e2.sentence, state, Some(E2PrecedesE1Marker))
//      _ = logger.info(s"Found ${e1e2Markers.size} E1PrecedesE2Markers in sentences ${e1.sentence} and ${e2.sentence}")
//      _ = logger.info(s"Found ${e2e1Markers.size} interceding E1PrecedesE2Markers in sentences ${e1.sentence} and ${e2.sentence}")
      // count how many markers fall between e1 and e2
      // FIXME: may want to consider markers that are contained by e1 and e2
      e1e2Mcount = e1e2Markers.count(marker => e1.precedes(marker) && !e2.precedes(marker))
      e2e1Mcount = e1e2Markers.count(marker => e2.precedes(marker) && !e1.precedes(marker))
//      _ = logger.info(s"Found $e1e2Mcount interceding E1PrecedesE2Markers")
//      _ = logger.info(s"Found $e2e1Mcount interceding E1PrecedesE2Markers")
      // check counts
    } yield (e1e2Mcount > 0, e2e1Mcount > 0) match {
      // only found E1PrecedesE2Markers
      case (true, false) =>
        Some(mkPrecedenceMention(before = e1, after = e2, m.foundBy))
      // only found E2PrecedesE1Markers
      case (false, true) =>
        Some(mkPrecedenceMention(before = e2, after = e1, m.foundBy))
      case _ => None
    }
    precedenceRelations.flatten
  }

  /** check triggers for */
//  def retrieveBindingsByTriggers(mentions: Seq[Mention], state: State): Seq[Mention] = for {
//    m <- mentions
//    if m matches UNKNOWN
//    b <- m.arguments("E1").flatMap(e1Trigger => state.mentionsFor(e1Trigger.sentence, e1Trigger.tokenInterval, "Binding"))
//    // tag should be past tense (ex. bound) or a noun (ex. complex)
//    if SieveUtils.findTrigger(b).tags.get.exists(t => t == "VBD" || t == "NN")
//    a <- m.arguments("E2").flatMap(e1Trigger => state.mentionsFor(e1Trigger.sentence, e1Trigger.tokenInterval, "Binding"))
//    // tag should be present tense (ex. binds)
//    if SieveUtils.findTrigger(a).tags.get.contains("VBZ")
//    binding <- validatePrecedenceRelations(Seq(mkPrecedenceMention(b, a, m.foundBy)), state)
//  } yield binding

  /** Inspect any textual overlap of "before" and "after" */
  def checkOverlap(mentions: Seq[Mention], state: State): Seq[Mention] = for {
    m <- mentions
    if m matches PRECEDENCE
    if m.arguments contains BEFORE
    if m.arguments contains AFTER
    b <- m.arguments(BEFORE)
    a <- m.arguments(AFTER)
    // if the spans intersect, apply a more aggressive validation check
    if (! sameSentence(b, a)) || (! invalidSpanOverlap(b, a))
    // general validity check
    valid <- validatePrecedenceRelations(Seq(m), state)
  } yield valid

  def inspectBindingPair(mentions: Seq[Mention], state: State): Seq[Mention] = for {
    m <- mentions
    if m matches PRECEDENCE
    if m.arguments contains BEFORE
    if m.arguments contains AFTER
    b <- m.arguments(BEFORE)
    a <- m.arguments(AFTER)
    // check if span overlap is plausible
    if ! invalidSpanOverlap(b, a)
    if ! crossesPhraseBoundary(b) && ! crossesPhraseBoundary(a)
    // "before" event should not be immediately preceded by "and" or "," etc.
    if notBlacklisted(b.sentenceObj.words, b.start - 1, Seq("and", ",", "whether"))
    // "before" event should not be immediately followed by "and"
    if notBlacklisted(b.sentenceObj.words, b.end + 1, Seq("and"))
    // "after" event should not be immediately preceded by "and" or "," etc.
    if notBlacklisted(a.sentenceObj.words, a.start - 1, Seq(",", "Since"))
    // general validity check
    valid <- validatePrecedenceRelations(Seq(m), state)
  } yield valid

  def validateCoordinatedEvents(mentions: Seq[Mention], state: State): Seq[Mention] = {
    val validSubStrings = Set(" which ", " that ", " and of ")
    val candidates = for {
      m <- mentions
      if m matches PRECEDENCE
      if m.arguments contains BEFORE
      if m.arguments contains AFTER
      b <- m.arguments(BEFORE)
      a <- m.arguments(AFTER)
      if a.sentence == b.sentence
      span = a.sentenceObj.words.slice(Seq(a.start, b.start).min, Seq(a.end, b.end).max).mkString(" ")
      if validSubStrings.exists(s => span contains s)
    } yield m
    checkOverlap(candidates.distinct, state)
  }
}


object AssemblyActions extends LazyLogging {

  val UNKNOWN = "Unknown"
  val BEFORE = SieveUtils.beforeRole
  val AFTER = SieveUtils.afterRole
  val PRECEDENCE = SieveUtils.precedenceMentionLabel
  val precedenceMentionLabels = taxonomy.hypernymsFor(PRECEDENCE)
  val E1PrecedesE2Marker = "E1PrecedesE2Marker"
  val E2PrecedesE1Marker = "E2PrecedesE1Marker"

  def summarizeBeforeAfter(mention: Mention): String = {
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
            |s1:\t'${Seq(before, after).minBy(_.sentence).sentenceObj.getSentenceText}'
            |s2:\t'${Seq(before, after).maxBy(_.sentence).sentenceObj.getSentenceText}'
            |before (${before.label}):\t"${before.text}"
            |after  (${after.label}):\t"${after.text}"
     """.stripMargin
    }
  }

  /** Create a precedence mention for a before and after pair */
  def mkPrecedenceMention(before: Mention, after: Mention, foundBy: String): Mention = before.sentence == after.sentence match {
    case true =>
      new CrossSentenceMention(
        labels = precedenceMentionLabels,
        anchor = before,
        neighbor = after,
        arguments = Map(BEFORE -> Seq(before), AFTER -> Seq(after)),
        document = before.document,
        keep = true,
        foundBy
      )
    case false =>
      new RelationMention(
        labels = precedenceMentionLabels,
        arguments = Map(BEFORE -> Seq(before), AFTER -> Seq(after)),
        sentence = before.sentence,
        document = before.document,
        keep = true,
        foundBy
      )
  }
  def mkPrecedenceMention(parent: Mention, before: Mention, after: Mention): Mention = before.sentence == after.sentence match {
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

  def getMentionsInRange(start: Int, end: Int, state: State, label: Option[String] = None): Seq[Mention] = {

    val candidates = for {
      i <- start to end
      m <- state.mentionsFor(i)
    } yield m

    label match {
      case Some(lbl) => candidates.filter(_ matches lbl)
      case _ => candidates
    }
  }

  /** Checks if mention crosses obvious phrase boundary */
  def crossesPhraseBoundary(m: Mention): Boolean = m match {
    case coord if coord.text contains ", and" => true
    case _ => false
  }

  /** check if position is a blacklisted word */
  def isBlacklisted(words: Seq[String], idx: Int, blacklisted: Seq[String]): Boolean =
    words.isDefinedAt(idx) && blacklisted.contains(words(idx))
  def notBlacklisted(words: Seq[String], idx: Int, blacklisted: Seq[String]): Boolean = ! isBlacklisted(words, idx, blacklisted)

  /** Checks if text spans of mentions overlap in a manner that is unlikely in a case of causal precedence */
  def invalidSpanOverlap(before: Mention, after: Mention): Boolean = (before, after) match {
    // assume cross sentence cases are plausible
    case differentSentences if ! sameSentence(before, after) => false
    // if the spans do not overlap, assume the pair is plausible
    case noIntersection if ! overlappingMentions(before, after) => false
    // inspect intrasentential pairs that overlap
    case (a,b) =>
      // after should not be contained by before and the two should not start on the same token
      (b.tokenInterval.contains(a.tokenInterval) && b.start == a.start) ||
      // "after" should not end with "before"
      // FIXME: this doesn't seem to be working according to the false positives
      (a.tokenInterval.contains(b.tokenInterval) && b.end == a.end)
  }

  def overlappingMentions(m1: Mention, m2: Mention): Boolean = (m1, m2) match {
    case differentSentences if ! sameSentence(m1, m2) => false
    case other => m1.tokenInterval overlaps m2.tokenInterval
  }

  def sameSentence(m1: Mention, m2: Mention): Boolean = m1.sentence == m2.sentence
}