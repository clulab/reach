package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

/**
  * Unit tests to ensure Transcription rules are matching correctly
  * User: enoriega
  * Date: 6/01/15
  */

class TestTranscriptionEvents extends FlatSpec with Matchers {

  val sent1 = "expression of NRF2 by Kras"
  sent1 should "contain a transcription event" in {
    val mentions = getBioMentions(sent1)
    hasEventWithArguments("Positive_regulation", List("Kras"), mentions) should be (true)
    hasEventWithArguments("Transcription", List("NRF2"), mentions) should be (true)
  }

  val sent2 = "ErbB3 gene transcription"
  sent2 should "contain a transcription event" in {
    val mentions = getBioMentions(sent2)
    hasEventWithArguments("Transcription", List("ErbB3"), mentions) should be (true)
  }

  val sent3 = "Transcription of Kras"
  sent3 should "contain a transcription event" in {
    val mentions = getBioMentions(sent3)
    hasEventWithArguments("Transcription", List("Kras"), mentions) should be (true)
  }

  val sent4 = "PTEN protein expression was detectable by Western blot in all cell lines."
  sent4 should "contain a transcription event" in {
    val mentions = getBioMentions(sent4)
    hasEventWithArguments("Transcription", List("PTEN"), mentions) should be (true)
  }

  val sent6 = "Indeed, EGFR is overexpressed by Mek in 30%-85% patients with CRC."
  sent6 should "contain a transcription event" in {
    val mentions = getBioMentions(sent6)
    hasEventWithArguments("Positive_regulation", List("Mek"), mentions) should be (true)
    hasEventWithArguments("Transcription", List("EGFR"), mentions) should be (true)
  }

  val sent7 = "We went on to examine the levels of MCL-1 and BIM expressed in several uveal melanoma cell lines"
  sent7 should "contain two transcription events" in {
    val mentions = getBioMentions(sent7)
    hasEventWithArguments("Transcription", List("BIM"), mentions) should be (true)
    hasEventWithArguments("Transcription", List("MCL-1"), mentions) should be (true)
  }

  val sent8 = "Ets-1 upregulates MMP-9 expression"
  sent8 should "contain a Regulation of a Transcription, not an activation" in {
    val mentions = getBioMentions(sent8)
    hasEventWithArguments("Transcription", List("MMP-9"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ets-1", "Transcription", Seq("MMP-9"), mentions) should be (true)
    mentions filter (_ matches "ActivationEvent") should have size (0)
  }

  val sent9 = "SRF induces TAZ transcription"
  sent8 should "contain a Regulation of a Transcription with SRF as the cause" in {
    val mentions = getBioMentions(sent8)
    hasEventWithArguments("Transcription", List("TAZ"), mentions) should be (true)
    hasPositiveRegulationByEntity("SRF", "Transcription", Seq("TAZ"), mentions) should be (true)
  }
}
