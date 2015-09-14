package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

/**
 * Unit tests to ensure Translocation rules are matching correctly
 * User: mihais
 * Date: 5/19/15
 */
class TestTranslocationEvents extends FlatSpec with Matchers {

  /*val sent1 = "We show here that ASPP2 is phosphorylated by the RAS/Raf/MAPK pathway and that this phosphorylation leads to its increased translocation to the cytosol or nucleus and increased binding to p53"
  sent1 should "contain 2 translocation events" in {
    val mentions = parseSentence(sent1)

    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)

    // not sure if this works with the new system...
    hasEventWithArguments("Translocation", List("nucleus"), mentions) should be (true)
    hasEventWithArguments("Translocation", List("cytosol"), mentions) should be (true)

    // TODO: missing two regulations:  phosphorylation leads to transport and binding
    // TODO: missing the binding between ASPP2 and p53 (HARD; ok to miss)
  }*/

  val sent2 = "ASPP2 is transported from the membrane to the nucleus and cytosol"
  sent2 should "contain 2 translocation events for ASPP2" in {
    val mentions = parseSentence(sent2)

    hasEventWithArguments("Translocation", List("ASPP2", "membrane", "cytosol"), mentions) should be (true)
    hasEventWithArguments("Translocation", List("ASPP2", "membrane", "nucleus"), mentions) should be (true)
  }

  // These test rely on COREF
/*  val sent3a = "ASPP1 is common, and its recruitment to the plasma membrane and nuclear membrane increases with its phosphorylation."
  val sent3b = "ASPP1 is common, and its release from the plasma membrane and nuclear membrane increases with its phosphorylation."
  val sent3c = "ASPP1 is common, and its release from the plasma membrane and nuclear membrane to the cytosol increases with its phosphorylation."
  sent3a should "contain two translocation events" in {
    val mentions = parseSentence(sent3a)
    mentions filter (_ matches "Translocation") should have size (2)
    hasEventWithArguments("Translocation", List("ASPP1", "plasma membrane"), mentions) should be (true)
    hasEventWithArguments("Translocation", List("ASPP1", "nuclear membrane"), mentions) should be (true)
  }

  sent3b should "contain two translocation events" in {
    val mentions = parseSentence(sent3b)
    mentions filter (_ matches "Translocation") should have size (2)
    hasEventWithArguments("Translocation", List("ASPP1", "plasma membrane"), mentions) should be (true)
    hasEventWithArguments("Translocation", List("ASPP1", "nuclear membrane"), mentions) should be (true)
  }

  sent3c should "contain two translocation events" in {
    val mentions = parseSentence(sent3c)
    mentions filter (_ matches "Translocation") should have size (2)
    hasEventWithArguments("Translocation", List("ASPP1", "plasma membrane", "cytosol"), mentions) should be (true)
    hasEventWithArguments("Translocation", List("ASPP1", "nuclear membrane", "cytosol"), mentions) should be (true)
  }*/


  "testTranslocation1" should "find 1 translocation event" in {
    val mentions = parseSentence("Phosphorylation leads the plasma membrane to release p53 to the cytosol.")
    hasEventWithArguments("Translocation", List("p53", "plasma membrane", "cytosol"), mentions) should be (true)
  }

  "testTranslocation2" should "find 1 translocation event" in {
    val mentions = parseSentence("Recruitment of p53 from the cytosol to the plasma membrane increases with phosphorylation.")
    hasEventWithArguments("Translocation", List("p53", "plasma membrane", "cytosol"), mentions) should be (true)
  }

  "testTranslocation3" should "find 1 translocation event" in {
    val mentions = parseSentence("With increased phosphorylation, p53 is exported from the plasma membrane to the cytosol.")
    hasEventWithArguments("Translocation", List("p53", "plasma membrane", "cytosol"), mentions) should be (true)
  }

  "testTranslocation4" should "find 1 translocation event" in {
    val mentions = parseSentence("ASPP2, a protein which is translocated from the membrane to the nucleus, is subsequently phosphorylated.")
    mentions.count(_ matches "Translocation") should be (1)
    mentions.count(_ matches "Phosphorylation") should be (1)
    hasEventWithArguments("Translocation", List("ASPP2", "membrane", "nucleus"), mentions) should be (true)
  }

  "testTranslocation5" should "find 1 translocation event" in {
    val mentions = parseSentence("ASPP2, a protein which translocates Pde2 from the membrane to the nucleus, is subsequently phosphorylated.")
    mentions.count(_ matches "Translocation") should be (1)
    mentions.count(_ matches "Phosphorylation") should be (1)
    hasEventWithArguments("Translocation", List("Pde2", "membrane", "nucleus"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP2", "Translocation", Seq("Pde", "membrane", "nucleus"), mentions) should be (true)
  }

  "testTranslocation6" should "find 2 translocation events" in {
    val mentions = parseSentence("KRAS translocation to the cytosol and nucleus")
    mentions.filter(_ matches "Translocation") should have size (2)
    hasEventWithArguments("Translocation", List("KRAS", "cytosol"), mentions) should be (true)
    hasEventWithArguments("Translocation", List("KRAS", "nucleus"), mentions) should be (true)
  }

  "testTranslocation7" should "find 1 translocation with a regulation" in {
    val mentions = parseSentence("ASPP2, a protein which is translocated from the membrane to the nucleus by ASPP1, is subsequently phosphorylated")
    mentions.filter(_.label == "Translocation") should have size (1)
    hasEventWithArguments("Translocation", List("ASPP2", "membrane", "nucleus"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Translocation", Seq("ASPP2", "membrane", "nucleus"), mentions) should be (true)
  }

}
