package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests to ensure grounding is working properly
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Split out KB key transform tests.
  */
class TestKBSupport extends FlatSpec with Matchers {

  // test KBEntry
  val kbe0 = new KBEntry("Eve", "ns", "YYY")
  val kbe1 = new KBEntry("Eve", "ns", "YXY", "")
  val kbe2 = new KBEntry("Adam", "ns", "XYX", "human")

  val kbe30 = new KBEntry("Chan", "ns", "idid", "human")
  val kbe31 = new KBEntry("QQQQ", "ns", "idid", "human")
  val kbe32 = new KBEntry("CHAN", "ns", "idid", "human")
  val kbe33 = new KBEntry("Chan", "QQ", "idid", "human")
  val kbe34 = new KBEntry("Chan", "ns", "QQQQ", "human")
  val kbe35 = new KBEntry("Chan", "ns", "idid", "QQQQQ")
  val kbe37 = new KBEntry("Chan", "QQ", "QQQQ", "QQQQQ")

  val kbe60 = new KBEntry("Able", "tst", "ZZZ", "mouse")
  val kbe61 = new KBEntry("able", "tst", "ZZZ", "mouse")

  "KBEntry(text, ns, id)" should "NOT have an associated species when tested" in {
    (kbe0.hasNoSpecies) should be (true)
    (kbe1.hasNoSpecies) should be (true)
    (kbe2.hasNoSpecies) should be (false)
    (kbe30.hasNoSpecies) should be (false)
    (kbe35.hasNoSpecies) should be (false)
    (kbe60.hasNoSpecies) should be (false)
  }

  "KBEntry(text, ns, id, species)" should "have an associated species when tested" in {
    (kbe2.hasSpecies) should be (true)
    (kbe30.hasSpecies) should be (true)
    (kbe35.hasSpecies) should be (true)
    (kbe60.hasSpecies) should be (true)
    (kbe0.hasSpecies) should be (false)
    (kbe1.hasSpecies) should be (false)
  }

  "KBEntry" should "should equal another KBEntry when tested" in {
    (kbe30.equals(kbe30)) should be (true)
    (kbe30.equals(kbe32)) should be (true)
    (kbe60.equals(kbe61)) should be (true)
  }

  "KBEntry" should "should NOT equal another KBEntry when tested" in {
    (kbe30.equals(kbe31)) should be (false)
    (kbe30.equals(kbe33)) should be (false)  // different namespace
    (kbe30.equals(kbe34)) should be (false)  // different ID
    (kbe30.equals(kbe35)) should be (false)  // different species
    (kbe30.equals(kbe37)) should be (false)  // same text only
    (kbe30.equals(kbe61)) should be (false)  // different record
  }


  // test Speciated
  "isHumanSpecies(human)" should "work" in {
    (Speciated.isHumanSpecies("human")) should be (true)
    (Speciated.isHumanSpecies("Human")) should be (true)
    (Speciated.isHumanSpecies("HUMAN")) should be (true)
    (Speciated.isHumanSpecies("Homo Sapiens")) should be (true)
    (Speciated.isHumanSpecies("HOMO SAPIENS")) should be (true)
  }

  "isHumanSpecies()" should "fail" in {
    (Speciated.isHumanSpecies("")) should be (false)
    (Speciated.isHumanSpecies("rana pipiens")) should be (false)
    (Speciated.isHumanSpecies("homo erectus")) should be (false)
    (Speciated.isHumanSpecies("Human being")) should be (false)
  }

  "containsHumanSpecies" should "should work" in {
    (Speciated.containsHumanSpecies(Seq("human"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("homo sapiens"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("human","homo sapiens"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "human", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("human", "bat", "ant", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "ant", "human"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "homo sapiens", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("homo sapiens", "bat", "ant", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "ant", "homo sapiens"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("Human"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("Homo Sapiens"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("HUMAN","HOMO SAPIENS"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "Human", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("HUMAN", "bat", "ant", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "ant", "Human"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "HOMO SAPIENS", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("Homo Sapiens", "bat", "ant", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "ant", "Homo sapiens"))) should be (true)
  }

  "containsHumanSpecies" should "should fail" in {
    (Speciated.containsHumanSpecies(Seq[String]())) should be (false)
    (Speciated.containsHumanSpecies(Seq("BAT"))) should be (false)
    (Speciated.containsHumanSpecies(Seq("bat"))) should be (false)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "ant"))) should be (false)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "homo erectus"))) should be (false)
    (Speciated.containsHumanSpecies(Seq("human being"))) should be (false)
    (Speciated.containsHumanSpecies(Seq("Human Being"))) should be (false)
  }


  // test KBUtils
  "makePathInKBDir(testFile)" should "return complete filepath in KB directory" in {
    val expected = ReachKBConstants.KBDirResourcePath + java.io.File.separator + "testFile"
    val path = ReachKBUtils.makePathInKBDir("testFile")
  }

}
