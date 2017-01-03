package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests to ensure grounding is working properly
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Update for refactoring of key transforms.
  */
class TestKBSupport extends FlatSpec with Matchers {

  // test KBEntry
  val kbe0 = new KBEntry("Eve", "key0", "ns", "YYY")
  val kbe1 = new KBEntry("Eve", "key1", "ns", "YXY", "")
  val kbe2 = new KBEntry("Adam", "key1", "ns", "XYX", "human")

  val kbe30 = new KBEntry("Chan", "key3", "ns", "idid", "human")
  val kbe31 = new KBEntry("QQQQ", "key3", "ns", "idid", "human")
  val kbe32 = new KBEntry("Chan", "QQQQ", "ns", "idid", "human")
  val kbe33 = new KBEntry("Chan", "key3", "QQ", "idid", "human")
  val kbe34 = new KBEntry("Chan", "key3", "ns", "QQQQ", "human")
  val kbe35 = new KBEntry("Chan", "key3", "ns", "idid", "QQQQQ")
  val kbe37 = new KBEntry("Chan", "QQQQ", "QQ", "QQQQ", "QQQQQ")

  val kbe60 = new KBEntry("Able", "key6", "tst", "ZZZ", "mouse")
  val kbe61 = new KBEntry("able", "key6", "tst", "ZZZ", "mouse")

  "KBEntry(text, key, ns, id)" should "NOT have an associated species when tested" in {
    (kbe0.hasNoSpecies) should be (true)
    (kbe1.hasNoSpecies) should be (true)
    (kbe2.hasNoSpecies) should be (false)
    (kbe30.hasNoSpecies) should be (false)
    (kbe35.hasNoSpecies) should be (false)
    (kbe60.hasNoSpecies) should be (false)
  }

  "KBEntry(text, key, ns, id, species)" should "have an associated species when tested" in {
    (kbe2.hasSpecies) should be (true)
    (kbe30.hasSpecies) should be (true)
    (kbe35.hasSpecies) should be (true)
    (kbe60.hasSpecies) should be (true)
    (kbe0.hasSpecies) should be (false)
    (kbe1.hasSpecies) should be (false)
  }

  "KBEntry" should "should equal another KBEntry when tested" in {
    (kbe30.equals(kbe30)) should be (true)
    (kbe30.equals(kbe31)) should be (true)
    (kbe60.equals(kbe61)) should be (true)
  }

  "KBEntry" should "should NOT equal another KBEntry when tested" in {
    (kbe30.equals(kbe32)) should be (false)  // different key
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


  // test ReachKeyTransforms
  "applyAllTransforms(identical, ProteinKeyTransforms)" should "return identical string" in {
    (applyAllTransforms("identical",ProteinKeyTransforms)).isEmpty should be (true)
    (applyAllTransforms("IDENTICAL",ProteinKeyTransforms)).isEmpty should be (true)
    (applyAllTransforms("no change",ProteinKeyTransforms)).isEmpty should be (true)
    (applyAllTransforms("result: empty list",ProteinKeyTransforms)).isEmpty should be (true)
  }

  "applyAllTransforms(LHS-RHS, ProteinKeyTransforms)" should "return RHS" in {
    // mostly testing unmutateProteinKey
    val xkeys = applyAllTransforms("LHS-RHS", ProteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "RHS") should be (true)
  }

  "applyAllTransforms(hairy protein, ProteinKeyTransforms)" should "return hairy" in {
    // mostly testing stripProteinSuffixes
    val xkeys = applyAllTransforms("hairy protein", ProteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "hairy") should be (true)
  }

  "applyAllTransforms(savage API mutant, ProteinKeyTransforms)" should "return savage" in {
    // mostly testing stripMutantProtein
    val xkeys = applyAllTransforms("savage API mutant", ProteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "savage") should be (true)
  }

  "applyAllTransforms(weird protein mutant, ProteinKeyTransforms)" should "return weird" in {
    // mostly testing stripProteinSuffixes
    val xkeys = applyAllTransforms("weird protein mutant", ProteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "weird") should be (true)
  }

  "applyAllTransforms(odd mutant protein, ProteinKeyTransforms)" should "return odd" in {
    // mostly testing stripProteinSuffixes
    val xkeys = applyAllTransforms("odd mutant protein", ProteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "odd") should be (true)
  }

  "applyAllTransforms(phosphorylated WILD XK mutant, ProteinKeyTransforms)" should "return WILD" in {
    // mostly testing stripMutantProtein
    val xkeys = applyAllTransforms("phosphorylated WILD XK mutant", ProteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "WILD") should be (true)
  }

  "applyAllTransforms(Parsnip family, FamilyKeyTransforms)" should "return Parsnip" in {
    val xkeys = applyAllTransforms("Parsnip family", FamilyKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "parsnip") should be (true)
  }

  "applyAllTransforms(sad protein family, FamilyKeyTransforms)" should "return sad" in {
    val xkeys = applyAllTransforms("sad protein family", FamilyKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "sad") should be (true)
  }


  "makeCanonicalKey(identical)" should "return identical string" in {
    (makeCanonicalKey("identical") == "identical") should be (true)
  }

  "makeCanonicalKey(a non-identical)" should "return a non-identical string" in {
    (makeCanonicalKey("a non-identical") == "a non-identical") should be (false)
  }

  "makeCanonicalKey(A-B and/or C)" should "return abandorc" in {
    (makeCanonicalKey("A-B and/or C") == "abandorc") should be (true)
  }

  "makeCanonicalKey(MAN_human)" should "return man" in {
    (makeCanonicalKey("MAN_human") == "man") should be (true)
  }

  "makeCanonicalKey(WO-MAN_HUMAN)" should "return woman" in {
    (makeCanonicalKey("WO-MAN_HUMAN") == "woman") should be (true)
  }

  val seq0 = Seq[String]()
  val seq1 = Seq("one")
  val seq2 = Seq("one", "two")
  "stripAllSuffixes(seq0, string one)" should "return string one" in {
    (stripAllSuffixes(seq0, "string one") == "string one") should be (true)
  }

  "stripAllSuffixes(seq1, stringone)" should "return string" in {
    (stripAllSuffixes(seq1, "stringone") == "string") should be (true)
  }

  "stripAllSuffixes(seq2, stringtwo)" should "return string" in {
    (stripAllSuffixes(seq2, "stringtwo") == "string") should be (true)
  }

  // test KBUtils
  "makePathInKBDir(testFile)" should "return complete filepath in KB directory" in {
    val expected = ReachKBConstants.KBDirResourcePath + java.io.File.separator + "testFile"
    val path = ReachKBUtils.makePathInKBDir("testFile")
  }

}
