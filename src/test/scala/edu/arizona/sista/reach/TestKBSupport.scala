package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests to ensure grounding is working properly
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Update for new equality and removal of alternate IDs.
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
  val kbe36 = new KBEntry("Chan", "key3", "ns", "idid", "human", Some("QQQQ"))
  val kbe37 = new KBEntry("Chan", "QQQQ", "QQ", "QQQQ", "QQQQQ")

  val kbe60 = new KBEntry("Able", "key6", "tst", "ZZZ", "mouse")
  val kbe61 = new KBEntry("Able", "key6", "tst", "ZZZ", "mouse", Some("STANDARD1"))

  "KBEntry(text, key, ns, id)" should "NOT have an associated species when tested" in {
    (kbe0.hasNoSpecies) should be (true)
    (kbe1.hasNoSpecies) should be (true)
    (kbe2.hasNoSpecies) should be (false)
    (kbe30.hasNoSpecies) should be (false)
    (kbe35.hasNoSpecies) should be (false)
    (kbe61.hasNoSpecies) should be (false)
  }

  "KBEntry(text, key, ns, id, species)" should "have an associated species when tested" in {
    (kbe2.hasSpecies) should be (true)
    (kbe30.hasSpecies) should be (true)
    (kbe35.hasSpecies) should be (true)
    (kbe61.hasSpecies) should be (true)
    (kbe0.hasSpecies) should be (false)
    (kbe1.hasSpecies) should be (false)
  }

  "KBEntry" should "should equal another KBEntry when tested" in {
    (kbe30.equals(kbe30)) should be (true)
    (kbe30.equals(kbe31)) should be (true)
    (kbe30.equals(kbe36)) should be (true)
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
  "isHumanSpecies()" should "be reported as NOT a human resolution" in {
    (kbe1.isHumanSpecies("")) should be (false)
  }

  "isHumanSpecies(rana pipiens)"should "be reported as NOT a human resolution 2" in {
    (kbe1.isHumanSpecies("rana pipiens")) should be (false)
  }

  "isHumanSpecies(homo erectus)" should "be reported as NOT a human resolution 3" in {
    (kbe1.isHumanSpecies("homo erectus")) should be (false)
  }

  "isHumanSpecies(human)" should "be reported as a human resolution" in {
    (kbe1.isHumanSpecies("human")) should be (true)
  }

  "isHumanSpecies(Homo Sapiens)"should "be reported as a human resolution 2" in {
    (kbe1.isHumanSpecies("Homo Sapiens")) should be (true)
  }

  "isHumanSpecies(HOMO SAPIENS)" should "be reported as a human resolution 3" in {
    (kbe1.isHumanSpecies("HOMO SAPIENS")) should be (true)
  }

  // test ReachKeyTransforms
  "makeAlternateKeys(identical, proteinKeyTransforms)" should "return identical string" in {
    (makeAlternateKeys("identical",proteinKeyTransforms)).isEmpty should be (true)
    (makeAlternateKeys("IDENTICAL",proteinKeyTransforms)).isEmpty should be (true)
    (makeAlternateKeys("no change",proteinKeyTransforms)).isEmpty should be (true)
    (makeAlternateKeys("result: empty list",proteinKeyTransforms)).isEmpty should be (true)
  }

  "makeAlternateKeys(LHS-RHS, proteinKeyTransforms)" should "return RHS" in {
    // val xkeys = makeAlternateKeys("LHS-RHS", Seq(unmutateProteinKey _))
    val xkeys = makeAlternateKeys("LHS-RHS", proteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "RHS") should be (true)
  }

  "makeAlternateKeys(hairy protein, proteinKeyTransforms)" should "return hairy" in {
    // val xkeys = makeAlternateKeys("hairy protein", Seq(stripProteinSuffixes _))
    val xkeys = makeAlternateKeys("hairy protein", proteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "hairy") should be (true)
  }

  "makeAlternateKeys(savage API mutant, proteinKeyTransforms)" should "return savage" in {
    // val xkeys = makeAlternateKeys("savage API mutant", Seq(stripMutantProtein _))
    val xkeys = makeAlternateKeys("savage API mutant", proteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "savage") should be (true)
  }

  "makeAlternateKeys(weird protein mutant, proteinKeyTransforms)" should "return weird" in {
    // val xkeys = makeAlternateKeys("weird protein mutant", Seq(stripProteinSuffixes _))
    val xkeys = makeAlternateKeys("weird protein mutant", proteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "weird") should be (true)
  }

  "makeAlternateKeys(odd mutant protein, proteinKeyTransforms)" should "return odd" in {
    // val xkeys = makeAlternateKeys("odd mutant protein", Seq(stripProteinSuffixes _))
    val xkeys = makeAlternateKeys("odd mutant protein", proteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "odd") should be (true)
  }

  "makeAlternateKeys(phosphorylated WILD XK mutant, proteinKeyTransforms)" should "return WILD" in {
    // val xkeys = makeAlternateKeys("phosphorylated WILD XK mutant", Seq(stripMutantProtein _))
    val xkeys = makeAlternateKeys("phosphorylated WILD XK mutant", proteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "WILD") should be (true)
  }

  "makeAlternateKeys(Parsnip family, familyKeyTransforms)" should "return Parsnip" in {
    val xkeys = makeAlternateKeys("Parsnip family", familyKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "Parsnip") should be (true)
  }

  "makeAlternateKeys(sad protein family, familyKeyTransforms)" should "return sad" in {
    val xkeys = makeAlternateKeys("sad protein family", familyKeyTransforms)
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
  "stripSuffixes(seq0, string one)" should "return string one" in {
    (stripSuffixes(seq0, "string one") == "string one") should be (true)
  }

  "stripSuffixes(seq1, stringone)" should "return string" in {
    (stripSuffixes(seq1, "stringone") == "string") should be (true)
  }

  "stripSuffixes(seq2, stringtwo)" should "return string" in {
    (stripSuffixes(seq2, "stringtwo") == "string") should be (true)
  }

  // test KBUtils
  "makePathInKBDir(testFile)" should "return complete filepath in KB directory" in {
    val expected = ReachKBConstants.KBDirResourcePath + java.io.File.separator + "testFile"
    val path = ReachKBUtils.makePathInKBDir("testFile")
  }

}
