package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import edu.arizona.sista.reach.grounding2._
import edu.arizona.sista.reach.grounding2.LocalKBKeyTransforms._

/**
  * Unit tests to ensure grounding is working properly
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Update for key transforms class rename.
  */
class TestGrounding2 extends FlatSpec with Matchers {

  // test KBEntry
  val kbe1 = new KBEntry("Adam", "key1", "XYX", "human")
  val kbe2 = new KBEntry("Eve",  "key1", "YXY", "")
  val kbe3 = new KBEntry("Seth", "key3", "XXX", "human", Some(Set("AAA", "BBB")))
  val kbe5k = new KBEntry("Seth", "key3", "QQQ", "human", None)
  val kbe50 = new KBEntry("Seth", "key3", "XXX", "human", None)
  val kbe51 = new KBEntry("Seth", "key3", "XXX", "human", Some(Set("AAA")))
  val kbe52 = new KBEntry("Seth", "key3", "XXX", "human", Some(Set("BBB", "CCC")))
  val kbe60 = new KBEntry("Able", "key6", "ZZZ", "human", None, None)
  val kbe61 = new KBEntry("Able", "key6", "ZZZ", "human", None, Some("STANDARD1"))

  "KBR(text, key, id)" should "NOT have an associated species when tested" in {
    val kbe0 = new KBEntry("Eve", "key2", "YYY")
    (kbe0.hasSpecies) should be (false)
  }

  "KBR(text, key, id, species)" should "have an associated species when tested" in {
    (kbe1.hasSpecies) should be (true)
  }

  "KBR(text, key, id, species)" should "have primary ID XYX" in {
    (kbe1.hasPrimaryId("XYX")) should be (true)
  }

  "KBR(text, key, id, species)" should "NOT have primary ID YYY" in {
    (kbe1.hasPrimaryId("YYY")) should be (false)
  }

  "KBR(text, key, id, species)" should "NOT have alternate ID XYX" in {
    (kbe1.hasAlternateId("XYX")) should be (false) // no alternates at all
  }

  "KBR(text, key, id, species, alternateId)" should "have alternate IDs AAA and BBB" in {
    (kbe3.hasAlternateId("AAA")) should be (true) // true alternate
    (kbe3.hasAlternateId("BBB")) should be (true) // true alternate
  }

  "KBR(text, key, id, species, alternateId)" should "NOT have primary ID AAA" in {
    (kbe3.hasPrimaryId("AAA")) should be (false) // AAA is alternate, not primary
  }

  "KBR(text, key, id, species, alternateId)" should "NOT have alternate ID XXX" in {
    (kbe3.hasAlternateId("XXX")) should be (false) // XXX is primary, not alternate
  }

  "KBR(text, key, id, species, alternateId)" should "have IDs XXX, AAA and BBB" in {
    (kbe3.hasId("XXX")) should be (true)    // primary
    (kbe3.hasId("AAA")) should be (true)    // alternate
    (kbe3.hasId("BBB")) should be (true)    // alternate
  }

  "KBR(text, key, id, species, alternateId)" should "NOT have ID YYY" in {
    (kbe3.hasId("YYY")) should be (false)
  }

  "kbe1.combine(kbe2)" should "have the text from kbe1, key/id/species from kbe1" in {
    val nkbe = kbe1.combine(kbe2)           // combine with no text overwrite
    (nkbe.text == kbe1.text) should be (true)
    (nkbe.key == kbe1.key) should be (true)
    (nkbe.id == kbe1.id) should be (true)
    (nkbe.species == kbe1.species) should be (true)
  }

  "kbe1.combine(kbe2,true)" should "have the text from kbe2, key/id/species from kbe1" in {
    val nkbe = kbe1.combine(kbe2, true)     // combine with text overwrite
    (nkbe.text == kbe2.text) should be (true)
    (nkbe.key == kbe1.key) should be (true)
    (nkbe.id == kbe1.id) should be (true)
    (nkbe.species == kbe1.species) should be (true)
  }

  "kbe2.combine(kbe1)" should "have the text/key/id from kbe2, species from kbe1" in {
    val nkbe = kbe2.combine(kbe1)
    (nkbe.text == kbe2.text) should be (true)
    (nkbe.key == kbe2.key) should be (true)
    (nkbe.id == kbe2.id) should be (true)
    (nkbe.species == kbe1.species) should be (true)
  }

  "kbe50.combine(kbe50)" should "have alternateIds of None" in {
    val nkbe = kbe50.combine(kbe50)
    (nkbe.alternateIds.isEmpty) should be (true)
  }

  "kbe50.combine(kbe51)" should "have alternateIds of Set(AAA)" in {
    val nkbe = kbe50.combine(kbe51)
    (nkbe.alternateIds.isEmpty) should be (false)
    (nkbe.alternateIds.get) should have size (1)
    (nkbe.alternateIds.get.contains("AAA")) should be (true)
  }

  "kbe51.combine(kbe50)" should "have alternateIds of Set(AAA)" in {
    val nkbe = kbe51.combine(kbe50)
    (nkbe.alternateIds.isEmpty) should be (false)
    (nkbe.alternateIds.get) should have size (1)
    (nkbe.alternateIds.get.contains("AAA")) should be (true)
  }

  "kbe50.combine(kbe52)" should "have alternateIds of Set(BBB, CCC)" in {
    val nkbe = kbe50.combine(kbe52)
    (nkbe.alternateIds.isEmpty) should be (false)
    (nkbe.alternateIds.get) should have size (2)
    (nkbe.alternateIds.get.contains("BBB")) should be (true)
    (nkbe.alternateIds.get.contains("CCC")) should be (true)
  }

  "kbe52.combine(kbe50)" should "have alternateIds of Set(BBB, CCC)" in {
    val nkbe = kbe52.combine(kbe50)
    (nkbe.alternateIds.isEmpty) should be (false)
    (nkbe.alternateIds.get) should have size (2)
    (nkbe.alternateIds.get.contains("BBB")) should be (true)
    (nkbe.alternateIds.get.contains("CCC")) should be (true)
  }

  "kbe51.combine(kbe52)" should "have alternateIds of Set(AAA, BBB, CCC)" in {
    val nkbe = kbe51.combine(kbe52)
    (nkbe.alternateIds.isEmpty) should be (false)
    (nkbe.alternateIds.get) should have size (3)
    (nkbe.alternateIds.get.contains("AAA")) should be (true)
    (nkbe.alternateIds.get.contains("BBB")) should be (true)
    (nkbe.alternateIds.get.contains("CCC")) should be (true)
  }

  "kbe52.combine(kbe51)" should "have alternateIds of Set(AAA, BBB, CCC)" in {
    val nkbe = kbe52.combine(kbe51)
    (nkbe.alternateIds.isEmpty) should be (false)
    (nkbe.alternateIds.get) should have size (3)
    (nkbe.alternateIds.get.contains("AAA")) should be (true)
    (nkbe.alternateIds.get.contains("BBB")) should be (true)
    (nkbe.alternateIds.get.contains("CCC")) should be (true)
  }

  "kbe5k.combine(kbe5k)" should "have alternateIds of None" in {
    val nkbe = kbe5k.combine(kbe5k)
    (nkbe.alternateIds.isEmpty) should be (true)
  }

  "kbe5k.combine(kbe50)" should "have alternateIds of Set(QQQ, XXX)" in {
    val nkbe = kbe5k.combine(kbe50)
    (nkbe.alternateIds.isEmpty) should be (false)
    (nkbe.alternateIds.get) should have size (2)
    (nkbe.alternateIds.get.contains("QQQ")) should be (true)
    (nkbe.alternateIds.get.contains("XXX")) should be (true)
  }

  "kbe50.combine(kbe5k)" should "have alternateIds of Set(QQQ, XXX)" in {
    val nkbe = kbe50.combine(kbe5k)
    (nkbe.alternateIds.isEmpty) should be (false)
    (nkbe.alternateIds.get) should have size (2)
    (nkbe.alternateIds.get.contains("QQQ")) should be (true)
    (nkbe.alternateIds.get.contains("XXX")) should be (true)
  }

  "kbe5k.combine(kbe51)" should "have alternateIds of Set(AAA, QQQ, XXX)" in {
    val nkbe = kbe5k.combine(kbe51)
    (nkbe.alternateIds.isEmpty) should be (false)
    (nkbe.alternateIds.get) should have size (3)
    (nkbe.alternateIds.get.contains("AAA")) should be (true)
    (nkbe.alternateIds.get.contains("QQQ")) should be (true)
    (nkbe.alternateIds.get.contains("XXX")) should be (true)
  }

  "kbe51.combine(kbe5k)" should "have alternateIds of Set(AAA, QQQ, XXX)" in {
    val nkbe = kbe51.combine(kbe5k)
    (nkbe.alternateIds.isEmpty) should be (false)
    (nkbe.alternateIds.get) should have size (3)
    (nkbe.alternateIds.get.contains("AAA")) should be (true)
    (nkbe.alternateIds.get.contains("QQQ")) should be (true)
    (nkbe.alternateIds.get.contains("XXX")) should be (true)
  }

  "kbe60.combine(kbe60)" should "have NO standard name" in {
    val nkbe = kbe60.combine(kbe60)
    (nkbe.standardName.isEmpty) should be (true)
  }

  "kbe60.combine(kbe61)" should "have standard name STANDARD1" in {
    val nkbe = kbe60.combine(kbe61)
    (nkbe.standardName.isEmpty) should be (false)
    (nkbe.standardName.get == "STANDARD1") should be (true)
  }

  "kbe61.combine(kbe60)" should "have standard name STANDARD1" in {
    val nkbe = kbe61.combine(kbe60)
    (nkbe.standardName.isEmpty) should be (false)
    (nkbe.standardName.get == "STANDARD1") should be (true)
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

  // test LocalKBKeyTransforms
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

  "makeAlternateKeys(Parsnip family, proteinKeyTransforms)" should "return Parsnip" in {
    // val xkeys = makeAlternateKeys("Parnsip family", Seq(stripFamilySuffixes _))
    val xkeys = makeAlternateKeys("Parsnip family", proteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "Parsnip") should be (true)
  }

  "makeAlternateKeys(savage API mutant, proteinKeyTransforms)" should "return savage" in {
    // val xkeys = makeAlternateKeys("savage API mutant", Seq(stripMutantProtein _))
    val xkeys = makeAlternateKeys("savage API mutant", proteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "savage") should be (true)
  }

  "makeAlternateKeys(phosphorylated WILD XK mutant, proteinKeyTransforms)" should "return WILD" in {
    // val xkeys = makeAlternateKeys("phosphorylated WILD XK mutant", Seq(stripMutantProtein _))
    val xkeys = makeAlternateKeys("phosphorylated WILD XK mutant", proteinKeyTransforms)
    (xkeys.size == 1) should be (true)
    (xkeys.head == "WILD") should be (true)
  }


  "makeCanonicalKey(identical)" should "return identical string" in {
    (LocalKBKeyTransforms.makeCanonicalKey("identical") == "identical") should be (true)
  }

  "makeCanonicalKey(a non-identical)" should "return a non-identical string" in {
    (LocalKBKeyTransforms.makeCanonicalKey("a non-identical") == "a non-identical") should be (false)
  }

  "makeCanonicalKey(A-B and/or C)" should "return abandorc" in {
    (LocalKBKeyTransforms.makeCanonicalKey("A-B and/or C") == "abandorc") should be (true)
  }

  "makeCanonicalKey(MAN_human)" should "return man" in {
    (LocalKBKeyTransforms.makeCanonicalKey("MAN_human") == "man") should be (true)
  }

  "makeCanonicalKey(WO-MAN_HUMAN)" should "return woman" in {
    (LocalKBKeyTransforms.makeCanonicalKey("WO-MAN_HUMAN") == "woman") should be (true)
  }

  val set0 = Set[String]()
  val set1 = Set("one")
  val set2 = Set("one", "two")
  "stripASuffix(set0, string one)" should "return string one" in {
    (LocalKBKeyTransforms.stripASuffix(set0, "string one") == "string one") should be (true)
  }

  "stripASuffix(set1, stringone)" should "return string" in {
    (LocalKBKeyTransforms.stripASuffix(set1, "stringone") == "string") should be (true)
  }

  "stripASuffix(set2, stringtwo)" should "return string" in {
    (LocalKBKeyTransforms.stripASuffix(set2, "stringtwo") == "string") should be (true)
  }

  // "tsvRowToFields()" should "return a sequence with an empty string" in {
  //   val flds = LocalKBUtils.tsvRowToFields("")
  //   (flds) should have size (1)
  //   (flds == Seq("")) should be (true)
  // }

  // "tsvRowToFields(the SAME string)" should "return a sequence with the SAME string" in {
  //   val flds = LocalKBUtils.tsvRowToFields("the SAME string")
  //   (flds) should have size (1)
  //   (flds == Seq("the SAME string")) should be (true)
  // }

  // "tsvRowToFields(one\ttwo)" should "return a sequence of the two fields" in {
  //   val flds = LocalKBUtils.tsvRowToFields("one\ttwo")
  //   (flds) should have size (2)
  //   (flds == Seq("one", "two")) should be (true)
  // }

  // "tsvRowToFields( one \ttwo )" should "return a sequence of the two fields trimmed" in {
  //   val flds = LocalKBUtils.tsvRowToFields(" one \ttwo ")
  //   (flds) should have size (2)
  //   (flds == Seq("one", "two")) should be (true)
  // }

  // test KBUtils
  "makePathInKBDir(testFile)" should "return complete filepath in KB directory" in {
    val expected = LocalKBConstants.KBDirResourcePath + java.io.File.separator + "testFile"
    val path = LocalKBUtils.makePathInKBDir("testFile")
  }

}
