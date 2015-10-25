package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.grounding2._

/**
 * Unit tests to ensure grounding is working properly
 *   Written by: Tom Hicks. 10/23/2015.
 *   Last Modified: Update for KBEntry.
 */
class TestGrounding2 extends FlatSpec with Matchers {

  // test KBMetaInfo
  val kbm = new KBMetaInfo("baseURI://", "UAnamespace", "MIR:00000000")

  "KBMetaInfo.referenceURI(007)" should "have a referenceURI of baseURI://007" in {
    (kbm.referenceURI("007") == "baseURI://007") should be (true)
  }

  // test KBEntry
  val kbe = new KBEntry(kbm, "Adam", "key1", "XYX", Some("human"))
  val kbe2 = new KBEntry(kbm, "Seth", "key3", "XXX", Some("human"), Some(Set("AAA", "BBB")))

  "KBR(kbm, text, key, id)" should "NOT have an associated species when tested" in {
    val kbe0 = new KBEntry(kbm, "Eve", "key2", "YYY")
    (kbe0.hasSpecies) should be (false)
  }

  "KBR(kbm, text, key, id, species)" should "have an associated species when tested" in {
    (kbe.hasSpecies) should be (true)
  }

  "KBR(kbm, text, key, id, species)" should "have primary ID XYX" in {
    (kbe.hasPrimaryId("XYX")) should be (true)
  }

  "KBR(kbm, text, key, id, species)" should "NOT have primary ID YYY" in {
    (kbe.hasPrimaryId("YYY")) should be (false)
  }

  "KBR(kbm, text, key, id, species)" should "NOT have alternate ID XYX" in {
    (kbe.hasAlternateId("XYX")) should be (false) // no alternates at all
  }

  "KBR(kbm, text, key, id, species, alternateId)" should "have alternate IDs AAA and BBB" in {
    (kbe2.hasAlternateId("AAA")) should be (true) // true alternate
    (kbe2.hasAlternateId("BBB")) should be (true) // true alternate
  }

  "KBR(kbm, text, key, id, species, alternateId)" should "NOT have primary ID AAA" in {
    (kbe2.hasPrimaryId("AAA")) should be (false) // AAA is alternate, not primary
  }

  "KBR(kbm, text, key, id, species, alternateId)" should "NOT have alternate ID XXX" in {
    (kbe2.hasAlternateId("XXX")) should be (false) // XXX is primary, not alternate
  }

  "KBR(kbm, text, key, id, species, alternateId)" should "have IDs XXX, AAA and BBB" in {
    (kbe2.hasId("XXX")) should be (true)    // primary
    (kbe2.hasId("AAA")) should be (true)    // alternate
    (kbe2.hasId("BBB")) should be (true)    // alternate
  }

  "KBR(kbm, text, key, id, species, alternateId)" should "NOT have ID YYY" in {
    (kbe2.hasId("YYY")) should be (false)
  }


  // test Speciated
  "isHumanSpecies()" should "be reported as NOT a human resolution" in {
    (kbe.isHumanSpecies("")) should be (false)
  }

  "isHumanSpecies(rana pipiens)"should "be reported as NOT a human resolution 2" in {
    (kbe.isHumanSpecies("rana pipiens")) should be (false)
  }

  "isHumanSpecies(homo erectus)" should "be reported as NOT a human resolution 3" in {
    (kbe.isHumanSpecies("homo erectus")) should be (false)
  }

  "isHumanSpecies(human)" should "be reported as a human resolution" in {
    (kbe.isHumanSpecies("human")) should be (true)
  }

  "isHumanSpecies(Homo Sapiens)"should "be reported as a human resolution 2" in {
    (kbe.isHumanSpecies("Homo Sapiens")) should be (true)
  }

  "isHumanSpecies(HOMO SAPIENS)" should "be reported as a human resolution 3" in {
    (kbe.isHumanSpecies("HOMO SAPIENS")) should be (true)
  }

  // test LocalKBUtils
  "makeKBCanonKey(identical)" should "return identical string" in {
    (LocalKBUtils.makeKBCanonKey("identical") == "identical") should be (true)
  }

  "makeKBCanonKey(a non-identical)" should "return a non-identical string" in {
    (LocalKBUtils.makeKBCanonKey("a non-identical") == "a non-identical") should be (false)
  }

  "makeKBCanonKey(A-B and/or C)" should "return abandorc" in {
    (LocalKBUtils.makeKBCanonKey("A-B and/or C") == "abandorc") should be (true)
  }

  "makeKBCanonKey(MAN_human)" should "return man" in {
    (LocalKBUtils.makeKBCanonKey("MAN_human") == "man") should be (true)
  }

  "makeKBCanonKey(WO-MAN_HUMAN)" should "return woman" in {
    (LocalKBUtils.makeKBCanonKey("WO-MAN_HUMAN") == "woman") should be (true)
  }

  val set0 = Set[String]()
  val set1 = Set("one")
  val set2 = Set("one", "two")
  "stripASuffix(set0, string one)" should "return string one" in {
    (LocalKBUtils.stripASuffix(set0, "string one") == "string one") should be (true)
  }

  "stripASuffix(set1, stringone)" should "return string" in {
    (LocalKBUtils.stripASuffix(set1, "stringone") == "string") should be (true)
  }

  "stripASuffix(set2, stringtwo)" should "return string" in {
    (LocalKBUtils.stripASuffix(set2, "stringtwo") == "string") should be (true)
  }

  "tsvRowToFields()" should "return a sequence with an empty string" in {
    val flds = LocalKBUtils.tsvRowToFields("")
    (flds) should have size (1)
    (flds == Seq("")) should be (true)
  }

  "tsvRowToFields(the SAME string)" should "return a sequence with the SAME string" in {
    val flds = LocalKBUtils.tsvRowToFields("the SAME string")
    (flds) should have size (1)
    (flds == Seq("the SAME string")) should be (true)
  }

  "tsvRowToFields(one\ttwo)" should "return a sequence of the two fields" in {
    val flds = LocalKBUtils.tsvRowToFields("one\ttwo")
    (flds) should have size (2)
    (flds == Seq("one", "two")) should be (true)
  }

  "tsvRowToFields( one \ttwo )" should "return a sequence of the two fields trimmed" in {
    val flds = LocalKBUtils.tsvRowToFields(" one \ttwo ")
    (flds) should have size (2)
    (flds == Seq("one", "two")) should be (true)
  }

  "makePathInKBDir(testFile)" should "return complete filepath in KB directory" in {
    val expected = LocalKBConstants.KBDirResourcePath + java.io.File.separator + "testFile"
    val path = LocalKBUtils.makePathInKBDir("testFile")
  }

}
