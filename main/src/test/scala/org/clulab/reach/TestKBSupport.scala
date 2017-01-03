package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests to ensure grounding is working properly
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Redo key transform and strip suffix tests.
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
  def firstKT (text:String): KeyCandidates = Seq(text.head.toString)
  def lastKT (text:String): KeyCandidates = Seq(text.last.toString)
  def revKT (text:String): KeyCandidates = Seq(text.reverse)

  def kt_id = Seq(identityKT _)
  def kt_first = Seq(firstKT _)
  def kt_last = Seq(lastKT _)
  def kt_rev = Seq(revKT _)
  def kt_all = Seq(identityKT _, firstKT _, lastKT _, revKT _)

  "applyAllTransforms(XXX, identityKT)" should "return identical strings" in {
    (applyAllTransforms("", kt_id).head) should equal ("")
    (applyAllTransforms("I", kt_id).head) should equal ("I")
    (applyAllTransforms("identical", kt_id).head) should equal ("identical")
    (applyAllTransforms("IDENTICAL", kt_id).head) should equal ("IDENTICAL")
    (applyAllTransforms("ID-ENTICAL", kt_id).head) should equal ("ID-ENTICAL")
    (applyAllTransforms("IDENT/ICAL", kt_id).head) should equal ("IDENT/ICAL")
    (applyAllTransforms("ID-ENT/ICAL", kt_id).head) should equal ("ID-ENT/ICAL")
    (applyAllTransforms("Abc/DEF/xyz", kt_id).head) should equal ("Abc/DEF/xyz")
  }

  "applyAllTransforms(XXX, various KTs)" should "do the right things separately" in {
    (applyAllTransforms("abc-xyz", kt_first).head) should equal ("a")
    (applyAllTransforms("ABC-XYZ", kt_first).head) should equal ("A")
    (applyAllTransforms("abc-xyz", kt_last).head) should equal ("z")
    (applyAllTransforms("ABC-XYZ", kt_last).head) should equal ("Z")
    (applyAllTransforms("abcxyz", kt_rev).head) should equal ("zyxcba")
    (applyAllTransforms("ABCXYZ", kt_rev).head) should equal ("ZYXCBA")
  }

  "applyAllTransforms(XXX, multiple KTs)" should "do the right things" in {
    (applyAllTransforms("abcxyz", kt_all).size) should be (4)
    (applyAllTransforms("ABCXYZ", kt_all).size) should be (4)
    (applyAllTransforms(" X ", kt_all).size) should be (4)
    (applyAllTransforms("abcxyz", kt_all)) should equal (Seq("abcxyz", "a", "z", "zyxcba"))
    (applyAllTransforms("ABCXYZ", kt_all)) should equal (Seq("ABCXYZ", "A", "Z", "ZYXCBA"))
    (applyAllTransforms(" X ", kt_all)) should equal (Seq(" X ", " ", " ", " X "))
  }

  // Test real Reach transforms
  def kt_hyphenPK = Seq(hyphenatedProteinKey _)
  def kt_stripPPA = Seq(stripProteinPostAttributives _)
  def kt_stripMP  = Seq(stripMutantProtein _)
  def kt_stripPTMP = Seq(stripPTMPrefixes _)
  def kt_stripGNA = Seq(stripGeneNameAffixes _)
  def kt_stripFPA = Seq(stripFamilyPostAttributives _)
  def kt_stripOPA = Seq(stripOrganPostAttributives _)

  "applyAllTransforms(various strings, hyphenatedProteinKey)" should "return stems" in {
    (applyAllTransforms("LHS-RHS", kt_hyphenPK)) should equal (Seq("RHS"))
    (applyAllTransforms("lhs-aai", kt_hyphenPK)) should equal (Seq("lhs"))
    (applyAllTransforms("AKT1-AAI", kt_hyphenPK)) should equal (Seq("AKT1"))
    (applyAllTransforms("Akt1-Aai", kt_hyphenPK)) should equal (Seq("Akt1"))
    (applyAllTransforms("AAI-AKT1", kt_hyphenPK)) should equal (Seq("AKT1"))
  }

  "applyAllTransforms(various strings, stripProteinPostAttributives)" should "return stems" in {
    (applyAllTransforms("hairy protein", kt_stripPPA)) should equal (Seq("hairy"))
    (applyAllTransforms("HAIRY protein", kt_stripPPA)) should equal (Seq("hairy"))
    (applyAllTransforms("odd mutant protein", kt_stripPPA)) should equal (Seq("odd"))
    (applyAllTransforms("Odd Mutant protein", kt_stripPPA)) should equal (Seq("odd"))
  }

  "applyAllTransforms(various strings, stripOrganPostAttributives)" should "return stems" in {
    (applyAllTransforms("red cell", kt_stripOPA)) should equal (Seq("red"))
    (applyAllTransforms("red cells", kt_stripOPA)) should equal (Seq("red"))
    (applyAllTransforms("blue tissue", kt_stripOPA)) should equal (Seq("blue"))
    (applyAllTransforms("blue tissues", kt_stripOPA)) should equal (Seq("blue"))
    (applyAllTransforms("green fluid", kt_stripOPA)) should equal (Seq("green"))
    (applyAllTransforms("green fluids", kt_stripOPA)) should equal (Seq("green"))
    (applyAllTransforms("purple cell tissue", kt_stripOPA)) should equal (Seq("purple"))
    (applyAllTransforms("purple cell tissues", kt_stripOPA)) should equal (Seq("purple"))
    (applyAllTransforms("purple cell fluid", kt_stripOPA)) should equal (Seq("purple"))
    (applyAllTransforms("purple cell fluids", kt_stripOPA)) should equal (Seq("purple"))
    (applyAllTransforms("purple cell tissue fluid", kt_stripOPA)) should equal (Seq("purple"))
    (applyAllTransforms("purple cell tissue fluids", kt_stripOPA)) should equal (Seq("purple"))
    (applyAllTransforms("purple cells tissues fluids", kt_stripOPA)) should equal (Seq("purple"))
  }

  "applyAllTransforms(various strings, stripMutantProtein)" should "return stems" in {
    (applyAllTransforms("crazy weird mutant", kt_stripMP)) should equal (Seq("crazy"))
    (applyAllTransforms("Crazy Weird Mutant", kt_stripMP)) should equal (Seq("Crazy"))
    (applyAllTransforms("crazy API mutant", kt_stripMP)) should equal (Seq("crazy"))
    (applyAllTransforms("phosphorylated WILD XK mutant", kt_stripMP)) should equal (Seq("WILD"))
    (applyAllTransforms("Phosphorylated WILD XK mutant", kt_stripMP)) should equal (Seq("WILD"))
  }

  "applyAllTransforms(various strings, stripFamilyPostAttributives)" should "return stems" in {
    (applyAllTransforms("parsnip family", kt_stripFPA)) should equal (Seq("parsnip"))
    (applyAllTransforms("Parsnip Family", kt_stripFPA)) should equal (Seq("parsnip"))
    (applyAllTransforms("sad protein family", kt_stripFPA)) should equal (Seq("sad"))
    (applyAllTransforms("SAD protein family", kt_stripFPA)) should equal (Seq("sad"))
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
  val seq1 = Seq(" one")
  val seq2 = Seq(" one", " two")

  "stripAllSuffixes(seq0, string one)" should "return string one" in {
    (stripAllSuffixes(seq0, "string one")) should equal (Some("string one"))
  }

  "stripAllSuffixes(seq0, a string/one-two)" should "return a string/one-two" in {
    (stripAllSuffixes(seq0, "a string/one-two")) should equal (Some("a string/one-two"))
  }

  "stripAllSuffixes(seq1, string one)" should "return string" in {
    (stripAllSuffixes(seq1, "string one")) should equal (Some("string"))
  }

  "stripAllSuffixes(seq2, string two)" should "return string" in {
    (stripAllSuffixes(seq2, "string two")) should equal (Some("string"))
  }

  "stripAllSuffixes(seq2, string one two one two)" should "return string" in {
    (stripAllSuffixes(seq2, "string one two one two")) should equal (Some("string"))
  }

  "stripAllSuffixes(seq2, string one one one)" should "return string" in {
    (stripAllSuffixes(seq2, "string one one one")) should equal (Some("string"))
  }

  // test KBUtils
  "makePathInKBDir(testFile)" should "return complete filepath in KB directory" in {
    val expected = ReachKBConstants.KBDirResourcePath + java.io.File.separator + "testFile"
    val path = ReachKBUtils.makePathInKBDir("testFile")
  }

}
