package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests to ensure grounding is working properly
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Update for apply all transforms argument reversal.
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
    (applyAllTransforms(kt_id, "").head) should equal ("")
    (applyAllTransforms(kt_id, "I").head) should equal ("I")
    (applyAllTransforms(kt_id, "identical").head) should equal ("identical")
    (applyAllTransforms(kt_id, "IDENTICAL").head) should equal ("IDENTICAL")
    (applyAllTransforms(kt_id, "ID-ENTICAL").head) should equal ("ID-ENTICAL")
    (applyAllTransforms(kt_id, "IDENT/ICAL").head) should equal ("IDENT/ICAL")
    (applyAllTransforms(kt_id, "ID-ENT/ICAL").head) should equal ("ID-ENT/ICAL")
    (applyAllTransforms(kt_id, "Abc/DEF/xyz").head) should equal ("Abc/DEF/xyz")
  }

  "applyAllTransforms(XXX, various KTs)" should "do the right things separately" in {
    (applyAllTransforms(kt_first, "abc-xyz").head) should equal ("a")
    (applyAllTransforms(kt_first, "ABC-XYZ").head) should equal ("A")
    (applyAllTransforms(kt_last, "abc-xyz").head) should equal ("z")
    (applyAllTransforms(kt_last, "ABC-XYZ").head) should equal ("Z")
    (applyAllTransforms(kt_rev, "abcxyz").head) should equal ("zyxcba")
    (applyAllTransforms(kt_rev, "ABCXYZ").head) should equal ("ZYXCBA")
  }

  "applyAllTransforms(XXX, multiple KTs)" should "do the right things" in {
    (applyAllTransforms(kt_all, "abcxyz").size) should be (4)
    (applyAllTransforms(kt_all, "ABCXYZ").size) should be (4)
    (applyAllTransforms(kt_all, " X ").size) should be (4)
    (applyAllTransforms(kt_all, "abcxyz")) should equal (Seq("abcxyz", "a", "z", "zyxcba"))
    (applyAllTransforms(kt_all, "ABCXYZ")) should equal (Seq("ABCXYZ", "A", "Z", "ZYXCBA"))
    (applyAllTransforms(kt_all, " X ")) should equal (Seq(" X ", " ", " ", " X "))
  }

  // Test real Reach transforms
  def kt_hyphenPK = Seq(hyphenatedProteinKey _)
  def kt_stripPPA = Seq(stripProteinPostAttributives _)
  def kt_stripMP  = Seq(stripMutantProtein _)
  def kt_stripPTMP = Seq(stripPTMPrefixes _)
  def kt_stripGNA = Seq(stripGeneNameAffixes _)
  def kt_stripFPA = Seq(stripFamilyPostAttributives _)
  def kt_stripOPA = Seq(stripOrganPostAttributives _)

  "applyAllTransforms(hyphenatedProteinKey, various strings)" should "return stems" in {
    (applyAllTransforms(kt_hyphenPK, "LHS-RHS")) should equal (Seq("RHS"))
    (applyAllTransforms(kt_hyphenPK, "lhs-aai")) should equal (Seq("lhs"))
    (applyAllTransforms(kt_hyphenPK, "AKT1-AAI")) should equal (Seq("AKT1"))
    (applyAllTransforms(kt_hyphenPK, "Akt1-Aai")) should equal (Seq("Akt1"))
    (applyAllTransforms(kt_hyphenPK, "AAI-AKT1")) should equal (Seq("AKT1"))
  }

  "applyAllTransforms(stripProteinPostAttributives, various strings)" should "return stems" in {
    (applyAllTransforms(kt_stripPPA, "hairy protein")) should equal (Seq("hairy"))
    (applyAllTransforms(kt_stripPPA, "HAIRY protein")) should equal (Seq("hairy"))
    (applyAllTransforms(kt_stripPPA, "odd mutant protein")) should equal (Seq("odd"))
    (applyAllTransforms(kt_stripPPA, "Odd Mutant protein")) should equal (Seq("odd"))
  }

  "applyAllTransforms(stripOrganPostAttributives, various strings)" should "return stems" in {
    (applyAllTransforms(kt_stripOPA, "red cell")) should equal (Seq("red"))
    (applyAllTransforms(kt_stripOPA, "red cells")) should equal (Seq("red"))
    (applyAllTransforms(kt_stripOPA, "blue tissue")) should equal (Seq("blue"))
    (applyAllTransforms(kt_stripOPA, "blue tissues")) should equal (Seq("blue"))
    (applyAllTransforms(kt_stripOPA, "green fluid")) should equal (Seq("green"))
    (applyAllTransforms(kt_stripOPA, "green fluids")) should equal (Seq("green"))
    (applyAllTransforms(kt_stripOPA, "purple cell tissue")) should equal (Seq("purple"))
    (applyAllTransforms(kt_stripOPA, "purple cell tissues")) should equal (Seq("purple"))
    (applyAllTransforms(kt_stripOPA, "purple cell fluid")) should equal (Seq("purple"))
    (applyAllTransforms(kt_stripOPA, "purple cell fluids")) should equal (Seq("purple"))
    (applyAllTransforms(kt_stripOPA, "purple cell tissue fluid")) should equal (Seq("purple"))
    (applyAllTransforms(kt_stripOPA, "purple cell tissue fluids")) should equal (Seq("purple"))
    (applyAllTransforms(kt_stripOPA, "purple cells tissues fluids")) should equal (Seq("purple"))
  }

  "applyAllTransforms(stripMutantProtein, various strings)" should "return stems" in {
    (applyAllTransforms(kt_stripMP, "crazy weird mutant")) should equal (Seq("crazy"))
    (applyAllTransforms(kt_stripMP, "Crazy Weird Mutant")) should equal (Seq("Crazy"))
    (applyAllTransforms(kt_stripMP, "crazy API mutant")) should equal (Seq("crazy"))
    (applyAllTransforms(kt_stripMP, "phosphorylated WILD XK mutant")) should equal (Seq("WILD"))
    (applyAllTransforms(kt_stripMP, "Phosphorylated WILD XK mutant")) should equal (Seq("WILD"))
  }

  "applyAllTransforms(stripFamilyPostAttributives, various strings)" should "return stems" in {
    (applyAllTransforms(kt_stripFPA, "parsnip family")) should equal (Seq("parsnip"))
    (applyAllTransforms(kt_stripFPA, "Parsnip Family")) should equal (Seq("parsnip"))
    (applyAllTransforms(kt_stripFPA, "sad protein family")) should equal (Seq("sad"))
    (applyAllTransforms(kt_stripFPA, "SAD protein family")) should equal (Seq("sad"))
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
