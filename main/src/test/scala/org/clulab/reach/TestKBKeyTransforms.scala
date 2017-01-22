package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests to ensure grounding is working properly
  *   Written by: Tom Hicks. 1/22/2017.
  *   Last Modified: Initial creation.
  */
class TestKBKeyTransforms extends FlatSpec with Matchers {

  // test makeCanonicalKey
  "makeCanonicalKey(identical)" should "return identical string" in {
    (makeCanonicalKey("identical")) should equal ("identical")
  }

  "makeCanonicalKey(a non-identical)" should "return a non-identical string" in {
    (makeCanonicalKey("a non-identical")) should not equal ("a non-identical")
  }

  "makeCanonicalKey(A-B and/or C)" should "return abandorc" in {
    (makeCanonicalKey("A-B and/or C")) should equal ("abandorc")
  }

  "makeCanonicalKey(MAN_human)" should "return man" in {
    (makeCanonicalKey("MAN_human")) should equal ("man")
  }

  "makeCanonicalKey(WO-MAN_HUMAN)" should "return woman" in {
    (makeCanonicalKey("WO-MAN_HUMAN")) should equal ("woman")
  }

  // test stripAllSuffixes
  val seq0 = Seq[String]()
  val seq1 = Seq(" one")
  val seq2 = Seq(" one", " two")

  "stripAllSuffixes(seq0, string one)" should "return string one" in {
    (stripAllSuffixes(seq0, "string one")) should equal ("string one")
  }

  "stripAllSuffixes(seq0, a string/one-two)" should "return a string/one-two" in {
    (stripAllSuffixes(seq0, "a string/one-two")) should equal ("a string/one-two")
  }

  "stripAllSuffixes(seq1, string one)" should "return string" in {
    (stripAllSuffixes(seq1, "string one")) should equal ("string")
  }

  "stripAllSuffixes(seq2, string two)" should "return string" in {
    (stripAllSuffixes(seq2, "string two")) should equal ("string")
  }

  "stripAllSuffixes(seq2, string one two one two)" should "return string" in {
    (stripAllSuffixes(seq2, "string one two one two")) should equal ("string")
  }

  "stripAllSuffixes(seq2, string one one one)" should "return string" in {
    (stripAllSuffixes(seq2, "string one one one")) should equal ("string")
  }

  // test toKeyCandidates
  "toKeyCandidates(string)" should "return correct results" in {
    (toKeyCandidates("")) should be (empty)
    (toKeyCandidates("string")) should equal (Seq("string"))
    (toKeyCandidates("STRING")) should equal (Seq("STRING")) // no case change
    (toKeyCandidates("a to z")) should equal (Seq("a to z"))
    (toKeyCandidates("string one/two-tree")) should equal (Seq("string one/two-tree"))
    (toKeyCandidates(" XXX ")) should equal (Seq(" XXX ")) // no trimming
  }


  // test apply all transforms (string)
  def firstKT (text:String): KeyCandidates = Seq(text.head.toString)
  def lastKT (text:String): KeyCandidates = Seq(text.last.toString)
  def revKT (text:String): KeyCandidates = Seq(text.reverse)

  def kt_id = Seq(identityKT _)
  def kt_first = Seq(firstKT _)
  def kt_last = Seq(lastKT _)
  def kt_rev = Seq(revKT _)
  def kt_all = Seq(identityKT _, firstKT _, lastKT _, revKT _)

  "applyAllTransforms(XXX, identityKT)" should "return identical strings" in {
    (applyAllTransforms(kt_id, "")) should be (empty)
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

}
