package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.KBKeyTransforms._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests to ensure grounding is working properly
  *   Written by: Tom Hicks. 1/22/2017.
  *   Last Modified: Add/update KT tests. Update for leading mutations.
  */
class TestKBKeyTransforms extends FlatSpec with Matchers {

  // test canonicalKey
  "canonicalKey(identical)" should "return identical string" in {
    (canonicalKey("identical")) should equal ("identical")
  }

  "canonicalKey(a non-identical)" should "return a non-identical string" in {
    (canonicalKey("a non-identical")) should not equal ("a non-identical")
  }

  "canonicalKey(A-B and/or C)" should "return abandorc" in {
    (canonicalKey("A-B and/or C")) should equal ("abandorc")
  }

  "canonicalKey(MAN_human)" should "return man" in {
    (canonicalKey("MAN_human")) should equal ("man_human")
  }

  "canonicalKey(WO-MAN_HUMAN)" should "return woman" in {
    (canonicalKey("WO-MAN")) should equal ("woman")
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

  // test stripAllKeysSuffixes
  "stripAllKeysSuffixes" should "strip the right suffixes" in {
    (stripAllKeysSuffixes("string_human")) should equal ("string")
    (stripAllKeysSuffixes("String_human")) should equal ("String")
    (stripAllKeysSuffixes("STRING_human")) should equal ("STRING")
    (stripAllKeysSuffixes("string_Human")) should equal ("string")
    (stripAllKeysSuffixes("String_Human")) should equal ("String")
    (stripAllKeysSuffixes("STRING_Human")) should equal ("STRING")
    (stripAllKeysSuffixes("string_HUMAN")) should equal ("string")
    (stripAllKeysSuffixes("String_HUMAN")) should equal ("String")
    (stripAllKeysSuffixes("STRING_HUMAN")) should equal ("STRING")
    (stripAllKeysSuffixes("being human")) should equal ("being human")
    (stripAllKeysSuffixes("Being Human")) should equal ("Being Human")
    (stripAllKeysSuffixes("BEING HUMAN")) should equal ("BEING HUMAN")
  }

  // test toKeyCandidates
  "toKeyCandidates(string)" should "return correct results" in {
    (toKeyCandidates("")) should be (empty)
    (toKeyCandidates("string")) should equal (Seq("string"))
    (toKeyCandidates("STRING")) should equal (Seq("STRING")) // no case change
    (toKeyCandidates("a to z")) should equal (Seq("a to z"))
    (toKeyCandidates("string one/two-tree")) should equal (Seq("string one/two-tree"))
    (toKeyCandidates(" XXX ")) should equal (Seq("XXX")) // trimming
  }

  "toKeyCandidates(sequences[string])" should "return correct results" in {
    (toKeyCandidates(Seq(""))) should be (empty)
    (toKeyCandidates(Seq("string"))) should equal (Seq("string"))
    (toKeyCandidates(Seq("STRING"))) should equal (Seq("STRING")) // no case change
    (toKeyCandidates(Seq("a to z"))) should equal (Seq("a to z"))
    (toKeyCandidates(Seq("string one/two-tree"))) should equal (Seq("string one/two-tree"))
    (toKeyCandidates(Seq(" XXX "))) should equal (Seq("XXX")) // trimming
    (toKeyCandidates(Seq("", "", ""))) should be (empty)
    (toKeyCandidates(Seq("a", "ZZ"))) should equal (Seq("a", "ZZ"))
    (toKeyCandidates(Seq("a", "b", "c"))) should equal (Seq("a", "b", "c"))
    (toKeyCandidates(Seq("a", "", "c"))) should equal (Seq("a", "c"))
    (toKeyCandidates(Seq("", "XXX", ""))) should equal (Seq("XXX"))
  }


  // test apply all transforms (string)
  def firstKT (text:String): KeyCandidates = Seq(text.head.toString)
  def lastKT (text:String): KeyCandidates = Seq(text.last.toString)
  def revKT (text:String): KeyCandidates = Seq(text.reverse)

  def kt_id = Seq(identityKT _)
  def kt_lc = Seq(lowercaseKT _)
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
    (applyAllTransforms(kt_lc, "abcxyz").head) should equal ("abcxyz")
    (applyAllTransforms(kt_lc, "ABCXYZ").head) should equal ("abcxyz")
    (applyAllTransforms(kt_lc, "a/bc-xy z").head) should equal ("a/bc-xy z")
    (applyAllTransforms(kt_lc, "AlBeRt EiNsTeIn").head) should equal ("albert einstein")
  }

  "applyAllTransforms(XXX, multiple KTs)" should "do the right things" in {
    (applyAllTransforms(kt_all, "abcxyz").size) should be (4)
    (applyAllTransforms(kt_all, "ABCXYZ").size) should be (4)
    (applyAllTransforms(kt_all, " X ").size) should be (2)
    (applyAllTransforms(kt_all, "abcxyz")) should equal (Seq("abcxyz", "a", "z", "zyxcba"))
    (applyAllTransforms(kt_all, "ABCXYZ")) should equal (Seq("ABCXYZ", "A", "Z", "ZYXCBA"))
    (applyAllTransforms(kt_all, " X ")) should equal (Seq("X", "X"))
  }


  // Test real Reach key transforms
  def kt_stripFPA = Seq(stripFamilyPostAttributivesKT _)
  def kt_stripGNA = Seq(stripGeneNameAffixesKT _)
  def kt_stripMP  = Seq(stripMutantProteinKT _)
  def kt_stripOPA = Seq(stripOrganPostAttributivesKT _)
  def kt_stripPD = Seq(stripProteinDomainKT _)
  def kt_stripPPA = Seq(stripProteinPostAttributivesKT _)
  def kt_stripPTMP = Seq(stripPTMPrefixesKT _)

  "applyAllTransforms(stripFamilyPostAttributives, various strings)" should "return stems" in {
    (applyAllTransforms(kt_stripFPA, "parsnip family")) should equal (Seq("parsnip"))
    (applyAllTransforms(kt_stripFPA, "Parsnip family")) should equal (Seq("Parsnip"))
    (applyAllTransforms(kt_stripFPA, "PARSNIP family")) should equal (Seq("PARSNIP"))
    (applyAllTransforms(kt_stripFPA, "parsnip FAMILY")) should equal (Seq("parsnip"))
    (applyAllTransforms(kt_stripFPA, "Parsnip Family")) should equal (Seq("Parsnip"))
    (applyAllTransforms(kt_stripFPA, "Parsnip FAMILY")) should equal (Seq("Parsnip"))
    (applyAllTransforms(kt_stripFPA, "sad protein family")) should equal (Seq("sad"))
    (applyAllTransforms(kt_stripFPA, "Sad protein family")) should equal (Seq("Sad"))
    (applyAllTransforms(kt_stripFPA, "SAD protein family")) should equal (Seq("SAD"))
    (applyAllTransforms(kt_stripFPA, "sad protein FAMILY")) should equal (Seq("sad"))
    (applyAllTransforms(kt_stripFPA, "sad PROTEIN family")) should equal (Seq("sad"))
    (applyAllTransforms(kt_stripFPA, "sad PROTEIN Family")) should equal (Seq("sad"))
    (applyAllTransforms(kt_stripFPA, "Sad Protein Family")) should equal (Seq("Sad"))
    (applyAllTransforms(kt_stripFPA, "SAD PROTEIN FAMILY")) should equal (Seq("SAD"))
  }

  "applyAllTransforms(stripFamilyPostAttributives, _family)" should "not return stems" in {
    (applyAllTransforms(kt_stripFPA, "parsnip_family")) should equal (Seq("parsnip_family"))
    (applyAllTransforms(kt_stripFPA, "Parsnip_family")) should equal (Seq("Parsnip_family"))
    (applyAllTransforms(kt_stripFPA, "PARSNIP_family")) should equal (Seq("PARSNIP_family"))
    (applyAllTransforms(kt_stripFPA, "parsnip_Family")) should equal (Seq("parsnip_Family"))
    (applyAllTransforms(kt_stripFPA, "Parsnip_Family")) should equal (Seq("Parsnip_Family"))
    (applyAllTransforms(kt_stripFPA, "PARSNIP_Family")) should equal (Seq("PARSNIP_Family"))
    (applyAllTransforms(kt_stripFPA, "parsnip_FAMILY")) should equal (Seq("parsnip_FAMILY"))
    (applyAllTransforms(kt_stripFPA, "Parsnip_FAMILY")) should equal (Seq("Parsnip_FAMILY"))
    (applyAllTransforms(kt_stripFPA, "PARSNIP_FAMILY")) should equal (Seq("PARSNIP_FAMILY"))
  }

  "applyAllTransforms(stripGeneNameAffixes, various strings)" should "strip suffixes" in {
    (applyAllTransforms(kt_stripGNA, "NoSuffix")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "BadSuffix-e")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "BadSuffix-gf")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "BadSuffix e")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "BadSuffix gf")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "dashend-")) should equal (Seq("dashend"))
    (applyAllTransforms(kt_stripGNA, "DashEnd-")) should equal (Seq("DashEnd"))
    (applyAllTransforms(kt_stripGNA, "stem-egfp")) should equal (Seq("stem"))
    (applyAllTransforms(kt_stripGNA, "Stem-eGfp")) should equal (Seq("Stem"))
    (applyAllTransforms(kt_stripGNA, "STEM-eGFP")) should equal (Seq("STEM"))
    (applyAllTransforms(kt_stripGNA, "stem egfp")) should equal (Seq("stem"))
    (applyAllTransforms(kt_stripGNA, "Stem eGfp")) should equal (Seq("Stem"))
    (applyAllTransforms(kt_stripGNA, "STEM eGFP")) should equal (Seq("STEM"))
    (applyAllTransforms(kt_stripGNA, "stem-gfp")) should equal (Seq("stem"))
    (applyAllTransforms(kt_stripGNA, "Stem-Gfp")) should equal (Seq("Stem"))
    (applyAllTransforms(kt_stripGNA, "STEM-GFP")) should equal (Seq("STEM"))
    (applyAllTransforms(kt_stripGNA, "stem gfp")) should equal (Seq("stem"))
    (applyAllTransforms(kt_stripGNA, "Stem Gfp")) should equal (Seq("Stem"))
    (applyAllTransforms(kt_stripGNA, "STEM GFP")) should equal (Seq("STEM"))
  }

  "applyAllTransforms(stripGeneNameAffixes, various strings)" should "strip prefixes" in {
    (applyAllTransforms(kt_stripGNA, "noprefix")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "NoPrefix")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "No Prefix")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "pre-BadPrefix")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "fla-BadPrefix")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "pre BadPrefix")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "fla BadPrefix")) should be (empty)
    (applyAllTransforms(kt_stripGNA, "egfp-stem")) should equal (Seq("stem"))
    (applyAllTransforms(kt_stripGNA, "eGfp-Stem")) should equal (Seq("Stem"))
    (applyAllTransforms(kt_stripGNA, "eGFP-STEM")) should equal (Seq("STEM"))
    (applyAllTransforms(kt_stripGNA, "gfp-stem")) should equal (Seq("stem"))
    (applyAllTransforms(kt_stripGNA, "Gfp-Stem")) should equal (Seq("Stem"))
    (applyAllTransforms(kt_stripGNA, "GFP-STEM")) should equal (Seq("STEM"))
  }

  "applyAllTransforms(stripGeneNameAffixes, various strings)" should "strip prefixes too" in {
    (applyAllTransforms(kt_stripGNA, "egfp-KRAS")) should equal (Seq("KRAS"))
    (applyAllTransforms(kt_stripGNA, "eGfp-KRAS")) should equal (Seq("KRAS"))
    (applyAllTransforms(kt_stripGNA, "EGFP-KRas")) should equal (Seq("KRas"))
    (applyAllTransforms(kt_stripGNA, "gfp-IL2")) should equal (Seq("IL2"))
    (applyAllTransforms(kt_stripGNA, "gfp-IL-2")) should equal (Seq("IL-2"))
    (applyAllTransforms(kt_stripGNA, "gfp-IL 2")) should equal (Seq("IL 2"))
    (applyAllTransforms(kt_stripGNA, "gfp-IL-2-RA")) should equal (Seq("IL-2-RA"))
    (applyAllTransforms(kt_stripGNA, "gfp-IL7")) should equal (Seq("IL7"))
    (applyAllTransforms(kt_stripGNA, "gfp-IL-7")) should equal (Seq("IL-7"))
    (applyAllTransforms(kt_stripGNA, "gfp-IL-7R")) should equal (Seq("IL-7R"))
    (applyAllTransforms(kt_stripGNA, "gfp-IL 7")) should equal (Seq("IL 7"))
    (applyAllTransforms(kt_stripGNA, "gfp-IL-7R-alpha")) should equal (Seq("IL-7R-alpha"))
  }

  "applyAllTransforms(stripGeneNameAffixes, various strings)" should "strip multiple prefixes" in {
    (applyAllTransforms(kt_stripGNA, "Lenti-MYR-Rh-Luc-adv")) should be (empty) // all prefixes
    (applyAllTransforms(kt_stripGNA, "Myr-Rh-Luc-adv-GFP")) should be (empty) // all affixes
    (applyAllTransforms(kt_stripGNA, "myr-flag-akt1")) should equal (Seq("akt1"))
    (applyAllTransforms(kt_stripGNA, "Myr-Flag-Akt1")) should equal (Seq("Akt1"))
    (applyAllTransforms(kt_stripGNA, "MYR-FLAG-AKT1")) should equal (Seq("AKT1"))
    (applyAllTransforms(kt_stripGNA, "activated-myr-flag-akt1")) should equal (Seq("akt1"))
    (applyAllTransforms(kt_stripGNA, "Activated-Myr-Flag-Akt1")) should equal (Seq("Akt1"))
    (applyAllTransforms(kt_stripGNA, "ACTIVATED-MYR-FLAG-AKT1")) should equal (Seq("AKT1"))
    (applyAllTransforms(kt_stripGNA, "p-MYR-HA-Flag-Akt1")) should equal (Seq("Akt1"))
  }

  "applyAllTransforms(stripMutantProtein, various strings)" should "return stems" in {
    (applyAllTransforms(kt_stripMP, "crazy weird mutant")) should equal (Seq("crazy"))
    (applyAllTransforms(kt_stripMP, "Crazy Weird Mutant")) should equal (Seq("Crazy"))
    (applyAllTransforms(kt_stripMP, "crazy API mutant")) should equal (Seq("crazy"))
    (applyAllTransforms(kt_stripMP, "phosphorylated WILD XK mutant")) should equal (Seq("WILD"))
    (applyAllTransforms(kt_stripMP, "Phosphorylated WILD XK mutant")) should equal (Seq("WILD"))
    (applyAllTransforms(kt_stripMP, "mutant-odd")) should equal (Seq("odd"))
    (applyAllTransforms(kt_stripMP, "mutant odd")) should equal (Seq("odd"))
    (applyAllTransforms(kt_stripMP, "Mutant-Odd")) should equal (Seq("Odd"))
    (applyAllTransforms(kt_stripMP, "Mutant Odd")) should equal (Seq("Odd"))
    (applyAllTransforms(kt_stripMP, "MUTANT-ODD")) should equal (Seq("ODD"))
    (applyAllTransforms(kt_stripMP, "MUTANT ODD")) should equal (Seq("ODD"))
    (applyAllTransforms(kt_stripMP, "mutant-zyx-1")) should equal (Seq("zyx-1"))
    (applyAllTransforms(kt_stripMP, "mutant zyx 1")) should equal (Seq("zyx 1"))
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

  "applyAllTransforms(stripOrganPostAttributives, various strings)" should "work un-cased" in {
    (applyAllTransforms(kt_stripOPA, "RED CELL")) should equal (Seq("RED"))
    (applyAllTransforms(kt_stripOPA, "red CELLS")) should equal (Seq("red"))
    (applyAllTransforms(kt_stripOPA, "Blue Tissue")) should equal (Seq("Blue"))
    (applyAllTransforms(kt_stripOPA, "blue Tissues")) should equal (Seq("blue"))
    (applyAllTransforms(kt_stripOPA, "GrEeN fluid")) should equal (Seq("GrEeN"))
    (applyAllTransforms(kt_stripOPA, "green FlUiDs")) should equal (Seq("green"))
    (applyAllTransforms(kt_stripOPA, "PURPLE cell TISSUE")) should equal (Seq("PURPLE"))
    (applyAllTransforms(kt_stripOPA, "purple CELL TISSUES")) should equal (Seq("purple"))
    (applyAllTransforms(kt_stripOPA, "Purple Cell Fluid")) should equal (Seq("Purple"))
    (applyAllTransforms(kt_stripOPA, "purple cellS tissueS fluidS")) should equal (Seq("purple"))
  }

  "applyAllTransforms(stripProteinDomainKey, various strings)" should "return stems" in {
    (applyAllTransforms(kt_stripPD, "LHS-RHS")) should be (empty) // not a PD
    (applyAllTransforms(kt_stripPD, "AAI-AKT1")) should be (empty) // PD on LHS
    (applyAllTransforms(kt_stripPD, "lhs-aai")) should equal (Seq("lhs"))
    (applyAllTransforms(kt_stripPD, "akt1-aai")) should equal (Seq("akt1"))
    (applyAllTransforms(kt_stripPD, "Akt1-Aai")) should equal (Seq("Akt1"))
    (applyAllTransforms(kt_stripPD, "AKT1-AAI")) should equal (Seq("AKT1"))
  }

  "applyAllTransforms(stripProteinPostAttributives, various strings)" should "return stems" in {
    (applyAllTransforms(kt_stripPPA, "hairy protein")) should equal (Seq("hairy"))
    (applyAllTransforms(kt_stripPPA, "Hairy protein")) should equal (Seq("Hairy"))
    (applyAllTransforms(kt_stripPPA, "HAIRY protein")) should equal (Seq("HAIRY"))
    (applyAllTransforms(kt_stripPPA, "hairy protein")) should equal (Seq("hairy"))
    (applyAllTransforms(kt_stripPPA, "Hairy Protein")) should equal (Seq("Hairy"))
    (applyAllTransforms(kt_stripPPA, "HAIRY PROTEIN")) should equal (Seq("HAIRY"))
    (applyAllTransforms(kt_stripPPA, "hairy Protein")) should equal (Seq("hairy"))
    (applyAllTransforms(kt_stripPPA, "hairy PROTEIN")) should equal (Seq("hairy"))
    (applyAllTransforms(kt_stripPPA, "odd mutant protein")) should equal (Seq("odd"))
    (applyAllTransforms(kt_stripPPA, "Odd mutant protein")) should equal (Seq("Odd"))
    (applyAllTransforms(kt_stripPPA, "ODD mutant protein")) should equal (Seq("ODD"))
    (applyAllTransforms(kt_stripPPA, "odd Mutant Protein")) should equal (Seq("odd"))
    (applyAllTransforms(kt_stripPPA, "Odd MUTANT Protein")) should equal (Seq("Odd"))
    (applyAllTransforms(kt_stripPPA, "ODD mutant Protein")) should equal (Seq("ODD"))
    (applyAllTransforms(kt_stripPPA, "ODD mutant PROTEIN")) should equal (Seq("ODD"))
  }

  // TODO PTMP tests
}
