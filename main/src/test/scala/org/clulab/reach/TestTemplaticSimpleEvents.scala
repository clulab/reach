package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

/**
 * Unit tests to ensure that SimpleEvent rules coming from the templatic grammar are matching correctly
 * Date: 5/19/15
 */

class TestTemplaticSimpleEvents extends FlatSpec with Matchers {
  val sent1 = "The phosphorylation on AKT was great."
  sent1 should "not produce a phosphorylation based on the preposition \"on\"" in {
    val mentions = getBioMentions(sent1)
    val p = mentions.find(_ matches "Phosphorylation")
    p.isDefined should be (false)
  }

  val sent2 = "JAK3 phosphorylates three HuR residues (Y63, Y68, Y200)"
  sent2 should "extract 3 phosphorylations and 3 positive regulations" in {
    val mentions = getBioMentions(sent2)

    val p = mentions.filter(_ matches "Phosphorylation")
    p should have size (3)
    val r = mentions.filter(_ matches "Positive_regulation")
    r should have size (3)
  }

  val sent3 = "The deubiquitination of ASPP2 is promoted by optineurin."
  sent3 should "not contain a ubiquitination event" in {
    val doc = testReach.mkDoc(sent3, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_ matches "Ubiquitination") should be (false)
  }

  val sent4 = "The dephosphorylation of ASPP2 is promotted by optineurin."
  sent4 should "not contain a phosphorylation event" in {
    val doc = testReach.mkDoc(sent4, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_ matches "Phosphorylation") should be (false)
  }

  // This test has been ported from TestDarpaEval2015Training
  val sent5 = "In contrast, the EGFR T669A mutant increased both basal EGFR and ERBB3 tyrosine phosphorylation that was not augmented by MEK inhibition"
  sent5 should "contain two phosphorylations" in {
    val text = sent5
    val doc = testReach.mkDoc(text, "testdoc")
    val phosphorylations = testReach.extractFrom(doc).filter(_ matches "Phosphorylation")
    phosphorylations.size should be (2)
    TestUtils.hasEventWithArguments("Phosphorylation", List("EGFR"), phosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("ERBB3"), phosphorylations) should be (true)
  }

  // This test has been ported from TestDarpaEval2015Training
  val sent6 = "We hypothesized that MEK inhibition activates AKT by inhibiting ERK activity, which blocks an inhibitory threonine phosphorylation on the JM domains of EGFR and HER2, thereby increasing ERBB3 phosphorylation."
  sent6 should "contain 3 phosphorylations" in {
    val text = sent6
    val doc = testReach.mkDoc(text, "testdoc")
    val phosphorylations = testReach.extractFrom(doc).filter(_ matches "Phosphorylation")
    phosphorylations.size should be (3)
    TestUtils.hasEventWithArguments("Phosphorylation", List("EGFR"), phosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("HER2"), phosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("ERBB3"), phosphorylations) should be (true)
  }

  // there is a phosphorylation event in the example text
  val sent7 = "The ubiquitinated Ras protein phosphorylates AKT."
  sent7 should "contain a phosphorylation" in {
    val mentions = testReach.extractFrom(sent7, "testdoc", "1", None)
    val phospho = mentions.find(_ matches "Phosphorylation")
    phospho.isDefined should be (true)
    phospho.get.arguments.contains("theme") should be (true)
    // simple events get a single argument
    phospho.get.arguments("theme").size should be (1)
    // simple events shouldn't have causes
    // because they are promoted to Positive_regulations
    phospho.get.arguments.contains("cause") should be (false)
    phospho.get.arguments("theme").head.text.contains("AKT") should be (true)
  }

  val sent8 = "We next considered the effect of Ras monoubiquitination on GAP-mediated hydrolysis"
  sent8 should "contain a ubiquitination" in {
    val mentions = getBioMentions(sent8)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent9 = "The effects of monoubiquitination on Ras are not isoform-specific."
  sent9 should "contain a ubiquitination" in {
    val mentions = getBioMentions(sent9)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent10 = "We measured the rate of GAP-mediated GTP hydrolysis and observed that the response of Ras ligated to Ubiquitin was identical"
  sent10 should "contain a ubiquitination NOT binding" in {
    val mentions = getBioMentions(sent10)
    // per Ryan/Guang's comment this is Ubiq not Binding
    // we do not do hydrolysis, so we can ignore that
    hasEventWithArguments("Binding", List("Ras", "Ubiquitin"), mentions) should be (false)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  // This fails, as it should: the monoubiquitination is ambiguous. The theme is never stated.
  /*
  val sent12 = "Here we show that monoubiquitination decreases the sensitivity of Ras to GAP-mediated hydrolysis"
  sent12 should "contain a ubiquitination" in {
    val mentions = getBioMentions(sent12)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }
  */

  val sent13 = "Indicating that p38 SAPK is not an efficient kinase for ASPP2 phosphorylation."
  sent13 should "contain a phosphorylation" in {
    val mentions = getBioMentions(sent13)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent14a = "Experiments revealed ubiquitination at Lys residues 104 and 147 of K-Ras"
  sent14a should "contain 2 ubiquitinations" in {
    val mentions = getBioMentions(sent14a)
    mentions.filter(_ matches "Ubiquitination") should have size (2)
  }

  val sent14b = "Experiments revealed ubiquitination at Lys residues 117, 147, and 170 for H-Ras."
  sent14b should "contain 3 ubiquitinations" in {
    val mentions = getBioMentions(sent14b)
    mentions.filter(_ matches "Ubiquitination") should have size (3)
  }

  "testHydrolysisPass1" should "find 1 hydrolysis event" in {
    val mentions = getBioMentions("Ras-GDP is hydrolyzed by 26S proteasome without ubiquitination.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisSubjNom1" should "find 1 hydrolysis event" in {
    val mentions = getBioMentions("MEK hydrolysis of Ras-GDP increased.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisObjNom1" should "find 1 hydrolysis event" in {
    val mentions = getBioMentions("Ras-GDP hydrolysis by MEK increased.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisSubjectRel1" should "find 1 hydrolysis event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via Pde2, which specifically hydrolyzes Ras-GDP.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisSubjectRel2" should "find 1 hydrolysis event" in {
    val mentions = getBioMentions("Pde2, which has been found to hydrolyze Ras-GDP, activates MEK.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisSubjectRelApposition1" should "find 1 hydrolysis event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via overexpressing Pde2, a phosphodiesterase that specifically hydrolyzes Ras-GDP.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisSubjectRelApposition2" should "find 1 hydrolysis event" in {
    val mentions = getBioMentions("A main rate-controlling step in RAS is renin, an enzyme that hydrolyzes Ras-GTP.")
    hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions) should be (true)
  }

  "testHydrolysisObjectRel1" should "find 1 hydrolysis event" in {
    val mentions = getBioMentions("We measured transcription activation in the presence of MEK, which is hydrolyzed by CRP.")
    hasEventWithArguments("Hydrolysis", List("MEK"), mentions) should be (true)
  }

  "testPhosphorylationDecl1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras is phosphorylating ASPP2.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationPass1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 is phosphorylated by Ras.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationSubjNom1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras phosphorylation of ASPP2 increased.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationObjNom1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 phosphorylation by Ras increased.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationSubjectRel1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via Ras, which specifically phosphorylates ASPP2.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationSubjectRel2" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras, which has been found to phosphorylate ASPP2, activates MEK.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationSubjectRelApposition1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically phosphorylates ASPP2.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationSubjectRelApposition2" should "find 1 phosphorylation event" in {
    val mentions = getBioMentions("A main rate-controlling step in AAAA is renin, an enzyme that phosphorylates ASPP2 to generate XXXX")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationObjectRel1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("We measured transcription activation in the presence of ASPP2, which is phosphorylated by Ras.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationDecl1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras is hydroxylating ASPP2.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationPass1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 is hydroxylated by Ras.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationSubjNom1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras hydroxylation of ASPP2 increased.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationObjNom1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 hydroxylation by Ras increased.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationSubjectRel1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via Ras, which specifically hydroxylates ASPP2.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationSubjectRel2" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras, which has been found to hydroxylate ASPP2, activates MEK.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationSubjectRelApposition1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically hydroxylates ASPP2.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationSubjectRelApposition2" should "find 1 hydroxylation event" in {
    val mentions = getBioMentions("A main rate-controlling step in AAAA is renin, an enzyme that hydroxylates ASPP2 to generate XXXX")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationObjectRel1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("We measured transcription activation in the presence of ASPP2, which is hydroxylated by Ras.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationDecl1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("Ras is ubiquitinating ASPP2.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationPass1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 is ubiquitinated by Ras.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationSubjNom1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("Ras ubiquitination of ASPP2 increased.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationObjNom1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 ubiquitination by Ras increased.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationObjNom2" should "find 1 ubiquitination event and 2 regulation events" in {
    val mentions = getBioMentions("RAS ubiquitination and degradation by ASPP2 and p53 increased.")
    hasEventWithArguments("Ubiquitination", List("RAS"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP2", "Ubiquitination", List("RAS"), mentions) should be (true)
    hasPositiveRegulationByEntity("p53", "Ubiquitination", List("RAS"), mentions) should be (true)
  }

  "testUbiquitinationSubjectRel1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via Ras, which specifically ubiquitinates ASPP2.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationSubjectRel2" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("Ras, which has been found to ubiquitinate ASPP2, activates MEK.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationSubjectRelApposition1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically ubiquitinates ASPP2.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationSubjectRelApposition2" should "find 1 ubiquitination event" in {
    val mentions = getBioMentions("A main rate-controlling step in AAAA is renin, an enzyme that ubiquitinates ASPP2 to generate XXXX")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationObjectRel1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("We measured transcription activation in the presence of ASPP2, which is ubiquitinated by Ras.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  val sent15 = "ASPP2 phosphorylates p53 at serine 125 and serine 126."
  sent15 should "contains 2 phosphorylation and 2 regulation events" in {
    val mentions = getBioMentions(sent15)

    val p = mentions.filter(_ matches "Phosphorylation")
    p should have size (2)
    val r = mentions.filter(_ matches "Positive_regulation")
    r should have size (2)

    hasEventWithArguments("Phosphorylation", List("p53"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP2", "Phosphorylation", List("p53"), mentions) should be (true)
  }

  val sent16 = "ASPP2 phosphorylates p53 at serine 125, 126, and 127."
  sent16 should "contain 3 phosphorylation and 3 regulation events" in {
    val mentions = getBioMentions(sent16)

    val p = mentions.filter(_ matches "Phosphorylation")
    p should have size (3)
    val r = mentions.filter(_ matches "Positive_regulation")
    r should have size (3)

    hasEventWithArguments("Phosphorylation", List("p53"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP2", "Phosphorylation", List("p53"), mentions) should be (true)
  }

  val sent17 = "Its many abnormal phenotypes can be rescued via Pde2, which does not hydrolyze Ras-GDP."
  sent17 should "contain a negated regulated hydrolysis" in {
    val mentions = getBioMentions(sent17)
    val h = mentions.filter(_ matches "Hydrolysis")
    h should have size 1
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
    hasPositiveRegulationByEntity("Pde2", "Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  val sent18 = "Ras does not phosphorylate ASPP2."
  sent18 should "contain a negated regulated phosphorylation" in {
    val mentions = getBioMentions(sent18)
    val p = mentions.filter(_ matches "Phosphorylation")
    p should have size 1
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent19 = "Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that does not phosphorylate ASPP2."
  sent19 should "contain a negated regulated phosphorylation" in {
    val mentions = getBioMentions(sent19)
    val h = mentions.filter(_ matches "Phosphorylation")
    h should have size 1
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent20 = "We measured transcription activation in the presence of ASPP2, which is not ubiquitinated by Ras."
  sent20 should "contain a negated regulated ubiquitination" in {
    val mentions = getBioMentions(sent20)
    val u = mentions.filter(_ matches "Ubiquitination")
    u should have size 1
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  val sent21 = "phosphorylation of HuR at Y200 influences the response of immune cells to cytokines"
  sent21 should "contain a single phosphorylation with site" in {
    val mentions = getBioMentions(sent21)
    val p = mentions.filter(_ matches "Phosphorylation")
    p should have size (1)
    hasEventWithArguments("Phosphorylation", List("HuR", "Y200"), mentions) should be (true)
  }

  val sent22 = "CK2 phosphorylation of XRCC1 stimulates binding to either PNK or aprataxin"
  sent22 should "not contain a phosphorylation with CK2 as theme" in {
    val mentions = getBioMentions(sent22)
    hasEventWithArguments("Phosphorylation", List("XRCC1"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("CK2"), mentions) should be (false)
    hasPositiveRegulationByEntity("CK2", "Phosphorylation", List("XRCC1"), mentions) should be (true)
  }

  val sent23 = "Shown in Figure     is a Western blot detecting the phosphorylation of the mTOR substrate, 4EBP1."
  sent23 should "contain a phosphorylation of 4EBP1 not mTOR (GUS)" in {
    val mentions = getBioMentions(sent23)
    hasEventWithArguments("Phosphorylation", List("4EBP1"), mentions) should be (true)
    // the above applies to ALL events taking proteins as arguments, including activations and regulations...
    hasEventWithArguments("Phosphorylation", List("mTOR"), mentions) should be (false)
  }

  val sent24 = "We found that XRCC1 can be phosphorylated on S371 by DNA-PK"
  sent24 should "contain 1 phosphorylation and 1 cause" in {
    val mentions = getBioMentions(sent24)
    hasEventWithArguments("Phosphorylation", List("XRCC1", "S371"), mentions) should be (true)
    hasPositiveRegulationByEntity("DNA-PK", "Phosphorylation", List("XRCC1", "S371"), mentions) should be (true)
  }

  val sent25 = "We found that XRCC1 R399Q can be phosphorylated on S371 by DNA-PK"
  sent25 should "contain 1 phosphorylation on S371 and not R399Q, which is a mutation (GUS)" in {
    val mentions = getBioMentions(sent25)
    hasEventWithArguments("Phosphorylation", List("XRCC1", "S371"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("XRCC1", "R399Q"), mentions) should be (false)
  }

  val sent25b = "We found that R399Q-XRCC1 mutant can be phosphorylated on S371 by DNA-PK"
  sent25b should "contain 1 phosphorylation on S371 and not R399Q, which is a mutation (GUS)" in {
    val mentions = getBioMentions(sent25b)
    hasEventWithArguments("Phosphorylation", List("XRCC1", "S371"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("XRCC1", "R399Q"), mentions) should be (false)
  }

  val sent26 = "The BRCT1 domain of XRCC1 is phosphorylated in vitro by DNA-PK"
  sent26 should "contain 1 phospho with site + 1 reg (GUS)" in {
    val mentions = getBioMentions(sent26)
    hasEventWithArguments("Phosphorylation", List("XRCC1", "BRCT1 domain"), mentions) should be (true)
    hasPositiveRegulationByEntity("DNA-PK", "Phosphorylation", List("XRCC1", "BRCT1 domain"), mentions) should be (true)
  }

  val sent27 = "The study reveals that XRCC1 is phosphorylated by the co-immunoprecipitated DNA-PK."
  sent27 should "contain 1 phospho + 1 reg (GUS)" in {
    val mentions = getBioMentions(sent27)
    hasEventWithArguments("Phosphorylation", List("XRCC1"), mentions) should be (true)
    hasPositiveRegulationByEntity("DNA-PK", "Phosphorylation", List("XRCC1"), mentions) should be (true)
  }

  val sent28 = "all six FGFR3 mutants induced activatory ERK(T202/Y204) phosphorylation (Fig. 2)."
  sent28 should "contain 2 phospho + 2 pos reg (GUS/MARCO)" in {
    val mentions = getBioMentions(sent28)
    hasEventWithArguments("Phosphorylation", List("ERK", "T202"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("ERK", "Y204"), mentions) should be (true)
    hasPositiveRegulationByEntity("FGFR3", "Phosphorylation", List("ERK", "T202"), mentions) should be (true)
    hasPositiveRegulationByEntity("FGFR3", "Phosphorylation", List("ERK", "Y204"), mentions) should be (true)
  }

  val sent29 = "Figure 5 shows that only the K650M and K650E mutants caused significant STAT5(Y694) phosphorylation"
  sent29 should "contain 1 phospho (GUS/MARCO)" in {
    val mentions = getBioMentions(sent29)
    // TODO: mutants caused X...should be this be Reg. of some kind?
    hasEventWithArguments("Phosphorylation", List("STAT5", "Y694"), mentions) should be (true)
  }

  val sent30 = "we found slight STAT1(Y701) phosphorylation induced by wild-type FGFR3."
  sent30 should "contain 1 phospho and 1 pos reg" in {
    val mentions = getBioMentions(sent30)
    hasEventWithArguments("Phosphorylation", List("STAT1", "Y701"), mentions) should be (true)
    hasPositiveRegulationByEntity("FGFR3", "Phosphorylation", List("STAT1", "Y701"), mentions) should be (true)
  }

  val sent31 = "We found that endogenous K-Ras and H-Ras underwent mono-ubiquitination in HEK293T cells."
  sent31 should "contain 2 ubiquitinations" in {
    val mentions = getBioMentions(sent31)
    hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions) should be (true)
    hasEventWithArguments("Ubiquitination", List("H-Ras"), mentions) should be (true)
  }

  val sent32 = "The K650M, K660M, and K650E-FGFR3 mutants are phosphorylated on Y123 and T546"
  sent32 should "contain 6 phosphos" in {
    val mentions = getBioMentions(sent32)
    mentions.filter(_ matches "Phosphorylation") should have size (6)
    hasEventWithArguments("Phosphorylation", List("FGFR3", "Y123"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("FGFR3", "T546"), mentions) should be (true)
  }

  val sent33 = "blot analysis of in vitro p53-acetylation"
  sent33 should "contain 1 entity and 1 acetylation" in {
    val mentions = getBioMentions(sent33)
    mentions.filter(_ matches "Gene_or_gene_product") should have size (1) // blot should not be an entity
    mentions.filter(_ matches "Acetylation") should have size (1)
    hasEventWithArguments("Acetylation", List("p53"), mentions) should be (true)
  }

  val sent34 = "p35 regulation of GSK3betaser9 phosphorylation"
  sent34 should "not contain a phospho of p35 (GUS)" in {
    val mentions = getBioMentions(sent34)
    // TODO: this fails because the *_token_8_noun rule over matches. Please fix
    hasEventWithArguments("Phosphorylation", List("p35"), mentions) should be (false)
  }

  val sent35a = "E3 ubiquitin ligase ubiquitinates beta-catenin."
  sent35a should "contain a ubiquitination with cause" in {
    val mentions = getBioMentions(sent35a)
    hasPositiveRegulationByEntity("E3 ubiquitin ligase", "Ubiquitination", List("beta-catenin"), mentions) should be (true)
  }

  val sent35b = "Beta-catenin ubiquitinates E3 ubiquitin ligase."
  sent35b should "contain a ubiquitination with cause" in {
    val mentions = getBioMentions(sent35b)
    hasPositiveRegulationByEntity("Beta-catenin", "Ubiquitination", List("E3 ubiquitin ligase"), mentions) should be (true)
  }

  val sent35c = "Ubiquitin ubiquitinates beta-catenin."
  sent35c should "not contain a ubiquitination" in {
    val mentions = getBioMentions(sent35c)
    hasPositiveRegulationByEntity("E3 ubiquitin ligase", "Ubiquitination", List("beta-catenin"), mentions) should be (false)
  }

  val sent35d = "Beta-catenin ubiquitinates ubiquitin."
  sent35d should "not contain a ubiquitination" in {
    val mentions = getBioMentions(sent35d)
    hasPositiveRegulationByEntity("Beta-catenin", "Ubiquitination", List("E3 ubiquitin ligase"), mentions) should be (false)
  }

  // Ensure we capture sites that are nominal modifiers of a nominal trigger
  val sent36 = "We did not detect a change in the tyrosine phosphorylation of EGFR in cells expressing Gab1 proteins that are deficient in recruitment of Shp2."
  sent36 should "contain a phosphorylation with a Site" in {
    val mentions = getMentionsFromText(sent36)
    val phosphos = mentions filter(_ matches "Phosphorylation")

    phosphos should have size (1)

    phosphos.head.arguments.keySet should contain ("theme")
    phosphos.head.arguments("theme") should have size (1)
    phosphos.head.arguments("theme").head.text should equal ("EGFR")

    phosphos.head.arguments.keySet should contain ("site")
    phosphos.head.arguments("site") should have size (1)
    phosphos.head.arguments("site").head.text should equal ("tyrosine")
  }

  val sent37 = "The endogenous EGFR is tyrosine phosphorylated in response to EGF in all cell lines."
  sent37 should "contain only a phosphorylation of EGFR by EGF" in {
    val mentions = getBioMentions(sent37)
    val phosphos = mentions filter(_ matches "Phosphorylation")

    phosphos should have size (1)
    phosphos.head.arguments.keySet should contain ("theme")
    phosphos.head.arguments("theme") should have size (1)
    phosphos.head.arguments("theme").head.text should equal ("EGFR")

    phosphos.head.arguments.keySet should contain ("site")
    phosphos.head.arguments("site") should have size (1)
    phosphos.head.arguments("site").head.text should equal ("tyrosine")
  }

  val sent38 = "Both Gab1 and Gab1 F446/472/589 are tyrosine phosphorylated in response to EGF treatment"
  sent38 should "contain only a phosphorylation of EGFR by EGF" in {
    val mentions = getBioMentions(sent38)
    val phosphos = mentions filter(_ matches "Phosphorylation")

    phosphos should have size (2)
    phosphos.foreach(phos => phos.arguments.keySet should contain ("theme"))
    phosphos.foreach(phos => phos.arguments("theme") should have size (1))
    phosphos.foreach(phos => phos.arguments("theme").head.text should equal ("Gab1"))

    phosphos.foreach(phos => phos.arguments.keySet should contain ("site"))
    phosphos.foreach(phos => phos.arguments("site") should have size (1))
    phosphos.foreach(phos => phos.arguments("site").head.text should equal ("tyrosine"))
  }

  val sent39 = "However, while MEK5D phosphorylated a kinase dead mutant of ERK5 (ERK5-KD) at its TEY site"
  sent39 should "not contain a phosphorylation of MEK5D" in {
    val mentions = getBioMentions(sent39)
    hasEventWithArguments("Phosphorylation", Seq("MEK5D"), mentions) should be (false)
  }
  val sent40 = "MEK5D phosphorylated ERK5."
  sent40 should "contain a phosphorylation of ERK5 by MEK5D" in {
    val mentions = getBioMentions(sent40)
    hasEventWithArguments("Phosphorylation", Seq("ERK5"), mentions) should be (true)
    hasPositiveRegulationByEntity("MEK5D", "Phosphorylation", Seq("ERK5"), mentions) should be (true)
  }
  val sent41 = "However, while MEK5D phosphorylated a kinase dead ERK5."
  sent41 should "not contain a phosphorylation of MEK5D" in {
    val mentions = getBioMentions(sent41)
    hasEventWithArguments("Phosphorylation", Seq("MEK5D"), mentions) should be (false)
  }

  val sent42 = "Expression of SIRT1, SIRT2, and acetylated (Ac)-p53 in gastric cancer cell lines."
  sent42 should "not contain an acetylation of SIRT1" in {
    val mentions = getBioMentions(sent42)
    hasEventWithArguments("Acetylation", Seq("SIRT1"), mentions) should be (false)
  }

  val sent43 = "SIRT1 ubiquitylates MEK5D"
  sent43 should "contain a ubiqutination" in {
    val mentions = getBioMentions(sent43)
    hasEventWithArguments("Ubiquitination", Seq("MEK5D"), mentions) should be (true)
  }

  val sent44 = "Activated Akt phosphorylates FoxO3a protein at Ser-318 and Ser-321 and Ser 253"
  sent44 should "contain three controlled phosphorylations" in {
    val mentions = getBioMentions(sent44)
    mentions.filter(_ matches "Phosphorylation") should have size (3)
    mentions.filter(_ matches "Positive_regulation") should have size (3)
  }
}
