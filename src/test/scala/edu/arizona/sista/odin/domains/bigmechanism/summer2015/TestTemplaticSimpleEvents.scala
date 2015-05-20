package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

/**
 * Unit tests to ensure that SimpleEvent rules coming from the templatic grammar are matching correctly
 * Date: 5/19/15
 */
class TestTemplaticSimpleEvents extends FlatSpec with Matchers {
  val sent1 = "The phosphorylation on AKT was great."
  sent1 should "not produce a phosphorylation based on the preposition \"on\"" in {
    // TODO: Fails! (GUS)
    val mentions = parseSentence(sent1)
    val p = mentions.find(_.label == "Phosphorylation")
    p.isDefined should be (false)
  }

  val sent2 = "JAK3 phosphorylates three HuR residues (Y63, Y68, Y200)"
  it should "extract 3 phosphorylations and 3 positive regulations" in {
    // TODO: this fails because of bad syntax around Hur. Fix with a surface rule for phosphorylation? (GUS)
    val mentions = parseSentence(sent2)

    val p = mentions.filter(_.label == "Phosphorylation")
    p should have size (3)
    val r = mentions.filter(_.label == "Positive_regulation")
    r should have size (3)
  }

  val sent3 = "The deubiquitination of ASPP2 is promoted by optineurin."
  sent3 should "not contain a ubiquitination event" in {
    val doc = testReach.mkDoc(sent3, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_.label == "Ubiquitination") should be (false)
  }

  val sent4 = "The dephosphorylation of ASPP2 is promotted by optineurin."
  sent4 should "not contain a phosphorylation event" in {
    val doc = testReach.mkDoc(sent4, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_.label == "Phosphorylation") should be (false)
  }

  // This test has been ported from TestDarpaEval2015Training
  val sent5 = "In contrast, the EGFR T669A mutant increased both basal EGFR and ERBB3 tyrosine phosphorylation that was not augmented by MEK inhibition"
  sent5 should "contain two phosphorylations" in {
    val text = sent5
    val doc = testReach.mkDoc(text, "testdoc")
    val phosphorylations = testReach.extractFrom(doc).filter(_.label == "Phosphorylation")
    phosphorylations.size should be (2)
    TestUtils.hasEventWithArguments("Phosphorylation", List("EGFR"), phosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("ERBB3"), phosphorylations) should be (true)
  }

  // This test has been ported from TestDarpaEval2015Training
  val sent6 = "We hypothesized that MEK inhibition activates AKT by inhibiting ERK activity, which blocks an inhibitory threonine phosphorylation on the JM domains of EGFR and HER2, thereby increasing ERBB3 phosphorylation."
  sent6 should "contain three phosphorylations" in {
    val text = sent6
    val doc = testReach.mkDoc(text, "testdoc")
    val phosphorylations = testReach.extractFrom(doc).filter(_.label == "Phosphorylation")
    phosphorylations.size should be (3)
    TestUtils.hasEventWithArguments("Phosphorylation", List("EGFR"), phosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("HER2"), phosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("ERBB3"), phosphorylations) should be (true)
  }

  // there is a phosphorylation event in the example text
  val sent7 = "The ubiquitinated Ras protein phosphorylates AKT."
  sent7 should "contain a phosphorylation" in {
    val mentions = testReach.extractFrom(sent7, "testdoc", "1")
    val phospho = mentions.find(_.label == "Phosphorylation")
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
    val mentions = parseSentence(sent8)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent9 = "The effects of monoubiquitination on Ras are not isoform-specific."
  sent9 should "contain a ubiquitination" in {
    val mentions = parseSentence(sent9)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent10 = "We measured the rate of GAP-mediated GTP hydrolysis and observed that the response of Ras ligated to Ubiquitin was identical"
  sent10 should "contain a ubiquitination NOT binding" in {
    val mentions = parseSentence(sent10)
    // per Ryan/Guang's comment this is Ubiq not Binding
    // we do not do hydrolysis, so we can ignore that
    hasEventWithArguments("Binding", List("Ras", "Ubiquitin"), mentions) should be (false)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent11 = "monoubiquitinated K-Ras is less sensitive than the unmodified protein to GAP-mediated GTP hydrolysis"
  sent11 should "contain a ubiquitination" in {
    val mentions = parseSentence(sent11)
    hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions) should be (false)
  }

  val sent12 = "Here we show that monoubiquitination decreases the sensitivity of Ras to GAP-mediated hydrolysis"
  sent12 should "contain a ubiquitination" in {
    val mentions = parseSentence(sent12)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent13 = "Indicating that p38 SAPK is not an efficient kinase for ASPP2 phosphorylation."
  sent13 should "contain a phosphorylation" in {
    val mentions = parseSentence(sent13)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent14a = "Experiments revealed ubiquitination at Lys residues 104 and 147 of K-Ras"
  sent14a should "contain 2 ubiquitinations" in {
    val mentions = parseSentence(sent14a)
    mentions.filter(_.label == "Ubiquitination") should have size (2)
  }

  val sent14b = "Experiments revealed ubiquitination at Lys residues 117, 147, and 170 for H-Ras."
  sent14b should "contain 3 ubiquitinations" in {
    val mentions = parseSentence(sent14b)
    mentions.filter(_.label == "Ubiquitination") should have size (3)
  }

  "testHydrolysisPass1" should "find 1 hydrolysis event" in {
    val mentions = parseSentence("Ras-GDP is hydrolyzed by 26S proteasome without ubiquitination.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisSubjNom1" should "find 1 hydrolysis event" in {
    val mentions = parseSentence("MEK hydrolysis of Ras-GDP increased.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisObjNom1" should "find 1 hydrolysis event" in {
    val mentions = parseSentence("Ras-GDP hydrolysis by MEK increased.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisSubjectRel1" should "find 1 hydrolysis event" in {
    val mentions = parseSentence("Its many abnormal phenotypes can be rescued via Pde2, which specifically hydrolyzes Ras-GDP.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisSubjectRel2" should "find 1 hydrolysis event" in {
    val mentions = parseSentence("Pde2, which has been found to hydrolyze Ras-GDP, activates MEK.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisSubjectRelApposition1" should "find 1 hydrolysis event" in {
    val mentions = parseSentence("Its many abnormal phenotypes can be rescued via overexpressing Pde2, a phosphodiesterase that specifically hydrolyzes Ras-GDP.")
    hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testHydrolysisSubjectRelApposition2" should "find 1 hydrolysis event" in {
    val mentions = parseSentence("A main rate-controlling step in RAS is renin, an enzyme that hydrolyzes Ras-GTP.")
    hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions) should be (true)
  }

  "testHydrolysisObjectRel1" should "find 1 hydrolysis event" in {
    val mentions = parseSentence("We measured transcription activation in the presence of MEK, which is hydrolyzed by CRP.")
    hasEventWithArguments("Hydrolysis", List("MEK"), mentions) should be (true)
  }

  "testPhosphorylationDecl1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = parseSentence("Ras is phosphorylating ASPP2.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationPass1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = parseSentence("ASPP2 is phosphorylated by Ras.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationSubjNom1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = parseSentence("Ras phosphorylation of ASPP2 increased.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationObjNom1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = parseSentence("ASPP2 phosphorylation by Ras increased.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationSubjectRel1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = parseSentence("Its many abnormal phenotypes can be rescued via Ras, which specifically phosphorylates ASPP2.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationSubjectRel2" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = parseSentence("Ras, which has been found to phosphorylate ASPP2, activates MEK.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationSubjectRelApposition1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = parseSentence("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically phosphorylates ASPP2.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationSubjectRelApposition2" should "find 1 phosphorylation event" in {
    val mentions = parseSentence("A main rate-controlling step in AAAA is renin, an enzyme that phosphorylates ASPP2 to generate XXXX")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testPhosphorylationObjectRel1" should "find 1 phosphorylation event and 1 regulation event" in {
    val mentions = parseSentence("We measured transcription activation in the presence of ASPP2, which is phosphorylated by Ras.")
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationDecl1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = parseSentence("Ras is hydroxylating ASPP2.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationPass1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = parseSentence("ASPP2 is hydroxylated by Ras.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationSubjNom1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = parseSentence("Ras hydroxylation of ASPP2 increased.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationObjNom1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = parseSentence("ASPP2 hydroxylation by Ras increased.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationSubjectRel1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = parseSentence("Its many abnormal phenotypes can be rescued via Ras, which specifically hydroxylates ASPP2.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationSubjectRel2" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = parseSentence("Ras, which has been found to hydroxylate ASPP2, activates MEK.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationSubjectRelApposition1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = parseSentence("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically hydroxylates ASPP2.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationSubjectRelApposition2" should "find 1 hydroxylation event" in {
    val mentions = parseSentence("A main rate-controlling step in AAAA is renin, an enzyme that hydroxylates ASPP2 to generate XXXX")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testHydroxylationObjectRel1" should "find 1 hydroxylation event and 1 regulation event" in {
    val mentions = parseSentence("We measured transcription activation in the presence of ASPP2, which is hydroxylated by Ras.")
    hasEventWithArguments("Hydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Hydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationDecl1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = parseSentence("Ras is ubiquitinating ASPP2.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationPass1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = parseSentence("ASPP2 is ubiquitinated by Ras.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationSubjNom1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = parseSentence("Ras ubiquitination of ASPP2 increased.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationObjNom1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = parseSentence("ASPP2 ubiquitination by Ras increased.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationObjNom2" should "find 1 ubiquitination event and 2 regulation events" in {
    val mentions = parseSentence("RAS ubiquitination and degradation by ASPP2 and p53 increased.")
    hasEventWithArguments("Ubiquitination", List("RAS"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP2", "Ubiquitination", List("RAS"), mentions) should be (true)
    hasPositiveRegulationByEntity("p53", "Ubiquitination", List("RAS"), mentions) should be (true)
  }

  "testUbiquitinationSubjectRel1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = parseSentence("Its many abnormal phenotypes can be rescued via Ras, which specifically ubiquitinates ASPP2.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationSubjectRel2" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = parseSentence("Ras, which has been found to ubiquitinate ASPP2, activates MEK.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationSubjectRelApposition1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = parseSentence("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically ubiquitinates ASPP2.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationSubjectRelApposition2" should "find 1 ubiquitination event" in {
    val mentions = parseSentence("A main rate-controlling step in AAAA is renin, an enzyme that ubiquitinates ASPP2 to generate XXXX")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testUbiquitinationObjectRel1" should "find 1 ubiquitination event and 1 regulation event" in {
    val mentions = parseSentence("We measured transcription activation in the presence of ASPP2, which is ubiquitinated by Ras.")
    hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Ubiquitination", List("ASPP2"), mentions) should be (true)
  }
}
