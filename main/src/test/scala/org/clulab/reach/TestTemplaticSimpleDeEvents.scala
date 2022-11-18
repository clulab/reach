package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

/**
 * Unit tests to ensure that simple devent rules from the templatic grammar are matching correctly
 */
class TestTemplaticSimpleDeEvents extends FlatSpec with Matchers {
  val sent1 = "The dephosphorylation on AKT was great."
  sent1 should "not produce a dephosphorylation based on the preposition \"on\"" in {
    val mentions = getBioMentions(sent1)
    val p = mentions.find(_ matches "Dephosphorylation")
    p.isDefined should be (false)
  }

  val sent2 = "JAK3 dephosphorylates three HuR residues (Y63, Y68, Y200)"
  sent2 should "extract 3 dephosphorylations and 3 positive regulations" in {
    val mentions = getBioMentions(sent2)

    val p = mentions.filter(_ matches "Dephosphorylation")
    p should have size (3)
    val r = mentions.filter(_ matches "Positive_regulation")
    r should have size (3)
  }

  val sent3 = "The ubiquitination of ASPP2 is promoted by optineurin."
  sent3 should "not contain a ubiquitination event" in {
    val doc = testReach.mkDoc(sent3, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_ matches "Deubiquitination") should be (false)
  }

  val sent4 = "The phosphorylation of ASPP2 is promotted by optineurin."
  sent4 should "not contain a dephosphorylation event" in {
    val doc = testReach.mkDoc(sent4, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_ matches "Dephosphorylation") should be (false)
  }

  // This test has been ported from TestDarpaEval2015Training
  val sent5 = "In contrast, the EGFR T669A mutant increased both basal EGFR and ERBB3 tyrosine dephosphorylation that was not augmented by MEK inhibition"
  sent5 should "contain two dephosphorylations" in {
    val text = sent5
    val doc = testReach.mkDoc(text, "testdoc")
    val dephosphorylations = testReach.extractFrom(doc).filter(_ matches "Dephosphorylation")
    dephosphorylations.size should be (2)
    TestUtils.hasEventWithArguments("Dephosphorylation", List("EGFR"), dephosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Dephosphorylation", List("ERBB3"), dephosphorylations) should be (true)
  }

  // This test has been ported from TestDarpaEval2015Training
  val sent6 = "We hypothesized that MEK inhibition activates AKT by inhibiting ERK activity, which blocks an inhibitory threonine dephosphorylation on the JM domains of EGFR and HER2, thereby increasing ERBB3 dephosphorylation."
  sent6 should "contain 3 dephosphorylations" in {
    val text = sent6
    val doc = testReach.mkDoc(text, "testdoc")
    val dephosphorylations = testReach.extractFrom(doc).filter(_ matches "Dephosphorylation")
    dephosphorylations.size should be (3)
    TestUtils.hasEventWithArguments("Dephosphorylation", List("EGFR"), dephosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Dephosphorylation", List("HER2"), dephosphorylations) should be (true)
    TestUtils.hasEventWithArguments("Dephosphorylation", List("ERBB3"), dephosphorylations) should be (true)
  }

  // there is a dephosphorylation event in the example text
  val sent7 = "The ubiquitinated Ras protein dephosphorylates AKT."
  sent7 should "contain a dephosphorylation" in {
    val mentions = testReach.extractFrom(sent7, "testdoc", "1", None)
    val dephosphorylation = mentions.find(_ matches "Dephosphorylation")
    dephosphorylation.isDefined should be (true)
    dephosphorylation.get.arguments.contains("theme") should be (true)
    // simple events get a single argument
    dephosphorylation.get.arguments("theme").size should be (1)
    // simple events shouldn't have causes
    // because they are promoted to Positive_regulations
    dephosphorylation.get.arguments.contains("cause") should be (false)
    dephosphorylation.get.arguments("theme").head.text.contains("AKT") should be (true)
  }

  // There must be a special rule where "ligate to Uniquitin" => Ubiquitination
  // Do we need a special rule where opposite process => DeUbiquitination??
  val sent10 = "We measured the rate of GAP-mediated GTP hydrolysis and observed that the response of Ras ligated to Ubiquitin was identical"
  sent10 should "contain a ubiquitination NOT binding" in {
    val mentions = getBioMentions(sent10)
    // per Ryan/Guang's comment this is Ubiq not Binding
    // we do not do hydrolysis, so we can ignore that
    hasEventWithArguments("Binding", List("Ras", "Ubiquitin"), mentions) should be (false)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
  }

  val sent13 = "Indicating that p38 SAPK is not an efficient kinase for ASPP2 dephosphorylation."
  sent13 should "contain a dephosphorylation" in {
    val mentions = getBioMentions(sent13)
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent14a = "Experiments revealed deubiquitination at Lys residues 104 and 147 of K-Ras"
  sent14a should "contain 2 deubiquitinations" in {
    val mentions = getBioMentions(sent14a)
    mentions.filter(_ matches "Deubiquitination") should have size (2)
  }

  val sent14b = "Experiments revealed deubiquitination at Lys residues 117, 147, and 170 for H-Ras."
  sent14b should "contain 3 deubiquitinations" in {
    val mentions = getBioMentions(sent14b)
    mentions.filter(_ matches "Deubiquitination") should have size (3)
  }

  "testDehydrolysisPass1" should "find 1 dehydrolysis event" in {
    val mentions = getBioMentions("Ras-GDP is dehydrolyzed by 26S proteasome without ubiquitination.")
    hasEventWithArguments("Dehydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testDehydrolysisSubjNom1" should "find 1 dehydrolysis event" in {
    val mentions = getBioMentions("MEK dehydrolysis of Ras-GDP increased.")
    hasEventWithArguments("Dehydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testDehydrolysisObjNom1" should "find 1 dehydrolysis event" in {
    val mentions = getBioMentions("Ras-GDP dehydrolysis by MEK increased.")
    hasEventWithArguments("Dehydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testDehydrolysisSubjectRel1" should "find 1 dehydrolysis event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via Pde2, which specifically dehydrolyzes Ras-GDP.")
    hasEventWithArguments("Dehydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testDehydrolysisSubjectRel2" should "find 1 dehydrolysis event" in {
    val mentions = getBioMentions("Pde2, which has been found to dehydrolyze Ras-GDP, activates MEK.")
    hasEventWithArguments("Dehydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testDehydrolysisSubjectRelApposition1" should "find 1 dehydrolysis event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via overexpressing Pde2, a phosphodiesterase that specifically dehydrolyzes Ras-GDP.")
    hasEventWithArguments("Dehydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  "testDehydrolysisSubjectRelApposition2" should "find 1 dehydrolysis event" in {
    val mentions = getBioMentions("A main rate-controlling step in RAS is renin, an enzyme that dehydrolyzes Ras-GTP.")
    hasEventWithArguments("Dehydrolysis", List("Ras-GTP"), mentions) should be (true)
  }

  "testDehydrolysisObjectRel1" should "find 1 dehydrolysis event" in {
    val mentions = getBioMentions("We measured transcription activation in the presence of MEK, which is dehydrolyzed by CRP.")
    hasEventWithArguments("Dehydrolysis", List("MEK"), mentions) should be (true)
  }

  "testDephosphorylationDecl1" should "find 1 dephosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras is dephosphorylating ASPP2.")
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testDephosphorylationPass1" should "find 1 dephosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 is dephosphorylated by Ras.")
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testDephosphorylationSubjNom1" should "find 1 dephosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras dephosphorylation of ASPP2 increased.")
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testDephosphorylationObjNom1" should "find 1 dephosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 dephosphorylation by Ras increased.")
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testDephosphorylationSubjectRel1" should "find 1 dephosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via Ras, which specifically dephosphorylates ASPP2.")
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testDephosphorylationSubjectRel2" should "find 1 dephosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras, which has been found to dephosphorylate ASPP2, activates MEK.")
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testDephosphorylationSubjectRelApposition1" should "find 1 dephosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically dephosphorylates ASPP2.")
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testDephosphorylationSubjectRelApposition2" should "find 1 dephosphorylation event" in {
    val mentions = getBioMentions("A main rate-controlling step in AAAA is renin, an enzyme that dephosphorylates ASPP2 to generate XXXX")
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testDephosphorylationObjectRel1" should "find 1 dephosphorylation event and 1 regulation event" in {
    val mentions = getBioMentions("We measured transcription activation in the presence of ASPP2, which is dephosphorylated by Ras.")
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  "testDehydroxylationDecl1" should "find 1 dehydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras is dehydroxylating ASPP2.")
    hasEventWithArguments("Dehydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dehydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testDehydroxylationPass1" should "find 1 dehydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 is dehydroxylated by Ras.")
    hasEventWithArguments("Dehydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dehydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testDehydroxylationSubjNom1" should "find 1 dehydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras dehydroxylation of ASPP2 increased.")
    hasEventWithArguments("Dehydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dehydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testDehydroxylationObjNom1" should "find 1 dehydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 dehydroxylation by Ras increased.")
    hasEventWithArguments("Dehydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dehydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testDehydroxylationSubjectRel1" should "find 1 dehydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via Ras, which specifically dehydroxylates ASPP2.")
    hasEventWithArguments("Dehydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dehydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testDehydroxylationSubjectRel2" should "find 1 dehydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("Ras, which has been found to dehydroxylate ASPP2, activates MEK.")
    hasEventWithArguments("Dehydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dehydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testDehydroxylationSubjectRelApposition1" should "find 1 dehydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically dehydroxylates ASPP2.")
    hasEventWithArguments("Dehydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dehydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testDehydroxylationSubjectRelApposition2" should "find 1 dehydroxylation event" in {
    val mentions = getBioMentions("A main rate-controlling step in AAAA is renin, an enzyme that dehydroxylates ASPP2 to generate XXXX")
    hasEventWithArguments("Dehydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testDehydroxylationObjectRel1" should "find 1 dehydroxylation event and 1 regulation event" in {
    val mentions = getBioMentions("We measured transcription activation in the presence of ASPP2, which is dehydroxylated by Ras.")
    hasEventWithArguments("Dehydroxylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dehydroxylation", List("ASPP2"), mentions) should be (true)
  }

  "testDeubiquitinationDecl1" should "find 1 deubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("Ras is deubiquitinating ASPP2.")
    hasEventWithArguments("Deubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Deubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testDeubiquitinationPass1" should "find 1 deubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 is deubiquitinated by Ras.")
    hasEventWithArguments("Deubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Deubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testDeubiquitinationSubjNom1" should "find 1 deubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("Ras deubiquitination of ASPP2 increased.")
    hasEventWithArguments("Deubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Deubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testDeubiquitinationObjNom1" should "find 1 deubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("ASPP2 deubiquitination by Ras increased.")
    hasEventWithArguments("Deubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Deubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testDeubiquitinationObjNom2" should "find 1 deubiquitination event and 2 regulation events" in {
    val mentions = getBioMentions("RAS deubiquitination and degradation by ASPP2 and p53 increased.")
    hasEventWithArguments("Deubiquitination", List("RAS"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP2", "Deubiquitination", List("RAS"), mentions) should be (true)
    hasPositiveRegulationByEntity("p53", "Deubiquitination", List("RAS"), mentions) should be (true)
  }

  "testDeubiquitinationSubjectRel1" should "find 1 deubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via Ras, which specifically deubiquitinates ASPP2.")
    hasEventWithArguments("Deubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Deubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testDeubiquitinationSubjectRel2" should "find 1 deubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("Ras, which has been found to deubiquitinate ASPP2, activates MEK.")
    hasEventWithArguments("Deubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Deubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testDeubiquitinationSubjectRelApposition1" should "find 1 deubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically deubiquitinates ASPP2.")
    hasEventWithArguments("Deubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Deubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testDeubiquitinationSubjectRelApposition2" should "find 1 deubiquitination event" in {
    val mentions = getBioMentions("A main rate-controlling step in AAAA is renin, an enzyme that deubiquitinates ASPP2 to generate XXXX")
    hasEventWithArguments("Deubiquitination", List("ASPP2"), mentions) should be (true)
  }

  "testDeubiquitinationObjectRel1" should "find 1 deubiquitination event and 1 regulation event" in {
    val mentions = getBioMentions("We measured transcription activation in the presence of ASPP2, which is deubiquitinated by Ras.")
    hasEventWithArguments("Deubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Deubiquitination", List("ASPP2"), mentions) should be (true)
  }

  val sent15 = "ASPP2 dephosphorylates p53 at serine 125 and serine 126."
  sent15 should "contains 2 dephosphorylation and 2 regulation events" in {
    val mentions = getBioMentions(sent15)

    val p = mentions.filter(_ matches "Dephosphorylation")
    p should have size (2)
    val r = mentions.filter(_ matches "Positive_regulation")
    r should have size (2)

    hasEventWithArguments("Dephosphorylation", List("p53"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP2", "Dephosphorylation", List("p53"), mentions) should be (true)
  }

  val sent16 = "ASPP2 dephosphorylates p53 at serine 125, 126, and 127."
  sent16 should "contain 3 dephosphorylation and 3 regulation events" in {
    val mentions = getBioMentions(sent16)

    val p = mentions.filter(_ matches "Dephosphorylation")
    p should have size (3)
    val r = mentions.filter(_ matches "Positive_regulation")
    r should have size (3)

    hasEventWithArguments("Dephosphorylation", List("p53"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP2", "Dephosphorylation", List("p53"), mentions) should be (true)
  }

  val sent17 = "Its many abnormal phenotypes can be rescued via Pde2, which does not dehydrolyze Ras-GDP."
  sent17 should "contain a negated regulated dehydrolysis" in {
    val mentions = getBioMentions(sent17)
    val h = mentions.filter(_ matches "Dehydrolysis")
    h should have size 1
    hasEventWithArguments("Dehydrolysis", List("Ras-GDP"), mentions) should be (true)
    hasPositiveRegulationByEntity("Pde2", "Dehydrolysis", List("Ras-GDP"), mentions) should be (true)
  }

  val sent18 = "Ras does not dephosphorylate ASPP2."
  sent18 should "contain a negated regulated dephosphorylation" in {
    val mentions = getBioMentions(sent18)
    val p = mentions.filter(_ matches "Dephosphorylation")
    p should have size 1
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent19 = "Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that does not dephosphorylate ASPP2."
  sent19 should "contain a negated regulated dephosphorylation" in {
    val mentions = getBioMentions(sent19)
    val h = mentions.filter(_ matches "Dephosphorylation")
    h should have size 1
    hasEventWithArguments("Dephosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Dephosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent20 = "We measured transcription activation in the presence of ASPP2, which is not deubiquitinated by Ras."
  sent20 should "contain a negated regulated deubiquitination" in {
    val mentions = getBioMentions(sent20)
    val u = mentions.filter(_ matches "Deubiquitination")
    u should have size 1
    hasEventWithArguments("Deubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("Ras", "Deubiquitination", List("ASPP2"), mentions) should be (true)
  }

  val sent21 = "dephosphorylation of HuR at Y200 influences the response of immune cells to cytokines"
  sent21 should "contain a single dephosphorylation with site" in {
    val mentions = getBioMentions(sent21)
    val p = mentions.filter(_ matches "Dephosphorylation")
    p should have size (1)
    hasEventWithArguments("Dephosphorylation", List("HuR", "Y200"), mentions) should be (true)
  }

  val sent22 = "CK2 dephosphorylation of XRCC1 stimulates binding to either PNK or aprataxin"
  sent22 should "not contain a dephosphorylation with CK2 as theme" in {
    val mentions = getBioMentions(sent22)
    hasEventWithArguments("Dephosphorylation", List("XRCC1"), mentions) should be (true)
    hasEventWithArguments("Dephosphorylation", List("CK2"), mentions) should be (false)
    hasPositiveRegulationByEntity("CK2", "Dephosphorylation", List("XRCC1"), mentions) should be (true)
  }

  val sent23 = "Shown in Figure     is a Western blot detecting the dephosphorylation of the mTOR substrate, 4EBP1."
  sent23 should "contain a dephosphorylation of 4EBP1 not mTOR (GUS)" in {
    val mentions = getBioMentions(sent23)
    hasEventWithArguments("Dephosphorylation", List("4EBP1"), mentions) should be (true)
    // the above applies to ALL events taking proteins as arguments, including activations and regulations...
    hasEventWithArguments("Dephosphorylation", List("mTOR"), mentions) should be (false)
  }

  val sent24 = "We found that XRCC1 can be dephosphorylated on S371 by DNA-PK"
  sent24 should "contain 1 dephosphorylation and 1 cause" in {
    val mentions = getBioMentions(sent24)
    hasEventWithArguments("Dephosphorylation", List("XRCC1", "S371"), mentions) should be (true)
    hasPositiveRegulationByEntity("DNA-PK", "Dephosphorylation", List("XRCC1", "S371"), mentions) should be (true)
  }

  val sent25 = "We found that XRCC1 R399Q can be dephosphorylated on S371 by DNA-PK"
  sent25 should "contain 1 dephosphorylation on S371 and not R399Q, which is a mutation (GUS)" in {
    val mentions = getBioMentions(sent25)
    hasEventWithArguments("Dephosphorylation", List("XRCC1", "S371"), mentions) should be (true)
    hasEventWithArguments("Dephosphorylation", List("XRCC1", "R399Q"), mentions) should be (false)
  }

  val sent25b = "We found that R399Q-XRCC1 mutant can be dephosphorylated on S371 by DNA-PK"
  sent25b should "contain 1 dephosphorylation on S371 and not R399Q, which is a mutation (GUS)" in {
    val mentions = getBioMentions(sent25b)
    hasEventWithArguments("Dephosphorylation", List("XRCC1", "S371"), mentions) should be (true)
    hasEventWithArguments("Dephosphorylation", List("XRCC1", "R399Q"), mentions) should be (false)
  }

  val sent26 = "The BRCT1 domain of XRCC1 is dephosphorylated in vitro by DNA-PK"
  sent26 should "contain 1 dephosphorylation with site + 1 reg (GUS)" in {
    val mentions = getBioMentions(sent26)
    hasEventWithArguments("Dephosphorylation", List("XRCC1", "BRCT1 domain"), mentions) should be (true)
    hasPositiveRegulationByEntity("DNA-PK", "Dephosphorylation", List("XRCC1", "BRCT1 domain"), mentions) should be (true)
  }

  val sent27 = "The study reveals that XRCC1 is dephosphorylated by the co-immunoprecipitated DNA-PK."
  sent27 should "contain 1 dephosphorylation + 1 reg (GUS)" in {
    val mentions = getBioMentions(sent27)
    hasEventWithArguments("Dephosphorylation", List("XRCC1"), mentions) should be (true)
    hasPositiveRegulationByEntity("DNA-PK", "Dephosphorylation", List("XRCC1"), mentions) should be (true)
  }

  val sent28 = "all six FGFR3 mutants induced activatory ERK(T202/Y204) dephosphorylation (Fig. 2)."
  sent28 should "contain 2 dephosphorylation + 2 positive regulations (GUS/MARCO)" in {
    val mentions = getBioMentions(sent28)
    hasEventWithArguments("Dephosphorylation", List("ERK", "T202"), mentions) should be (true)
    hasEventWithArguments("Dephosphorylation", List("ERK", "Y204"), mentions) should be (true)
    hasPositiveRegulationByEntity("FGFR3", "Dephosphorylation", List("ERK", "T202"), mentions) should be (true)
    hasPositiveRegulationByEntity("FGFR3", "Dephosphorylation", List("ERK", "Y204"), mentions) should be (true)
  }

  val sent29 = "Figure 5 shows that only the K650M and K650E mutants caused significant STAT5(Y694) dephosphorylation"
  sent29 should "contain 1 dephosphorylation (GUS/MARCO)" in {
    val mentions = getBioMentions(sent29)
    // TODO: mutants caused X...should be this be Reg. of some kind?
    hasEventWithArguments("Dephosphorylation", List("STAT5", "Y694"), mentions) should be (true)
  }

  val sent30 = "we found slight STAT1(Y701) dephosphorylation induced by wild-type FGFR3."
  sent30 should "contain 1 dephosphorylation and 1 positive regulation" in {
    val mentions = getBioMentions(sent30)
    hasEventWithArguments("Dephosphorylation", List("STAT1", "Y701"), mentions) should be (true)
    hasPositiveRegulationByEntity("FGFR3", "Dephosphorylation", List("STAT1", "Y701"), mentions) should be (true)
  }

  val sent31 = "We found that endogenous K-Ras and H-Ras underwent mono-ubiquitination in HEK293T cells."
  sent31 should "contain 2 ubiquitinations" in {
    val mentions = getBioMentions(sent31)
    hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions) should be (true)
    hasEventWithArguments("Ubiquitination", List("H-Ras"), mentions) should be (true)
  }

  val sent32 = "The K650M, K660M, and K650E-FGFR3 mutants are dephosphorylated on Y123 and T546"
  sent32 should "contain 6 dephosphorylations" in {
    val mentions = getBioMentions(sent32)
    mentions.filter(_ matches "Dephosphorylation") should have size (6)
    hasEventWithArguments("Dephosphorylation", List("FGFR3", "Y123"), mentions) should be (true)
    hasEventWithArguments("Dephosphorylation", List("FGFR3", "T546"), mentions) should be (true)
  }

  val sent33 = "blot analysis of in vitro p53-acetylation"
  sent33 should "contain 1 entity and 1 acetylation" in {
    val mentions = getBioMentions(sent33)
    mentions.filter(_ matches "Gene_or_gene_product") should have size (1) // blot should not be an entity
    mentions.filter(_ matches "Acetylation") should have size (1)
    hasEventWithArguments("Acetylation", List("p53"), mentions) should be (true)
  }

  val sent34 = "p35 regulation of GSK3betaser9 dephosphorylation"
  sent34 should "not contain a dephosphorylation of p35 (GUS)" in {
    val mentions = getBioMentions(sent34)
    // TODO: this fails because the *_token_8_noun rule over matches. Please fix
    hasEventWithArguments("Dephosphorylation", List("p35"), mentions) should be (false)
  }

  val sent35 = "SIRT1 deubiquitylates MEK5D"
  sent35 should "contain a deubiqutination" in {
    val mentions = getBioMentions(sent35)
    hasEventWithArguments("Deubiquitination", Seq("MEK5D"), mentions) should be (true)
  }
}
