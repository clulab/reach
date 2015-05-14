package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.junit.Assert._
import org.junit.Test
import TestResources.reach
import DarpaEvalUtils._

/**
 * Testing for common syntactic variations on rules, e.g. passive voice, relative clauses, etc.
 */

class TestSyntacticVariants {

  /**
   * TODO: Coref
  @Test def testHydrolysisDecl1() {
    val doc = bioproc.annotate("RasGAP is hydrolyzing GTP to GDP in Ras reactions.")
    val mentions = extractor.extractFrom(doc)

    try {
      // TODO: fix hasEventWithArguments to match Complex (RelationMention) with desired argument.
      assertTrue("hydrolysis with COREF (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectDecl1")
        displayMentions(mentions, doc)
        throw e
    }
  }
*/
  @Test def testHydrolysisPass1() {
    val text = "Ras-GDP is hydrolyzed by 26S proteasome without ubiquitination."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectPass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisSubjNom1() {
    val text = "MEK hydrolysis of Ras-GDP increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisObjNom1() {
    val text = "Ras-GDP hydrolysis by MEK increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisSubjectRel1() {
    val text = "Its many abnormal phenotypes can be rescued via Pde2, which specifically hydrolyzes Ras-GDP."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisSubjectRel2() {
    val text = "Pde2, which has been found to hydrolyze Ras-GDP, activates MEK."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectRel2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisSubjectRelApposition1() {
    val text = "Its many abnormal phenotypes can be rescued via overexpressing Pde2, a phosphodiesterase that specifically hydrolyzes Ras-GDP."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectRelApposition1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisSubjectRelApposition2() {
    val text = "A main rate-controlling step in RAS is renin, an enzyme that hydrolyzes Ras-GTP."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectApposition2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisObjectRel1() {
    val text = "We measured transcription activation in the presence of MEK, which is hydrolyzed by CRP."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("MEK"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisObjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingDecl1() {
    val text = "Mechanistically, ASPP1 and ASPP2 bind RAS-GTP."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1", "ASPP2", "RAS-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingDecl1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingDecl2() {
    val text = "Mechanistically, ASPP1 and ASPP2 bind with RAS-GTP." 
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1", "ASPP2", "RAS-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingDecl2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingPass1() {
    val text = "Mechanistically, ASPP1 and ASPP2 are bound by RAS-GTP."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1", "ASPP2", "RAS-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingPass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingPrepNom1() {
    val text = "We detected elevated binding of p53 to K-Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingPrepNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingPrepNom2() {
    val text = "We detected elevated binding of p53 and K-Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingPrepNom2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingPrepNom3() {
    val text = "We detected elevated binding of p53 with K-Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingPrepNom3")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingSubjNom1() {
    val text = "We detected elevated p53 binding to K-Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingSubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingObjNom1() {
    val text = "We detected elevated K-Ras binding by p53."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingSubjRel1() {
    val text = "We detected elevated phosphorylation of K-Ras, a protein that subsequently binds p53."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingSubjRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingObjRel1() {
    val text = "We detected elevated phosphorylation of K-Ras, a protein that is subsequently bound by p53."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingObjRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testTranslocation1() {
    val text = "Phosphorylation leads the plasma membrane to release p53 to the cytosol."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("Translocation (ENRIQUE)", hasEventWithArguments("Translocation", List("p53", "plasma membrane", "cytosol"), mentions))
    } catch {
      case e: AssertionError =>
        header("testTranslocation1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testTranslocation2() {
    val text = "Recruitment of p53 from the cytosol to the plasma membrane increases with phosphorylation."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("Translocation (ENRIQUE)", hasEventWithArguments("Translocation", List("p53", "plasma membrane", "cytosol"), mentions))
    } catch {
      case e: AssertionError =>
        header("testTranslocation2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testTranslocation3() {
    val text = "With increased phosphorylation, p53 is exported from the plasma membrane to the cytosol."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("Translocation (ENRIQUE)", hasEventWithArguments("Translocation", List("p53", "plasma membrane", "cytosol"), mentions))
    } catch {
      case e: AssertionError =>
        header("testTranslocation3")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testTranslocation4() {
    val text = "ASPP2, a protein which is Translocationed from the membrane to the nucleus, is subsequently phosphorylated."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("Translocation (ENRIQUE)", hasEventWithArguments("Translocation", List("ASPP2", "membrane", "nucleus"), mentions))
    } catch {
      case e: AssertionError =>
        header("testTranslocation4")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testTranslocation5() {
    val text = "ASPP2, a protein which translocates Pde2 from the membrane to the nucleus, is subsequently phosphorylated."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("Translocation (ENRIQUE)", hasEventWithArguments("Translocation", List("Pde2", "membrane", "nucleus"), mentions))
    } catch {
      case e: AssertionError =>
        header("testTranslocation4")
        displayMentions(mentions, doc)
        throw e
    }
  }

  // Phospho tests
  @Test def testPhosphorylationDecl1() {
    val text = "Ras is phosphorylating ASPP2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectDecl1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationPass1() {
    val text = "ASPP2 is phosphorylated by Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectPass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationSubjNom1() {
    val text = "Ras phosphorylation of ASPP2 increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationObjNom1() {
    val text = "ASPP2 phosphorylation by Ras increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testPhosphorylationObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationSubjectRel1() {
    val text = "Its many abnormal phenotypes can be rescued via Ras, which specifically phosphorylates ASPP2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationSubjectRel2() {
    val text = "Ras, which has been found to phosphorylate ASPP2, activates MEK."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationSubjectRelApposition1() {
    val text = "Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically phosphorylates ASPP2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRelApposition1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationSubjectRelApposition2() {
    val text = "A main rate-controlling step in AAAA is renin, an enzyme that phosphorylates ASPP2 to generate XXXX"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectApposition2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationObjectRel1() {
    val text = "We measured transcription activation in the presence of ASPP2, which is phosphorylated by Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  // Hydrox tests
  @Test def testHydroxylationDecl1() {
    val text = "Ras is hydroxylating ASPP2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectDecl1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationPass1() {
    val text = "ASPP2 is hydroxylated by Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectPass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationSubjNom1() {
    val text = "Ras hydroxylation of ASPP2 increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationObjNom1() {
    val text = "ASPP2 hydroxylation by Ras increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydroxylationObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationSubjectRel1() {
    val text = "Its many abnormal phenotypes can be rescued via Ras, which specifically hydroxylates ASPP2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationSubjectRel2() {
    val text = "Ras, which has been found to hydroxylate ASPP2, activates MEK."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationSubjectRelApposition1() {
    val text = "Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically hydroxylates ASPP2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRelApposition1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationSubjectRelApposition2() {
    val text = "A main rate-controlling step in AAAA is renin, an enzyme that hydroxylates ASPP2 to generate XXXX"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectApposition2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationObjectRel1() {
    val text = "We measured transcription activation in the presence of ASPP2, which is hydroxylated by Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  // Ubiq tests
  @Test def testUbiquitinationDecl1() {
    val text = "Ras is ubiquitinating ASPP2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectDecl1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationPass1() {
    val text = "ASPP2 is ubiquitinated by Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectPass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationSubjNom1() {
    val text = "Ras ubiquitination of ASPP2 increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationObjNom1() {
    val text = "ASPP2 ubiquitination by Ras increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testUbiquitinationObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationObjNom2() {
    val text = "RAS ubiquitination and degradation by ASPP2 and p53 increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("RAS"), mentions))
      assertTrue(s"ASPP2 regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("RAS"), mentions))
      assertTrue(s"p53 regulation ($assignedParty)", hasPositiveRegulationByEntity("p53", eventLabel, List("RAS"), mentions))
    } catch {
      case e: AssertionError =>
        header("testUbiquitinationObjNom2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationSubjectRel1() {
    val text = "Its many abnormal phenotypes can be rescued via Ras, which specifically ubiquitinates ASPP2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationSubjectRel2() {
    val text = "Ras, which has been found to ubiquitinate ASPP2, activates MEK."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationSubjectRelApposition1() {
    val text = "Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically ubiquitinates ASPP2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRelApposition1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationSubjectRelApposition2() {
    val text = "A main rate-controlling step in AAAA is renin, an enzyme that ubiquitinates ASPP2 to generate XXXX"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectApposition2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationObjectRel1() {
    val text = "We measured transcription activation in the presence of ASPP2, which is ubiquitinated by Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }
}
