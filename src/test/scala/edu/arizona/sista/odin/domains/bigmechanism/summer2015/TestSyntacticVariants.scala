package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import TestUtils._

/**
 * Testing for common syntactic variations on rules, e.g. passive voice, relative clauses, etc.
 */

class Demo {

  //
  // TODO: Mihai will port these soon to the new testing framework. Please do not touch for now.
  //

  /**





  // Phospho tests

  // Hydrox tests
  @Test def testHydroxylationDecl1() {
    val text = "Ras is hydroxylating ASPP2."
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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
    val doc = testReach.mkDoc(text, "testdoc")
    val mentions = testReach.extractFrom(doc)
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

  */
}
