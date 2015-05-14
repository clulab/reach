package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin.domains.bigmechanism.summer2015.DarpaEvalUtils._
import org.junit.Assert._
import org.junit.Test
import scala.util.{ Try, Success, Failure }
import org.scalatest._
import edu.arizona.sista.bionlp._

class TestInactiveRules extends FlatSpec with Matchers {
  // instantiate ReachSystem for tests
  val reach = new ReachSystem

  // test data
  val text = "The ubiquitinated Ras protein phosphorylates AKT."
  val docId = "testdoc"
  val chunkId = "1"

  /**

  "ProteinWithSite rules" should "find an entity with site" in {
    val text = "To test this hypothesis, we transiently transfected CHO-KI cells, which do not express ERBB receptors endogenously, with wildtype ERBB3 with either wild-type EGFR or EGFR T669A."
    val doc = reach.mkDoc(text, docId)
    val mentions = reach.extractEntitiesFrom(doc)
    hasEntityWithSite("EGFR", "T669A", mentions) should be true
  }

  it should "find an entity with site" in {
    val text = "We observed analogous results in CHO-KI cells expressing wild-type ERBB3 in combination with wild-type or T677A mutant HER2 (Figure 6B)"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    hasEntityWithSite("HER2", "T677A mutant", mentions) should be true
  }

  it should "find an entity with site" in {
    val text = "We hypothesized that MEK inhibition activates AKT by inhibiting ERK activity, which blocks an inhibitory threonine phosphorylation on the JM domains of EGFR and HER2, thereby increasing ERBB3 phosphorylation."
    val doc = reach.mkDoc(text, docId)
    val mentions = reach.extractFrom(doc)

    hasEntityWithSite("EGFR", "JM domains", mentions) should be true
    hasEntityWithSite("HER2", "JM domains", mentions) should be true
  }

  @Test def testDegradationDecl1() {
    val text = "ASPP2 degraded KRAS and RAS."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("KRAS","RAS"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("KRAS", "RAS"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}Decl1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testDegradationPass1() {
    val text = "KRAS and RAS are both degraded by ASPP2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("KRAS","RAS"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("KRAS", "RAS"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}Pass1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testDegradationPrepNom1() {
    val text = "The ubiquitination and degradation of RAS by ASPP2 increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("RAS"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("RAS"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}PrepNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testDegradationObjNom1() {
    val text = "RAS ubiquitination and degradation by ASPP2 increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("RAS"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("RAS"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testDegradationSubjNom1() {
    val text = "ASPP2 ubiquitination and degradation of Ras increased."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testDegradationSubjRel1() {
    val text = "Its many abnormal phenotypes can be rescued via Pde2, which specifically degrades Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("Pde2", eventLabel, List("Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testExchangeDecl1() {
    val text = "Ras exchanges GDP for GTP more rapidly in the presence of Pde2."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Exchange"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras", "GDP", "GTP"), mentions))
      // TODO: amend to find Pde2 (DANE)
      // assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("Pde2", eventLabel, List("Ras", "GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}Decl1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testExchangePass1() {
    val text = "the GDP bound to the Ras protein is not exchanged for GTP."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Exchange"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras", "GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}Pass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  /**
   * TODO: Coref
 @Test def testExchangePrepNom1() {
    val doc = bioproc.annotate("In RAS, the exchange of GDP for GTP increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Exchange"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} with COREF ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras", "GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}PrepNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }
   */

  /**
   * TODO: Coref
  @Test def testExchangeObjNom1() {
    val doc = bioproc.annotate("In Ras, GDP exchange for GTP increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Exchange with COREF"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras", "GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }
   */

  @Test def testExchangeSubjRel1() {
    val text = "Its many abnormal phenotypes can be rescued via Pde2, which normally exchanges GDP with GTP."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Exchange"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testExchangeObjRel1() {
    val text = "Its many abnormal phenotypes can be rescued via GDP, which is normally exchanged with GTP in Ras."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)
    val eventLabel = "Exchange"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras", "GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

    @Test def testRules13() {
    val text = "We propose that once ubiquitination occurs on Ras at Lys147, it enhances GDP/GTP exchange of Ras and increases the fraction of Ras in the GTP-form (Fig. 6B)."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("exchange (ENRIQUE)", hasEventWithArguments("Exchange", List("GTP", "GDP", "Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules14")
        displayMentions(mentions, doc)
        throw e
    }
  }

    */
}
