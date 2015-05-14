package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import TestResources.reach
import DarpaEvalUtils._

/**
 * Unit test for rules tailored for the DARPA evaluation; using the training corpus
 */
class TestDarpaEval2015Training extends AssertionsForJUnit {

  @Test def testRules1() {
    val text = "As expected based on previous studies, wild- type K-Ras bound primarily 32P-GDP, while G12V-Ras bound 32P-GTP (Fig.2, A and B)."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("K-Ras", "32P-GDP"), mentions))
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("G12V-Ras", "32P-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules2() {
    val text = "GTP loaded Ras induces multiple signaling pathways by binding to its numerous effectors such as Raf and PI3K."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("Raf", "PI3K", "Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules4() {
    val text = "We hypothesized that MEK inhibition activates AKT by inhibiting ERK activity, which blocks an inhibitory threonine phosphorylation on the JM domains of EGFR and HER2, thereby increasing ERBB3 phosphorylation."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("model entity (GUS)", hasEntity("ERK", mentions))
      assertTrue("model entity (GUS)", hasEntity("EGFR", mentions))
      assertTrue("model entity (GUS)", hasEntity("HER2", mentions))
      assertTrue("model entity (GUS)", hasEntity("ERBB3", mentions))
    } catch {
      case e: AssertionError =>
        header("testRules4")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules5() {
    val text = "To test this hypothesis, we transiently transfected CHO-KI cells, which do not express ERBB receptors endogenously, with wildtype ERBB3 with either wild-type EGFR or EGFR T669A."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("model entity (GUS)", hasEntity("ERBB receptors", mentions))
      assertTrue("model entity (GUS)", hasEntity("ERBB3", mentions))
      assertTrue("model entity (GUS)", hasEntity("EGFR", mentions))
    } catch {
      case e: AssertionError =>
        header("testRules5")
        displayMentions(mentions, doc)
        throw e
    }
  }

  /**
  @Test def testRules6() {
    val doc = bioproc.annotate("ERK negatively regulates the epidermal growth factor mediated interaction of Gab1 and the phosphatidylinositol 3-kinase.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("Gab1", "phosphatidylinositol 3-kinase"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules7")
        displayMentions(mentions, doc)
        throw e
    }
  }**/

  @Test def testRules7() {
    val text = "Raf and PI3K bind more to ubiquitinated Ras than to non- ubiquitinated Ras To examine whether the binding of ubiquitinated K-Ras to Raf and PI3K inhibits or can actually enhance their kinase activity, both total G12V-K-Ras and the ubiquitinated subfraction of G12V-K-Ras were purified from cell lysates and subjected to an in vitro kinase (I.V.K.) assay (Fig. 4A)."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("Raf", "PI3K", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules8")
        displayMentions(mentions, doc)
        throw e
    }
  }
  /**
  @Test def testRules8() {
    val doc = bioproc.annotate("Figure 5 MEK inhibition blocks phosphorylation of a direct ERK target site in the conserved JM domains of EGFR and HER2 We hypothesized that the MEK/ERK pathway may suppress trans-phosphorylation of ERBB3 by directly phosphorylating the JM domains of EGFR and HER2, and that this could be a dominant MEK inhibitor induced feedback leading to AKT activation in these cancers.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("entity with site (GUS)", hasEntityWithSite("HER2", "JM domains", mentions))
      assertTrue("entity with site (GUS)", hasEntityWithSite("EGFR", "JM domains", mentions))

      assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("EGFR"), mentions))
      assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("HER2"), mentions))

    } catch {
      case e: AssertionError =>
        header("testRules9")
        displayMentions(mentions, doc)
        throw e
    }
  }**/
  /**
  @Test def testRules9() {
    val doc = bioproc.annotate("For example, ERK- mediated serine phosphorylation of the GAB1 adaptor has been shown to negatively regulate GAB1-PI3K binding and downstream AKT signaling")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("phosphorylation (BANNER sometimes fails here, guys...)", hasEventWithArguments("Phosphorylation", List("GAB1 adaptor"), mentions))

      // NOTE this test is disabled because we don't do bindings with a single participant
      // assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("GAB1-PI3K"), mentions))

      // TODO: there are 2 regulations here! Whoever works on this, please add asserts for these two (MARCO or GUS)
      assertTrue("upregulation (MARCO/GUS)", hasPositiveRegulationByEntity("ERK-", "Phosphorylation", List("serine", "GAB1 adaptor"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules10")
        displayMentions(mentions, doc)
        throw e
    }
  }**/


  @Test def testRules11() {
    val text = "To address the effect of K-Ras ubiquitination on its binding to PI3K and Raf family members, either total G12V-K-Ras or the ubiquitinated subfraction of G12V-K-Ras was immunoprecipitated and the immunoprecipitates were probed with antibodies to detect associated Ras effector molecules."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("ubiquitination (GUS)", hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions))
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("PI3K", "Raf"), mentions))

      assertTrue("model entity (GUS)", hasEntity("G12V-K-Ras", mentions))
    } catch {
      case e: AssertionError =>
        header("testRules12")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules12() {
    val text = "We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D), and accordingly, MEK inhibition substantially increased tyrosine phosphorylated ERBB3 levels (Figure 1A)."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("PI3K", "ERBB3"), mentions))

      assertTrue("upregulation (MARCO or GUS)", hasPositiveRegulationByEntity("MEK", "Binding", List("PI3K", "ERBB3"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules13")
        displayMentions(mentions, doc)
        throw e
    }
  }

}
