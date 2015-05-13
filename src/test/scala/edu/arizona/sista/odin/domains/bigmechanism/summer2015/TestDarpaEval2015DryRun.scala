package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import DarpaEvalUtils._
import TestResources.reach

/**
 * Unit test for rules tailored for the DARPA evaluation; using the dryrun corpus
 */
class TestDarpaEval2015DryRun extends AssertionsForJUnit {

  @Test def testRules1() {
    val text = "We next considered the effect of Ras monoubiquitination on GAP-mediated hydrolysis"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("ubiquitination (GUS)", hasEventWithArguments("Ubiquitination", List("Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  /**
   * TODO: black magic
  @Test def testRules2() {
    val doc = bioproc.annotate("To this end we compared the rate of GTP hydrolysis for Ras and mUbRas in the presence of the catalytic domains of two GAPs")
    val mentions = extractor.extractFrom(doc)

    try {
      // TODO: fix hasEventWithArguments to match Complex (RelationMention) with desired argument.
      assertTrue("hydrolysis with black magic (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
      assertTrue("hydrolysis with black magic (DANE)", hasEventWithArguments("Hydrolysis", List("mUbRas-GTP"), mentions))

      // TODO: can we catch the Positive_regulation by GAP here?
      //assertTrue("upregulation + black magic (MARCO/GUS)", hasPositiveRegulationByEntity("GAPs", "Hydrolysis", List("Ras-GTP"), mentions))
      //assertTrue("upregulation + black magic (MARCO/GUS)", hasPositiveRegulationByEntity("GAPs", "Hydrolysis", List("mUbRas-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules2")
        displayMentions(mentions, doc)
        throw e
    }
  }
  */

  /**
   * TODO: black magic trick
  @Test def testRules3() {
    val doc = bioproc.annotate("We observed an order of magnitude increase in the rate of GTP hydrolysis for unmodified Ras relative to the intrinsic rate of GTP hydrolysis.")
    val mentions = extractor.extractFrom(doc)

    try {
      // TODO: fix hasEventWithArguments to match Complex (RelationMention) with desired argument.
      assertTrue("hydrolysis with black magic (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules3")
        displayMentions(mentions, doc)
        throw e
    }
  }
  */

  @Test def testRules4() {
    val text = "The effects of monoubiquitination on Ras are not isoform-specific."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("ubiquitination (GUS)", hasEventWithArguments("Ubiquitination", List("Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules4")
        displayMentions(mentions, doc)
        throw e
    }
  }

  // TODO: I think Dane had a method to convert this binding to a ubiquitination...
  @Test def testRules5() {
    val text = "We measured the rate of GAP-mediated GTP hydrolysis and observed that the response of Ras ligated to Ubiquitin was identical"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("GTP"), mentions))
      // TODO: appears as binding but it's ubiquitination (GUS + MARCO)
      assertTrue("binding -> ubiquitination (MARCO/GUS)", hasEventWithArguments("Ubiquitination", List("Ras"), mentions))

      // TODO: up-regulation ( MARCO + GUS)
      assertTrue("upregulation (MARCO/GUS)", hasPositiveRegulationByEntity("GAP", "Hydrolysis", List("GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules5")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules6() {
    val text = "monoubiquitinated K-Ras is less sensitive than the unmodified protein to GAP-mediated GTP hydrolysis"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("GTP"), mentions))
      assertTrue("ubiquitination (GUS)", hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions))

      assertTrue("upregulation (MARCO/GUS)", hasPositiveRegulationByEntity("GAP", "Hydrolysis", List("GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules6")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules7() {
    val text = "Here we show that monoubiquitination decreases the sensitivity of Ras to GAP-mediated hydrolysis"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras"), mentions))

      assertTrue("ubiquitination (GUS)", hasEventWithArguments("Ubiquitination", List("Ras"), mentions))

      assertTrue("upregulation (MARCO/GUS)", hasPositiveRegulationByEntity("GAP", "Hydrolysis", List("Ras"), mentions))

      // TODO: another down-regulation controller the ubiquitination, and controlled the GAP up-regulation??? Not sure about this...
      // assertTrue(hasNegativeRegulationByEvent("Ubiquitination", List("Ras"), "Positive_regulation", List("")))
    } catch {
      case e: AssertionError =>
        header("testRules7")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules8() {
    val text = "It has recently been shown that oncogenic RAS can enhance the apoptotic function of p53 via ASPP1 and ASPP2"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("model entity (GUS)", hasEntity("RAS", mentions))
      assertTrue("model entity (GUS)", hasEntity("p53", mentions))
      assertTrue("model entity (GUS)", hasEntity("ASPP1", mentions))
      assertTrue("model entity (GUS)", hasEntity("ASPP2", mentions))
    } catch {
      case e: AssertionError =>
        header("testRules8")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules9() {
    val text = "Mechanistically ASPP1 and ASPP2 bind RAS-GTP and potentiates RAS signalling to enhance p53 mediated apoptosis"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("model entity (GUS)", hasEntity("RAS-GTP", mentions))
      assertTrue("model entity (GUS)", hasEntity("RAS", mentions))
      assertTrue("model entity (GUS)", hasEntity("p53", mentions))
      assertTrue("model entity (GUS)", hasEntity("ASPP1", mentions))
      assertTrue("model entity (GUS)", hasEntity("ASPP2", mentions))
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1", "ASPP2", "RAS-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules9")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules10() {
    val text = "Mechanistically ASPP1 and ASPP2 bind RAS-GTP and potentiates RAS signalling to enhance p53 mediated apoptosis"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("model entity (GUS)", hasEntity("RAS-GTP", mentions))
      assertTrue("model entity (GUS)", hasEntity("RAS", mentions))
      assertTrue("model entity (GUS)", hasEntity("p53", mentions))
      assertTrue("model entity (GUS)", hasEntity("ASPP1", mentions))
      assertTrue("model entity (GUS)", hasEntity("ASPP2", mentions))
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1", "ASPP2", "RAS-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules10")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules11() {
    val text = "Interestingly, we observed two conserved putative MAPK phosphorylation sites in ASPP1 and ASPP2"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("model entity (GUS)", hasEntity("MAPK", mentions))
      assertTrue("model entity (GUS)", hasEntity("ASPP1", mentions))
      assertTrue("model entity (GUS)", hasEntity("ASPP2", mentions))

      assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP1"), mentions))
      assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

      // TODO: missing regulations (MARCO + GUS)
      assertTrue("upregulation (MARCO/GUS)", hasPositiveRegulationByEntity("MAPK", "Phosphorylation", List("ASPP1"), mentions))
      assertTrue("upregulation (MARCO/GUS)", hasPositiveRegulationByEntity("MAPK", "Phosphorylation", List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules11")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules12() {
    val text = "We thus tested whether RAS activation may regulate ASPP2 phosphorylation"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

      // we don't do regulations anymore, only + and -
      // assertTrue("regulation (MARCO/GUS)", hasRegulationByEntity("Regulation", "RAS", "Phosphorylation", List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules12")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules13() {
    val text = "MAPK1 was clearly able to phosphorylate the ASPP2 fragment in vitro"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

      assertTrue("upregulation (MARCO/GUS)", hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules13")
        displayMentions(mentions, doc)
        throw e
    }
  }

  // TODO: This is failing because we're missing SAPK in "p38 SAPK"; we only get p38, but we used to get "p38 SAPK"
  @Test def testRules14() {
    val text = "Under the same conditions, ASPP2 (693-1128) fragment phosphorylated by p38 SAPK had very low levels of incorporated 32P"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("phosphorylation missing site (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

      assertTrue("upregulation (MARCO/GUS)", hasPositiveRegulationByEntity("p38 SAPK", "Phosphorylation", List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules14")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules15() {
    val text = "Indicating that p38 SAPK is not an efficient kinase for ASPP2 phosphorylation."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules15")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules16() {
    val text = "The phosphorylated ASPP2 fragment by MAPK1 was digested by trypsin and fractioned on a high performance liquid chromatography."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

      assertTrue("upregulation (MARCO/GUS)", hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules16")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules17() {
    val text = "Hence ASPP2 can be phosphorylated at serine 827 by MAPK1 in vitro."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

      assertTrue("upregulation (MARCO/GUS)", hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules17")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules18() {
    val text = "Moreover, the RAS-ASPP interaction enhances the transcription function of p53 in cancer cells."
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      // TODO: Binding with 1 argument, which is a complex (MARCO)
      //assertTrue("binding -> splitting elements of complex (MARCO/GUS)", hasEventWithArguments("Binding", List("RAS", "ASPP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules18")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules19() {
    val text = "We show here that ASPP2 is phosphorylated by the RAS/Raf/MAPK pathway and that this phosphorylation leads to its increased translocation to the cytosol/nucleus and increased binding to p53"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

      assertTrue("Translocation (ENRIQUE)", hasEventWithArguments("Translocation", List("nucleus"), mentions))
      assertTrue("Translocation (ENRIQUE)", hasEventWithArguments("Translocation", List("cytosol"), mentions))

      // TODO: incomplete Binding with 1 argument; ideally we should add ASPP2 through coref... (MARCO)
      // NOTE this is a binding with one theme only, ignore it
      // assertTrue("binding with coref (MARCO/GUS)", hasEventWithArguments("Binding", List("p53"), mentions))

      // TODO: missing two regulations:  phosphorylation leads to transport and binding
    } catch {
      case e: AssertionError =>
        header("testRules19")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testRules20() {
    val text = "ASPP2 is transported from the membrane to the nucleus/cytosol"
    val doc = reach.mkDoc(text, "testdoc")
    val mentions = reach.extractFrom(doc)

    try {
      assertTrue("Translocation (ENRIQUE)", hasEventWithArguments("Translocation", List("ASPP2", "membrane", "cytosol"), mentions))
      assertTrue("Translocation (ENRIQUE)", hasEventWithArguments("Translocation", List("ASPP2", "membrane", "nucleus"), mentions))
    } catch {
      case e: AssertionError =>
        header("testRules20")
        displayMentions(mentions, doc)
        throw e
    }
  }
}
