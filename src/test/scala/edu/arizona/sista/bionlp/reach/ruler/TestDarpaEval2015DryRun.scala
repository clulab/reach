package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp.reach.ruler.DarpaEvalUtils._
import edu.arizona.sista.bionlp.reach.ruler.TestDarpaEval2015DryRun._
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/**
 * Unit test for rules tailored for the DARPA evaluation; using the dryrun corpus
 * User: mihais
 * Date: 1/5/15
 */
class TestDarpaEval2015DryRun extends AssertionsForJUnit {

  @Test def testRules1() {
    val doc = proc.annotate("We next considered the effect of Ras monoubiquitination on GAP-mediated hydrolysis")
    val mentions = extractor.extractFrom(doc)
    header("testRules1")
    displayMentions(mentions, doc)

    assertTrue("ubiquitination (GUS)", hasEventWithArguments("Ubiquitination", List("Ras"), mentions))
  }

  @Test def testRules2() {
    val doc = proc.annotate("To this end we compared the rate of GTP hydrolysis for Ras and mUbRas in the presence of the catalytic domains of two GAPs")
    val mentions = extractor.extractFrom(doc)
    header("testRules2")
    displayMentions(mentions, doc)
    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("mUbRas-GTP"), mentions))

    // TODO: can we catch the UpRegulation by GAP here?
    //assertTrue("upregulation + black magic (MARCO/GUS)", hasUpRegulationByEntity("GAPs", "Hydrolysis", List("Ras-GTP"), mentions))
    //assertTrue("upregulation + black magic (MARCO/GUS)", hasUpRegulationByEntity("GAPs", "Hydrolysis", List("mUbRas-GTP"), mentions))
  }

  @Test def testRules3() {
    val doc = proc.annotate("We observed an order of magnitude increase in the rate of GTP hydrolysis for unmodified Ras relative to the intrinsic rate of GTP hydrolysis.")
    val mentions = extractor.extractFrom(doc)
    header("testRules3")
    displayMentions(mentions, doc)
    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
  }

  @Test def testRules4() {
    val doc = proc.annotate("The effects of monoubiquitination on Ras are not isoform-specific.")
    val mentions = extractor.extractFrom(doc)
    header("testRules4")
    displayMentions(mentions, doc)

    assertTrue("ubiquitination (GUS)", hasEventWithArguments("Ubiquitination", List("Ras"), mentions))
  }

  @Test def testRules5() {
    val doc = proc.annotate("We measured the rate of GAP-mediated GTP hydrolysis and observed that the response of Ras ligated to Ubiquitin was identical")
    val mentions = extractor.extractFrom(doc)
    header("testRules5")
    displayMentions(mentions, doc)
    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("GTP"), mentions))
    // TODO: appears as binding but it's ubiquitination (GUS + MARCO)
    assertTrue("binding -> ubiqutination (MARCO/GUS)", hasEventWithArguments("Ubiquitination", List("Ras"), mentions))

    // TODO: up-regulation ( MARCO + GUS)
    assertTrue("upregulation (MARCO/GUS)", hasUpRegulationByEntity("GAP", "Hydrolysis", List("GTP"), mentions))
  }

  @Test def testRules6() {
    val doc = proc.annotate("monoubiquitinated K-Ras is less sensitive than the unmodified protein to GAP-mediated GTP hydrolysis")
    val mentions = extractor.extractFrom(doc)
    header("testRules6")
    displayMentions(mentions, doc)
    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("GTP"), mentions))
    assertTrue("ubiquitination (GUS)", hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions))

    assertTrue("upregulation (MARCO/GUS)", hasUpRegulationByEntity("GAP", "Hydrolysis", List("GTP"), mentions))
  }

  @Test def testRules7() {
    val doc = proc.annotate("Here we show that monoubiquitination decreases the sensitivity of Ras to GAP-mediated hydrolysis")
    val mentions = extractor.extractFrom(doc)
    header("testRules7")
    displayMentions(mentions, doc)
    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras"), mentions))

    // TODO: missing keyword, missing protein - needs coref (GUS)
    assertTrue("ubiquitination +coref (GUS/DANE)", hasEventWithArguments("Ubiquitination", List("Ras"), mentions))

    assertTrue("upregulation (MARCO/GUS)", hasUpRegulationByEntity("GAP", "Hydrolysis", List("GTP"), mentions))

    // TODO: another down-regulation controller the ubiquitination, and controlled the GAP up-regulation??? Not sure about this...
    // assertTrue(hasDownRegulationByEvent("Ubiquitination", List("Ras"), "UpRegulation", List("")))
  }

  @Test def testRules8() {
    val doc = proc.annotate("It has recently been shown that oncogenic RAS can enhance the apoptotic function of p53 via ASPP1 and ASPP2")
    val mentions = extractor.extractFrom(doc)
    header("testRules8")
    displayMentions(mentions, doc)

    assertTrue("model entity (GUS)", hasEntity("RAS", mentions))
    assertTrue("model entity (GUS)", hasEntity("p53", mentions))
    assertTrue("model entity (GUS)", hasEntity("ASPP1", mentions))
    assertTrue("model entity (GUS)", hasEntity("ASPP2", mentions))
  }

  @Test def testRules9() {
    val doc = proc.annotate("Mechanistically ASPP1 and ASPP2 bind RAS-GTP and potentiates RAS signalling to enhance p53 mediated apoptosis")
    val mentions = extractor.extractFrom(doc)
    header("testRules9")
    displayMentions(mentions, doc)

    assertTrue("model entity (GUS)", hasEntity("RAS-GTP", mentions))
    assertTrue("model entity (GUS)", hasEntity("RAS", mentions))
    assertTrue("model entity (GUS)", hasEntity("p53", mentions))
    assertTrue("model entity (GUS)", hasEntity("ASPP1", mentions))
    assertTrue("model entity (GUS)", hasEntity("ASPP2", mentions))
    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1", "ASPP2", "RAS-GTP"), mentions))
  }

  @Test def testRules10() {
    val doc = proc.annotate("Mechanistically ASPP1 and ASPP2 bind RAS-GTP and potentiates RAS signalling to enhance p53 mediated apoptosis")
    val mentions = extractor.extractFrom(doc)
    header("testRules10")
    displayMentions(mentions, doc)

    assertTrue("model entity (GUS)", hasEntity("RAS-GTP", mentions))
    assertTrue("model entity (GUS)", hasEntity("RAS", mentions))
    assertTrue("model entity (GUS)", hasEntity("p53", mentions))
    assertTrue("model entity (GUS)", hasEntity("ASPP1", mentions))
    assertTrue("model entity (GUS)", hasEntity("ASPP2", mentions))
    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1", "ASPP2", "RAS-GTP"), mentions))
  }

  @Test def testRules11() {
    val doc = proc.annotate("Interestingly, we observed two conserved putative MAPK phosphorylation sites in ASPP1 and ASPP2")
    val mentions = extractor.extractFrom(doc)
    header("testRules11")
    displayMentions(mentions, doc)

    assertTrue("model entity (GUS)", hasEntity("MAPK", mentions))
    assertTrue("model entity (GUS)", hasEntity("ASPP1", mentions))
    assertTrue("model entity (GUS)", hasEntity("ASPP2", mentions))

    assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP1"), mentions))
    assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

    // TODO: missing regulations (MARCO + GUS)
    assertTrue("upregulation (MARCO/GUS)", hasUpRegulationByEntity("MAPK", "Phosphorylation", List("ASPP1"), mentions))
    assertTrue("upregulation (MARCO/GUS)", hasUpRegulationByEntity("MAPK", "Phosphorylation", List("ASPP2"), mentions))
  }

  @Test def testRules12() {
    val doc = proc.annotate("We thus tested whether RAS activation may regulate ASPP2 phosphorylation")
    val mentions = extractor.extractFrom(doc)
    header("testRules12")
    displayMentions(mentions, doc)

    assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

    assertTrue("regulation (MARCO/GUS)", hasRegulationByEntity("Regulation", "RAS", "Phosphorylation", List("ASPP2"), mentions))
  }

  @Test def testRules13() {
    val doc = proc.annotate("MAPK1 was clearly able to phosphorylate the ASPP2 fragment in vitro")
    val mentions = extractor.extractFrom(doc)
    header("testRules13")
    displayMentions(mentions, doc)

    assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

    assertTrue("upregulation (MARCO/GUS)", hasUpRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions))
  }

  @Test def testRules14() {
    val doc = proc.annotate("Under the same conditions, ASPP2 (693-1128) fragment phosphorylated by p38 SAPK had very low levels of incorporated 32P")
    val mentions = extractor.extractFrom(doc)
    header("testRules14")
    displayMentions(mentions, doc)
    
    assertTrue("phosphorylation missing site (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

    assertTrue("upregulation (MARCO/GUS)", hasUpRegulationByEntity("p38 SAPK", "Phosphorylation", List("ASPP2"), mentions))
  }

  @Test def testRules15() {
    val doc = proc.annotate("Indicating that p38 SAPK is not an efficient kinase for ASPP2 phosphorylation.")
    val mentions = extractor.extractFrom(doc)
    header("testRules15")
    displayMentions(mentions, doc)

    assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))
  }

  @Test def testRules16() {
    val doc = proc.annotate("The phosphorylated ASPP2 fragment by MAPK1 was digested by trypsin and fractioned on a high performance liquid chromatography.")
    val mentions = extractor.extractFrom(doc)
    header("testRules16")
    displayMentions(mentions, doc)

    assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

    assertTrue("upregulation (MARCO/GUS)", hasUpRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions))
  }

  @Test def testRules17() {
    val doc = proc.annotate("Hence ASPP2 can be phosphorylated at serine 827 by MAPK1 in vitro.")
    val mentions = extractor.extractFrom(doc)
    header("testRules17")
    displayMentions(mentions, doc)

    assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

    assertTrue("upregulation (MARCO/GUS)", hasUpRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions))
  }

  @Test def testRules18() {
    val doc = proc.annotate("Moreover, the RAS-ASPP interaction enhances the transcription function of p53 in cancer cells.")
    val mentions = extractor.extractFrom(doc)
    header("testRules18")
    displayMentions(mentions, doc)

    // TODO: Binding with 1 argument, which is a complex (MARCO)
    //assertTrue("binding -> splitting elements of complex (MARCO/GUS)", hasEventWithArguments("Binding", List("RAS", "ASPP"), mentions))
  }

  @Test def testRules19() {
    val doc = proc.annotate("We show here that ASPP2 is phosphorylated by the RAS/Raf/MAPK pathway and that this phosphorylation leads to its increased translocation to the cytosol/nucleus and increased binding to p53")
    val mentions = extractor.extractFrom(doc)
    header("testRules19")
    displayMentions(mentions, doc)

    assertTrue("phosphorylation (GUS)", hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions))

    assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("nucleus"), mentions))
    assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("cytosol"), mentions))

    // TODO: incomplete Binding with 1 argument; ideally we should add ASPP2 through coref... (MARCO)
    assertTrue("binding with coref (MARCO/GUS)", hasEventWithArguments("Binding", List("p53"), mentions))

    // TODO: missing two regulations:  phosphorylation leads to transport and binding
  }

  @Test def testRules20() {
    val doc = proc.annotate("ASPP2 is transported from the membrane to the nucleus/cytosol")
    val mentions = extractor.extractFrom(doc)
    header("testRules19")
    displayMentions(mentions, doc)

    assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("ASPP2", "membrane", "cytosol"), mentions))
    assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("ASPP2", "membrane", "nucleus"), mentions))
  }
}

object TestDarpaEval2015DryRun {
  val proc = new BioNLPProcessor

  val extractor = mkExtractor

  def mkExtractor = {
    val actions = new DarpaActions
    val rules = BasicRuler.readRules
    new BasicRuler(rules, actions)
  }
}
