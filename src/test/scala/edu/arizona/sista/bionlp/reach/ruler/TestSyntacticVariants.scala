package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp.reach.ruler.DarpaEvalUtils._
import edu.arizona.sista.bionlp.reach.ruler.TestSyntacticVariants._
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import org.junit.Assert._
import org.junit.Test

/**
 * Created by dane on 1/14/15.
 * Testing for common syntactic variations on rules, e.g. passive voice, relative clauses, etc.
 */
class TestSyntacticVariants {

  @Test def testHydrolysisDecl1() {
    val doc = bioproc.annotate("RasGAP is hydrolyzing GTP to GDP in Ras reactions.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectDecl1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
  }

  @Test def testHydrolysisPass1() {
    val doc = bioproc.annotate("Ras-GDP is hydrolyzed by 26S proteasome without ubiquitination.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectPass1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
  }

  @Test def testHydrolysisSubjNom1() {
    val doc = bioproc.annotate("MEK hydrolysis of Ras-GDP increased.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjNom1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
  }

  @Test def testHydrolysisObjNom1() {
    val doc = bioproc.annotate("Ras-GDP hydrolysis by MEK increased.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisObjNom1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
  }

  @Test def testHydrolysisSubjectRel1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via Pde2, which specifically hydrolyzes Ras-GDP.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectRel1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
  }

  @Test def testHydrolysisSubjectRel2() {
    val doc = bioproc.annotate("Pde2, which has been found to hydrolyze Ras-GDP, activates MEK.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectRel2")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
  }

  @Test def testHydrolysisSubjectRelApposition1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via overexpressing Pde2, a phosphodiesterase that specifically hydrolyzes Ras-GDP.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectRelApposition1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    }

  @Test def testHydrolysisSubjectRelApposition2() {
    val doc = bioproc.annotate("A main rate-controlling step in RAS is renin, an enzyme that hydrolyzes Ras-GTP to generate angiotensin I.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectRelApposition2")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
  }

  @Test def testHydrolysisObjectRel1() {
    val doc = bioproc.annotate("We measured transcription activation in the presence of MEK, which is hydrolyzed by CRP.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisObjectRel1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("MEK"), mentions))
  }

  @Test def testBindingDecl1() {
    val doc = bioproc.annotate("Mechanistically, ASPP1 and ASPP2 bind RAS-GTP.")
    val mentions = extractor.extractFrom(doc)
    header("testBindingDecl1")
    displayMentions(mentions, doc)

    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1, ASPP2, RAS-GTP"), mentions))
  }

  @Test def testBindingDecl2() {
    val doc = bioproc.annotate("Mechanistically, ASPP1 and ASPP2 bind with RAS-GTP.")
    val mentions = extractor.extractFrom(doc)
    header("testBindingDecl2")
    displayMentions(mentions, doc)

    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1, ASPP2, RAS-GTP"), mentions))
  }

  @Test def testBindingPass1() {
    val doc = bioproc.annotate("Mechanistically, ASPP1 and ASPP2 are bound by RAS-GTP.")
    val mentions = extractor.extractFrom(doc)
    header("testBindingPass1")
    displayMentions(mentions, doc)

    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1, ASPP2, RAS-GTP"), mentions))
  }

  @Test def testBindingPrepNom1() {
    val doc = bioproc.annotate("We detected elevated binding of p53 to K-Ras.")
    val mentions = extractor.extractFrom(doc)
    header("testBindingPrepNom1")
    displayMentions(mentions, doc)

    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53","K-Ras"), mentions))
  }

  @Test def testBindingPrepNom2() {
    val doc = bioproc.annotate("We detected elevated binding of p53 and K-Ras.")
    val mentions = extractor.extractFrom(doc)
    header("testBindingPrepNom2")
    displayMentions(mentions, doc)

    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53","K-Ras"), mentions))
  }

  @Test def testBindingPrepNom3() {
    val doc = bioproc.annotate("We detected elevated binding of p53 with K-Ras.")
    val mentions = extractor.extractFrom(doc)
    header("testBindingPrepNom3")
    displayMentions(mentions, doc)

    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53","K-Ras"), mentions))
  }

  @Test def testBindingSubjNom1() {
    val doc = bioproc.annotate("We detected elevated p53 binding to K-Ras.")
    val mentions = extractor.extractFrom(doc)
    header("testBindingSubjNom1")
    displayMentions(mentions, doc)

    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53","K-Ras"), mentions))
  }

  @Test def testBindingObjNom1() {
    val doc = bioproc.annotate("We detected elevated K-Ras binding by p53.")
    val mentions = extractor.extractFrom(doc)
    header("testBindingObjNom1")
    displayMentions(mentions, doc)

    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53","K-Ras"), mentions))
  }

  @Test def testBindingSubjRel1() {
    val doc = bioproc.annotate("We detected elevated phosphorylation of K-Ras, a protein that subsequently binds p53.")
    val mentions = extractor.extractFrom(doc)
    header("testBindingSubjRel1")
    displayMentions(mentions, doc)

    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53","K-Ras"), mentions))
  }

  @Test def testBindingObjRel1() {
    val doc = bioproc.annotate("We detected elevated phosphorylation of K-Ras, a protein that is subsequently bound by p53.")
    val mentions = extractor.extractFrom(doc)
    header("testBindingObjRel1")
    displayMentions(mentions, doc)

    assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53","K-Ras"), mentions))
  }

  @Test def testTransport1() {
    val doc = bioproc.annotate("Phosphorylation leads the plasma membrane to release p53 to the cytosol.")
    val mentions = extractor.extractFrom(doc)
    header("testTransport1")
    displayMentions(mentions, doc)

    assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("p53", "plasma membrane", "cytosol"), mentions))
  }

  @Test def testTransport2() {
    val doc = bioproc.annotate("Recruitment of p53 from the cytosol to the plasma membrane increases with phosphorylation.")
    val mentions = extractor.extractFrom(doc)
    header("testTransport2")
    displayMentions(mentions, doc)

    assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("p53", "plasma membrane", "cytosol"), mentions))
  }

  @Test def testTransport3() {
    val doc = bioproc.annotate("With increased phosphorylation, p53 is exported from the plasma membrane to the cytosol.")
    val mentions = extractor.extractFrom(doc)
    header("testTransport3")
    displayMentions(mentions, doc)

    assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("p53", "plasma membrane", "cytosol"), mentions))
  }

  @Test def testTransport4() {
    val doc = bioproc.annotate("ASPP2, a protein which is transported from the membrane to the nucleus, is subsequently phosphorylated.")
    val mentions = extractor.extractFrom(doc)
    header("testTransport4")
    displayMentions(mentions, doc)

    assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("ASPP2", "membrane", "nucleus"), mentions))
  }

  @Test def testTransport5() {
    val doc = bioproc.annotate("ASPP2, a protein which translocates Pde2 from the membrane to the nucleus, is subsequently phosphorylated.")
    val mentions = extractor.extractFrom(doc)
    header("testTransport4")
    displayMentions(mentions, doc)

    assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("Pde2", "membrane", "nucleus"), mentions))
  }
}

object TestSyntacticVariants {

  val extractor = mkExtractor

  def mkExtractor = {
    val actions = new DarpaActions
    val rules = BasicRuler.readRules()
    new BasicRuler(rules, actions)
  }
}
