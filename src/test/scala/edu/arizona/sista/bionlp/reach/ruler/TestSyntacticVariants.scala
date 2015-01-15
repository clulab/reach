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
    val doc = proc.annotate("RasGAP is hydrolyzing GTP to GDP in Ras reactions.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectDecl1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
  }

  @Test def testHydrolysisPass1() {
    val doc = proc.annotate("Ras-GDP is hydrolyzed by 26S proteasome without ubiquitination.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectPass1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
  }

  @Test def testHydrolysisSubjNom1() {
    val doc = proc.annotate("MEK hydrolysis of Ras-GDP increased.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjNom1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
  }

  @Test def testHydrolysisObjNom1() {
    val doc = proc.annotate("Ras-GDP hydrolysis by MEK increased.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisObjNom1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
  }

  @Test def testHydrolysisSubjectRel1() {
    val doc = proc.annotate("Its many abnormal phenotypes can be rescued via Pde2, which specifically hydrolyzes Ras-GDP.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectRel1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
  }

  @Test def testHydrolysisSubjectRel2() {
    val doc = proc.annotate("Pde2, which has been found to hydrolyze Ras-GDP, activates MEK.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectRel2")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
  }

  @Test def testHydrolysisSubjectRelApposition1() {
    val doc = proc.annotate("Its many abnormal phenotypes can be rescued via overexpressing Pde2, a phosphodiesterase that specifically hydrolyzes Ras-GDP.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectRelApposition1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    }

  @Test def testHydrolysisSubjectRelApposition2() {
    val doc = proc.annotate("A main rate-controlling step in RAS is renin, an enzyme that hydrolyzes Ras-GTP to generate angiotensin I.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisSubjectRelApposition2")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
  }

  @Test def testHydrolysisObjectRel1() {
    val doc = proc.annotate("We measured transcription activation in the presence of MEK, which is hydrolyzed by CRP.")
    val mentions = extractor.extractFrom(doc)
    header("testHydrolysisObjectRel1")
    displayMentions(mentions, doc)

    assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("MEK"), mentions))
  }
}

object TestSyntacticVariants {
  val proc = new BioNLPProcessor

  val extractor = mkExtractor

  def mkExtractor = {
    val actions = new DarpaActions
    val rules = BasicRuler.readRules()
    new BasicRuler(rules, actions)
  }
}
