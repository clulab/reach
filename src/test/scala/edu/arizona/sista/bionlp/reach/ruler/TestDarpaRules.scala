package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp.reach.core.RelationMention
import edu.arizona.sista.matcher._
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert._
import TestDarpaRules._


/**
 * Unit test for rules tailored for the DARPA evaluation
 * User: mihais
 * Date: 12/29/14
 */
class TestDarpaRules extends AssertionsForJUnit {
  def hasEventWithArguments(label:String, args:Seq[String], mentions:Seq[Mention]):Boolean = {
    for(m <- mentions) {
      if(m.isInstanceOf[EventMention]) {
        val em = m.asInstanceOf[EventMention]
        if(em.label == label) { // found the label
        var count = 0
          for(arg <- args) {
            for (a <- em.arguments.values.flatten) {
              if(arg == a.text) {
                count += 1
              }
            }
          }
          if(count == args.size) {
            // found all args as well

            println(s"\t==> found event mention: ${em.text}")
            return true
          }
        }
      }
    }
    false
  }

  def hasEntity(text:String, mentions:Seq[Mention]):Boolean = {
    for(m <- mentions) {
      if (m.isInstanceOf[TextBoundMention]) {
        val tm = m.asInstanceOf[TextBoundMention]
        if (tm.text == text) {
          println(s"\t==> found entity mention: ${tm.text}")
          return true
        }
      }
    }
    false
  }

  def hasEntityWithSite(text:String, site:String, mentions:Seq[Mention]):Boolean = {
    for(m <- mentions) {
      if (m.isInstanceOf[RelationMention]) {
        val rm = m.asInstanceOf[RelationMention]
        if (rm.arguments.contains("Site") &&
            contains(rm.arguments.get("Site").get, site) &&
            rm.arguments.contains("Protein") &&
            contains(rm.arguments.get("Protein").get, text)) {
          println(s"\t==> found entity mention with site: ${rm.text}")
          return true
        }
      }
    }
    false
  }

  def contains(mentions:Seq[Mention], text:String):Boolean = {
    for(m <- mentions) if(m.text == text) return true
    false
  }

  @Test def testRules1() {
    val doc = proc.annotate("As expected based on previous studies, wild- type K-Ras bound primarily 32P-GDP, while G12V-Ras bound 32P-GTP (Fig.2, A and B).")
    val mentions = extractor.extractFrom(doc)
    println("testRules1 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)
    assertTrue(hasEventWithArguments("Binding", List("K-Ras"), mentions)) // TODO: add 32P-GDP (MARCO)
    assertTrue(hasEventWithArguments("Binding", List("G12V-Ras"), mentions)) // TODO: add 32P-GTP (MARCO)
  }

  @Test def testRules2() {
    val doc = proc.annotate("Copyright notice and Disclaimer Abstract GTP loaded Ras induces multiple signaling pathways by binding to its numerous effectors such as Raf and PI3K.")
    val mentions = extractor.extractFrom(doc)
    println("testRules2 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)
    assertTrue(hasEventWithArguments("Binding", List("Raf", "PI3K", "Ras"), mentions))
  }

  @Test def testRules3() {
    val doc = proc.annotate("We hypothesized that MEK inhibition activates AKT by inhibiting ERK activity, which blocks an inhibitory threonine phosphorylation on the JM domains of EGFR and HER2, thereby increasing ERBB3 phosphorylation.")
    val mentions = extractor.extractFrom(doc)
    println("testRules3 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)

    assertTrue(hasEventWithArguments("Phosphorylation", List("EGFR"), mentions))
    assertTrue(hasEventWithArguments("Phosphorylation", List("HER2"), mentions))
    assertTrue(hasEventWithArguments("Phosphorylation", List("ERBB3"), mentions))
  }

  @Test def testRules4() {
    val doc = proc.annotate("We hypothesized that MEK inhibition activates AKT by inhibiting ERK activity, which blocks an inhibitory threonine phosphorylation on the JM domains of EGFR and HER2, thereby increasing ERBB3 phosphorylation.")
    val mentions = extractor.extractFrom(doc)
    println("testRules4 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)
    assertTrue(hasEntity("ERK", mentions))
    assertTrue(hasEntity("EGFR", mentions))
    assertTrue(hasEntity("HER2", mentions))
    assertTrue(hasEntity("ERBB3", mentions))

    assertTrue(hasEntityWithSite("EGFR", "JM domains", mentions))
    assertTrue(hasEntityWithSite("HER2", "JM domains", mentions))
  }

  @Test def testRules5() {
    val doc = proc.annotate("To test this hypothesis, we transiently transfected CHO-KI cells, which do not express ERBB receptors endogenously, with wildtype ERBB3 with either wild-type EGFR or EGFR T669A.")
    val mentions = extractor.extractFrom(doc)
    println("testRules5 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)
    assertTrue(hasEntity("ERBB receptors", mentions))
    assertTrue(hasEntity("ERBB3", mentions))
    assertTrue(hasEntity("EGFR", mentions))

    assertTrue(hasEntityWithSite("EGFR", "T669A", mentions))
  }

  @Test def testRules6() {
    val doc = proc.annotate("We observed analogous results in CHO-KI cells expressing wild-type ERBB3 in combination with wild-type or T677A mutant HER2 (Figure 6B)")
    val mentions = extractor.extractFrom(doc)
    println("testRules6 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)

    assertTrue(hasEntityWithSite("HER2", "T677A mutant", mentions))
  }

  @Test def testRules7() {
    val doc = proc.annotate("ERK negatively regulates the epidermal growth factor mediated interaction of Gab1 and the phosphatidylinositol 3-kinase.")
    val mentions = extractor.extractFrom(doc)
    println("testRules7 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)

    // TODO: this fails (MARCO)
    assertTrue(hasEventWithArguments("Binding", List("Gab1", "phosphatidylinositol 3-kinase"), mentions))
  }

  @Test def testRules8() {
    val doc = proc.annotate("Figure 3 Raf and PI3K bind more to ubiquitinated Ras than to non- ubiquitinated Ras To examine whether the binding of ubiquitinated K-Ras to Raf and PI3K inhibits or can actually enhance their kinase activity, both total G12V-K-Ras and the ubiquitinated subfraction of G12V-K-Ras were purified from cell lysates and subjected to an in vitro kinase (I.V.K.) assay (Fig. 4A).")
    val mentions = extractor.extractFrom(doc)
    println("testRules8 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)
    assertTrue(hasEventWithArguments("Binding", List("Raf", "PI3K", "K-Ras"), mentions))
  }

  @Test def testRules9() {
    val doc = proc.annotate("Figure 5 MEK inhibition blocks phosphorylation of a direct ERK target site in the conserved JM domains of EGFR and HER2 We hypothesized that the MEK/ERK pathway may suppress trans-phosphorylation of ERBB3 by directly phosphorylating the JM domains of EGFR and HER2, and that this could be a dominant MEK inhibitor induced feedback leading to AKT activation in these cancers.")
    val mentions = extractor.extractFrom(doc)
    println("testRules9 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)

    assertTrue(hasEntityWithSite("HER2", "JM domains", mentions))
    assertTrue(hasEntityWithSite("EGFR", "JM domains", mentions))

    assertTrue(hasEventWithArguments("Phosphorylation", List("EGFR"), mentions))
    assertTrue(hasEventWithArguments("Phosphorylation", List("HER2"), mentions))

  }

  @Test def testRules10() {
    val doc = proc.annotate("For example, ERK- mediated serine phosphorylation of the GAB1 adaptor has been shown to negatively regulate GAB1-PI3K binding and downstream AKT signaling")
    val mentions = extractor.extractFrom(doc)
    println("testRules10 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)

    assertTrue(hasEventWithArguments("Phosphorylation", List("GAB1 adaptor"), mentions))
    assertTrue(hasEventWithArguments("Binding", List("GAB1-PI3K"), mentions))

    // TODO: there are 2 regulations here! Whoever works on this, please add asserts for these two (MARCO or GUS)
  }

  @Test def testRules11() {
    val doc = proc.annotate("In contrast, the EGFR T669A mutant increased both basal EGFR and ERBB3 tyrosine phosphorylation that was not augmented by MEK inhibition")
    val mentions = extractor.extractFrom(doc)
    println("testRules11 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)

    assertTrue(hasEventWithArguments("Phosphorylation", List("EGFR"), mentions))
    assertTrue(hasEventWithArguments("Phosphorylation", List("ERBB3"), mentions))
  }

  @Test def testRules12() {
    val doc = proc.annotate("To address the effect of K-Ras ubiquitination on its binding to PI3K and Raf family members, either total G12V-K-Ras or the ubiquitinated subfraction of G12V-K-Ras was immunoprecipitated and the immunoprecipitates were probed with antibodies to detect associated Ras effector molecules.")
    val mentions = extractor.extractFrom(doc)
    println("testRules12 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)

    // TODO: this fails due to weird rule (GUS)
    assertTrue(hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions))
    assertTrue(hasEventWithArguments("Binding", List("PI3K", "Raf family members"), mentions))

    assertTrue(hasEntity("G12V-K-Ras", mentions))
  }

  @Test def testRules13() {
    val doc = proc.annotate("We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D), and accordingly, MEK inhibition substantially increased tyrosine phosphorylated ERBB3 levels (Figure 1A).")
    val mentions = extractor.extractFrom(doc)
    println("testRules13 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)

    // TODO: this fails, missing PI3K (MARCO)
    assertTrue(hasEventWithArguments("Binding", List("PI3K", "ERBB3"), mentions))

    // TODO: missing positive regulation of Binding, Controller: MEK. Please add assert for this (MARCO or GUS)
  }

  @Test def testRules14() {
    val doc = proc.annotate("We propose that once ubiquitination occurs on Ras at Lys147, it enhances GDP/GTP exchange of Ras and increases the fraction of Ras in the GTP-form (Fig. 6B).")
    val mentions = extractor.extractFrom(doc)
    println("testRules14 ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    RuleShell.displayMentions(mentions, doc)

    // TODO: this fails, missing Ras (DANE)
    assertTrue(hasEventWithArguments("Exchange", List("GTP", "GDP", "Ras"), mentions))
  }
}

object TestDarpaRules {
  val proc = new BioNLPProcessor

  val extractor = mkExtractor

  def mkExtractor = {
    val actions = new DarpaActions
    val entityRules = Ruler.readEntityRules
    val eventRules = Ruler.readEventRules // reads all entity/event rules for the DARPA eval
    val rules = entityRules + "\n\n" + eventRules
    new ExtractorEngine(rules, actions)
  }
}
