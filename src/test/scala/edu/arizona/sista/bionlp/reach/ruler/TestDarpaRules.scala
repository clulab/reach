package edu.arizona.sista.bionlp.reach.ruler

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

  @Test def testRules1() {
    val doc = proc.annotate("As expected based on previous studies, wild- type K-Ras bound primarily 32P-GDP, while G12V-Ras bound 32P-GTP (Fig.2, A and B).")
    val mentions = extractor.extractFrom(doc)
    RuleShell.displayMentions(mentions, doc)
    assertTrue(hasEventWithArguments("Binding", List("K-Ras"), mentions)) // TODO: add 32P-GDP
    assertTrue(hasEventWithArguments("Binding", List("G12V-Ras"), mentions)) // TODO: add 32P-GTP
  }

  @Test def testRules2() {
    val doc = proc.annotate("Copyright notice and Disclaimer Abstract GTP loaded Ras induces multiple signaling pathways by binding to its numerous effectors such as Raf and PI3K.")
    val mentions = extractor.extractFrom(doc)
    RuleShell.displayMentions(mentions, doc)
    assertTrue(hasEventWithArguments("Binding", List("Raf", "PI3K", "Ras"), mentions))
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
