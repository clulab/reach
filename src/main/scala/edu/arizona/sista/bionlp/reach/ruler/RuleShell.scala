package edu.arizona.sista.bionlp.reach.ruler

import scala.util.control.Breaks._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

object RuleShell extends App {
  require(args.size % 2 == 0, "wrong command line args")

  val prompt = ">>> "

  val entityRules = BasicRuler.readEntityRules

  val ruleArgIndex = args.indexOf("--rules")
  val eventRules = if (ruleArgIndex == -1) BasicRuler.readEventRules else BasicRuler.readFile(args(ruleArgIndex + 1))

  val rules = entityRules + "\n\n" + eventRules

  val actions = new DarpaActions

  val proc = new BioNLPProcessor
  val basicRuler = new BasicRuler(rules, actions)

  breakable {
    while (true) {
      val text = readLine(prompt)
      if (text == null) break
      val doc = proc.annotate(text)
      val mentions = basicRuler.extractFrom(doc)
      displayMentions(mentions, doc)
    }
  }

  println("\nbye")
}
