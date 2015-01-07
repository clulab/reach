package edu.arizona.sista.bionlp.reach.ruler

import jline.console.ConsoleReader
import jline.console.history.MemoryHistory
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

object RulerShell extends App {
  require(args.size % 2 == 0, "wrong command line args")

  val entityRules = BasicRuler.readEntityRules
  val ruleArgIndex = args.indexOf("--rules")
  val eventRules = if (ruleArgIndex == -1) BasicRuler.readEventRules else BasicRuler.readFile(args(ruleArgIndex + 1))
  val rules = entityRules + "\n\n" + eventRules
  val actions = new DarpaActions
  val proc = new BioNLPProcessor
  val basicRuler = new BasicRuler(rules, actions)

  val reader = new ConsoleReader
  reader.setPrompt(">>> ")
  reader.setHistory(new MemoryHistory)

  println("Welcome to RulerShell")
  println("Press ctrl-c to exit")

  while (true) {
    val text = reader.readLine
    if (text != null) {
      val doc = proc.annotate(text)
      val mentions = basicRuler.extractFrom(doc)
      displayMentions(mentions, doc)
    }
  }
}
