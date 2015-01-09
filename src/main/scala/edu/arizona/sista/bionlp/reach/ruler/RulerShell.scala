package edu.arizona.sista.bionlp.reach.ruler

import jline.console.ConsoleReader
import jline.console.history.MemoryHistory
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

object RulerShell extends App {
  require(args.size % 2 == 0, "wrong command line args")

  val proc = new BioNLPProcessor

  def createBasicRuler: BasicRuler = {
    val entityRules = BasicRuler.readEntityRules(shell=true)
    val ruleArgIndex = args.indexOf("--rules")
    val eventRules = if (ruleArgIndex == -1) BasicRuler.readEventRules(shell=true) else BasicRuler.readFile(args(ruleArgIndex + 1))
    val rules = entityRules + "\n\n" + eventRules
    val actions = new DarpaActions
    new BasicRuler(rules, actions)
  }

  var basicRuler: BasicRuler = createBasicRuler

  val reader = new ConsoleReader
  reader.setPrompt(">>> ")
  reader.setHistory(new MemoryHistory)

  val commands = Map(
    "%reload" -> "reload rules",
    "%help" -> "show commands",
    "%exit" -> "exit system"
  )

  def displayCommands() = commands foreach {
    case (k, v) => println(s"\t$k\t=>\t$v")
  }

  println("Welcome to RulerShell")
  displayCommands()

  var running = true

  while (running) {
    reader.readLine match {
      case "%reload" =>
        println("reloading RulerShell...")
        try {
          basicRuler = createBasicRuler
        } catch {
          case e: Throwable => println("Error reloading RulerShell. Please check your rules and try again.")
        }

      case "%exit" =>
        running = false

      case "%help" =>
        println("\tCOMMANDS:")
        displayCommands()

      case null =>
        println("\nPlease type %exit to quit.")

      case text =>
        val doc = proc.annotate(text)
        val mentions = basicRuler.extractFrom(doc)
        displayMentions(mentions, doc)
    }
  }
}
