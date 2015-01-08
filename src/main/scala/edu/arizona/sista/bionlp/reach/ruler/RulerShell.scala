package edu.arizona.sista.bionlp.reach.ruler

import jline.console.ConsoleReader
import jline.console.history.MemoryHistory
import scala.util.control.Breaks._
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

object RulerShell extends App {
  require(args.size % 2 == 0, "wrong command line args")

  val proc = new BioNLPProcessor


  def createBasicRuler:BasicRuler = {
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

  val commands = Map("%reload" -> "reload rules",
    "%help" -> "show commands",
    "%exit" -> "exit system")

  def displayCommands = commands.foreach { case (k, v) => println(s"\t$k\t=>\t$v")}

  println("Welcome to RulerShell")
  displayCommands

  breakable {
    while (true) {
      val text = reader.readLine
      text match {
        case reload if text == "%reload" => {
          println("reloading RulerShell...")
          basicRuler = createBasicRuler
        }
        case exit if text == "%exit" => break
        case help if text == "%help" => {
          println("\tCOMMANDS:")
          displayCommands
        }
        case _ => {
          val doc = proc.annotate(text)
          val mentions = basicRuler.extractFrom(doc)
          displayMentions(mentions, doc)
        }
      }
    }
  }
}
