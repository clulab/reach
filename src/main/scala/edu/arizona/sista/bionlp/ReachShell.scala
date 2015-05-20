package edu.arizona.sista.bionlp

import java.io.File
import jline.console.ConsoleReader
import jline.console.history.FileHistory
import edu.arizona.sista.bionlp.display._
import RuleReader._

object ReachShell extends App {

  println("Loading ReachSystem ...")
  var reach = new ReachSystem
  val proc = reach.processor

  val history = new FileHistory(new File(System.getProperty("user.home"), ".reachshellhistory"))
  sys addShutdownHook {
    history.flush() // we must flush the file before exiting
  }

  val reader = new ConsoleReader
  reader.setPrompt(">>> ")
  reader.setHistory(history)

  val commands = Map(
    ":help" -> "show commands",
    ":exit" -> "exit system",
    ":reload" -> "reload rules"
  )

  println(s"\nWelcome to ReachShell!")
  printCommands()

  var running = true

  while (running) {
    reader.readLine match {
      case ":help" =>
        printCommands()

      case ":exit" | null =>
        running = false

      case ":reload" =>
        println(s"reloading rules...")
        try {
          val rules = reloadRules()
          reach = new ReachSystem(Some(rules), Some(proc))
          println("successfully reloaded rules")
        } catch {
          case e: Exception => println(s"Error reloading ReachShell: ${e.getMessage}")
        }

      case text =>
        val doc = reach.mkDoc(text, "rulershell")
        val mentions = reach.extractFrom(doc)
        displayMentions(mentions, doc)
    }
  }

  // manual terminal cleanup
  reader.getTerminal().restore()
  reader.shutdown()


  // functions

  def printCommands(): Unit = {
    println("\nCOMMANDS:")
    for ((cmd, msg) <- commands)
      println(s"\t$cmd\t=> $msg")
    println
  }
}

