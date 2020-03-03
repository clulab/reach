package org.clulab.reach

import java.io.File

import jline.console.ConsoleReader
import jline.console.history.FileHistory

import scala.collection.immutable.ListMap
import com.typesafe.config.ConfigFactory
import org.clulab.processors.Processor
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.display._

object ReachShell extends App {
  println("Loading ReachSystem ...")

  val config = ConfigFactory.load()

  // create appropriate context engine with which to initialize ReachSystem
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  val contextConfig = config.getConfig("contextEngine.params").root
  val contextEngineParams: Map[String, String] = context.createContextEngineParams(contextConfig)

  // initialize ReachSystem
  val procAnnotator = new BioNLPProcessor()
  var reach = new ReachSystem(processorAnnotator = Some(procAnnotator),
                              contextEngineType = contextEngineType,
                              contextParams = contextEngineParams)

  val history = new FileHistory(new File(System.getProperty("user.home"), ".reachshellhistory"))
  sys addShutdownHook {
    history.flush() // we must flush the file before exiting
  }

  val reader = new ConsoleReader
  reader.setPrompt(">>> ")
  reader.setHistory(history)

  // use ListMap to preserve commands order
  val commands = ListMap(
    ":help" -> "show commands",
    ":exit" -> "exit system"
    //":entityrules" -> "show entity rules",
    //":modrules" -> "show modification rules",
    //":eventrules" -> "show event rules",
    //":reload" -> "reload rules"
  )

  println("\nWelcome to ReachShell!")
  printCommands()

  var running = true

  while (running) {
    reader.readLine match {
      case ":help" =>
        printCommands()

      case ":exit" | null =>
        running = false

      /*case ":reload" =>
        println("reloading rules ...")
        try {
          val rules = reload()
          reach = new ReachSystem(Some(rules), Some(proc))
          println("successfully reloaded rules")
        } catch {
          case e: Throwable => println(s"error reloading: ${e.getMessage}")
        }*/

      //case ":entityrules" =>
        //println(reach.entityRules)
        //  TODO: add rule attribute to extractors

      //case ":modrules" =>
      //  TODO: add rule attribute to extractors

      //case ":eventrules" =>
      //  TODO: add rule attribute to extractors*/

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
    val longest = commands.keys.toSeq.sortBy(_.length).last.length
    for ((cmd, msg) <- commands)
      println(s"\t$cmd${"\t"*(1 + (longest - cmd.length)/4)}=> $msg")
    println("")
  }

}
