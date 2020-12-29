package org.clulab.reach.assembly

import java.io.File
import com.typesafe.config.ConfigFactory
import org.clulab.reach.context.ContextEngineFactory.Engine
import scala.collection.immutable.ListMap
import jline.console.ConsoleReader
import jline.console.history.FileHistory
import org.clulab.reach.display._
import org.clulab.reach.{context, ReachSystem}
import org.clulab.reach.assembly.display._


object AssemblyShell extends App {
  println("Loading ReachSystem ...")

  val config = ConfigFactory.load()

  // read configuration parameters to create a context engine
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  val contextConfig = config.getConfig("contextEngine.params").root
  val contextEngineParams: Map[String, String] = context.createContextEngineParams(contextConfig)

  // initialize ReachSystem with appropriate context engine
  var reach = new ReachSystem(contextEngineType = contextEngineType,
    contextParams = contextEngineParams)

  val history = new FileHistory(new File(System.getProperty("user.home"), ".assemblyshellhistory"))
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
        displayPrecedence(mentions)
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
    println
  }

}

