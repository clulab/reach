package org.clulab.reach

import com.typesafe.config.ConfigFactory
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.display._
import org.clulab.utils.CliReader
import org.clulab.utils.DefaultMenuItem
import org.clulab.utils.ExitMenuItem
import org.clulab.utils.HelpMenuItem
import org.clulab.utils.MainMenuItem
import org.clulab.utils.Menu

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

  def entityRules(menu: Menu, text: String): Boolean = {
    println(reach.entityRules)
    //  TODO: add rule attribute to extractors
    true
  }

  def modRules(menu: Menu, text: String): Boolean = {
    //  TODO: add rule attribute to extractors
    true
  }

  def eventRules(menu: Menu, text: String): Boolean = {
    //  TODO: add rule attribute to extractors*/
    true
  }

  def reload(menu: Menu, text: String): Boolean = {
    println("reloading rules ...")
    try {
//      val rules = reload()
//      reach = new ReachSystem(Some(rules), Some(proc))
//      println("successfully reloaded rules")
    }
    catch {
      case e: Throwable => println(s"error reloading: ${e.getMessage}")
    }
    true
  }

  def extract(menu: Menu, text: String): Boolean = {
    val doc = reach.mkDoc(text, "rulershell")
    val mentions = reach.extractFrom(doc)
    displayMentions(mentions, doc)
    true
  }

  val lineReader = new CliReader(">>> ", "user.home", ".reachshellhistory")
  val mainMenuItems = Seq(
    new HelpMenuItem(":help", "show commands"),
//    new MainMenuItem(":entityrules", "show entity rules", entityRules),
//    new MainMenuItem(":modrules", "show modification rules", modRules),
//    new MainMenuItem(":eventrules", "show event rules", eventRules),
//    new MainMenuItem(":reload", "reload rules", reload),
    new ExitMenuItem(":exit", "exit system")
  )
  val defaultMenuItem = new DefaultMenuItem(extract)
  val menu = new Menu("Welcome to ReachShell!", lineReader, mainMenuItems, defaultMenuItem)

  menu.run()
}
