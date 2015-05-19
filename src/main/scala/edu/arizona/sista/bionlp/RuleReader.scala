package edu.arizona.sista.bionlp

import java.io.File


/**
 * Utilities to read rule files
 */
object RuleReader {

  case class Rules(entities: String, modifications: String, events: String)

  val resourcesDir = "/edu/arizona/sista/odin/domains/bigmechanism/summer2015/biogrammar"
  val entitiesDir = s"$resourcesDir/entities"
  val modificationsDir = s"$resourcesDir/modifications"
  val eventsDir = s"$resourcesDir/events"
  // For shell
  val resourcesPath = s"src/main/resources$resourcesDir"

  def readRules(): String =
    readEntityRules() + "\n\n" + readModificationRules() + "\n\n" + readEventRules()

  def readEntityRules(): String = {
    val files = Seq(
      s"$entitiesDir/entities.yml")
    files map readResource mkString "\n\n"
  }

  def readModificationRules(): String =
    readResource(s"$modificationsDir/modifications.yml")

  def readEventRules(): String = {
    val files = Seq(
      s"$eventsDir/phospho_events.yml",
      s"$eventsDir/ubiq_events.yml",
      s"$eventsDir/hydrox_events.yml",
      s"$eventsDir/hydrolysis_events.yml",
      s"$eventsDir/bind_events.yml",
      s"$eventsDir/transcription_events.yml",
      s"$eventsDir/neg_reg_events.yml",
      s"$eventsDir/pos_reg_events.yml",
      s"$eventsDir/translocation_events.yml",
      s"$eventsDir/pos_activation_events.yml",
      s"$eventsDir/neg_activation_events.yml")
    files map readResource mkString "\n\n"
  }

  def readFile(filename: String) = {
    val source = io.Source.fromFile(filename)
    val data = source.mkString
    source.close()
    data
  }

  def readRuleFilesFromDir(file: File):String = {
    val ruleFiles =
      file.listFiles
      .filter(f => f.getName.endsWith(".yml"))
    val rules =
      ruleFiles
        .map{case f:File => readFile(f.getAbsolutePath)}
        .mkString("\n")
    rules + "\n"
  }

  def readResource(filename: String): String = {
    val source = io.Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }

  /**
   * Reads rules from for entities, modifications, and events
   * @return Rule instance
   */
  def reloadRules(): Rules  = {

    val entitiesPath = s"$resourcesPath/entities"
    val modificationsPath = s"$resourcesPath/modifications"
    val eventsPath = s"$resourcesPath/events"

    val entitiesDir = new File(".", entitiesPath)
    val modificationsDir = new File(".", modificationsPath)
    val eventsDir = new File(".", eventsPath)
    println(s"\tentities are here: ${entitiesDir.getAbsolutePath}")
    println(s"\tmodifications are here: ${modificationsDir.getAbsolutePath}")
    println(s"\tevents are here: ${eventsDir.getAbsolutePath}")
    val entityRules = readRuleFilesFromDir(entitiesDir)
    val modificationRules = readRuleFilesFromDir(modificationsDir)
    val eventRules = readRuleFilesFromDir(eventsDir)
    Rules(entityRules, modificationRules, eventRules)
  }
}