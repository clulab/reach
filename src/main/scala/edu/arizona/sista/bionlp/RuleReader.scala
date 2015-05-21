package edu.arizona.sista.bionlp

import java.io.File
import edu.arizona.sista.bionlp.reach.preprocessing.TemplateMap


/**
 * Utilities to read rule files
 */
object RuleReader {

  case class Rules(entities: String, modifications: String, events: String, coref: String)

  val resourcesDir = "/edu/arizona/sista/odin/domains/bigmechanism/summer2015/biogrammar"
  val templatesDir = s"$resourcesDir/templates"
  val entitiesDir = s"$resourcesDir/entities"
  val modificationsDir = s"$resourcesDir/modifications"
  val eventsDir = s"$resourcesDir/events"
  val corefDir = s"$resourcesDir/coref"
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
      // non-templatic event grammars
      s"$eventsDir/hydrolysis_events.yml",
      s"$eventsDir/bind_events.yml",
      s"$eventsDir/transcription_events.yml",
      s"$eventsDir/neg_reg_events.yml",
      s"$eventsDir/pos_reg_events.yml",
      s"$eventsDir/translocation_events.yml",
      s"$eventsDir/pos_activation_events.yml",
      s"$eventsDir/neg_activation_events.yml")

    val ruleFiles = files map readResource mkString "\n\n"
    // Generate rules for templatic events
    val simpleEventTemplate = readResource(s"$templatesDir/simple-event_template.yml")
    val templaticEventRules = generateRulesFromTemplate(simpleEventTemplate, simpleEventMap)
    // println(templaticEventRules)
    ruleFiles + templaticEventRules
  }

  def readCorefRules(): String = {
    val files = Seq(
      // non-templatic grammars plus generics like "it" and "the protein"
      s"$corefDir/generic_entities.yml",
      s"$corefDir/generic_events.yml",
      s"$eventsDir/hydrolysis_events.yml",
      s"$eventsDir/bind_events.yml",
      s"$eventsDir/transcription_events.yml",
      s"$eventsDir/neg_reg_events.yml",
      s"$eventsDir/pos_reg_events.yml",
      s"$eventsDir/translocation_events.yml",
      s"$eventsDir/pos_activation_events.yml",
      s"$eventsDir/neg_activation_events.yml")

    val ruleFiles = files map readResource mkString "\n\n"
    // Generate rules for templatic events
    val simpleEventTemplate = readResource(s"$templatesDir/simple-event_template.yml")
    val templaticEventRules = generateRulesFromTemplate(simpleEventTemplate, simpleEventMap)
    // println(templaticEventRules)
    ruleFiles + templaticEventRules
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
    val templatesPath = s"$resourcesPath/templates"
    val corefPath = s"$resourcesPath/coref"

    val entitiesDir = new File(".", entitiesPath)
    val modificationsDir = new File(".", modificationsPath)
    val eventsDir = new File(".", eventsPath)
    val templatesDir = new File(".", templatesPath)
    val corefDir = new File(".", corefPath)

    println(s"\tentities\t=> ${entitiesDir.getCanonicalPath}")
    println(s"\tmodifications\t=> ${modificationsDir.getCanonicalPath}")
    println(s"\tevents\t\t=> ${eventsDir.getCanonicalPath}")
    println(s"\ttemplates\t=> ${templatesDir.getCanonicalPath}")
    println(s"\tcoref\t=> ${corefDir.getCanonicalPath}")

    // FIXME Could be dangerous since it will slurp up all the templates
    val simpleEventTemplate = readRuleFilesFromDir(templatesDir)
    val templaticEvents = generateRulesFromTemplate(simpleEventTemplate, simpleEventMap)
    val entityRules = readRuleFilesFromDir(entitiesDir)
    val modificationRules = readRuleFilesFromDir(modificationsDir)
    val eventRules = readRuleFilesFromDir(eventsDir) + templaticEvents
    val corefRules = eventRules + readRuleFilesFromDir(corefDir)
    Rules(entityRules, modificationRules, eventRules, corefRules)
  }

  /** Replaces rules variables.
    * 
    * @param rules A string with variables to replace
    * @param variables a map of (name -> value)
    * @return a string with the new text
    */
  def replaceVars(rules: String, variables: TemplateMap): String = {
    var text = rules
    for (name <- variables.keys)
      text = s"\\{\\{\\s*($name)\\s*\\}\\}".r.replaceAllIn(text, m => variables(m.group(1)))
    text
  }

  def generateRulesFromTemplate(template: String, varMap:Map[String, TemplateMap]):String = {
    varMap.values.map(m => replaceVars(template, m)) mkString "\n\n"
  }

  // Phosphorylation
  val phosphoMap: Map[String, String] =
    Map("eventName" -> "Phosphorylation",
        "actionFlow" -> "default",
        "labels" -> "Phosphorylation",
        "verbalTriggerLemma" -> "phosphorylate",
        "nominalTriggerLemma" -> "phosphorylation")

  // Ubiquitination
  val ubiqMap: Map[String, String] =
    Map("eventName" -> "Ubiquitination",
        "actionFlow" -> "mkUbiquitination",
        "labels" -> "Ubiquitination",
        "verbalTriggerLemma" -> "ubiquitinate",
        "nominalTriggerLemma" -> "ubiquitination")

  // Hydroxylation
  val hydroxMap: Map[String, String] =
    Map("eventName" -> "Hydroxylation",
        "actionFlow" -> "default",
        "labels" -> "Hydroxylation",
        "verbalTriggerLemma" -> "hydroxylate",
        "nominalTriggerLemma" -> "hydroxylation")

  // Sumosylation
  val sumoMap: Map[String, String] =
    Map("eventName" -> "Sumosylation",
        "actionFlow" -> "default",
        "labels" -> "Sumosylation",
        "verbalTriggerLemma" -> "sumosylate",
        "nominalTriggerLemma" -> "sumosylation")

  // Glycosylation
  val glycoMap: Map[String, String] =
    Map("eventName" -> "Glycosylation",
        "actionFlow" -> "default",
        "labels" -> "Glycosylation",
        "verbalTriggerLemma" -> "glycosylate",
        "nominalTriggerLemma" -> "glycosylation")

  // Acetylation
  val aceMap: Map[String, String] =
    Map("eventName" -> "Acetylation",
        "actionFlow" -> "default",
        "labels" -> "Acetylation",
        "verbalTriggerLemma" -> "acetylate",
        "nominalTriggerLemma" -> "acetylation")

  // Farnesylation
  val farneMap: Map[String, String] =
    Map("eventName" -> "Farnesylation",
      "actionFlow" -> "default",
      "labels" -> "Farnesylation",
      "verbalTriggerLemma" -> "farnesylate",
      "nominalTriggerLemma" -> "farnesylation")

  // Ribosylation
  val riboMap: Map[String, String] =
    Map("eventName" -> "Ribosylation",
        "actionFlow" -> "default",
        "labels" -> "Ribosylation",
        "verbalTriggerLemma" -> "ribosylate",
        "nominalTriggerLemma" -> "ribosylation")

  // Methylation
  val methMap: Map[String, String] =
    Map("eventName" -> "Methylation",
        "actionFlow" -> "default",
        "labels" -> "Methylation",
        "verbalTriggerLemma" -> "methylate",
        "nominalTriggerLemma" -> "methylation")

  val simpleEventMap: Map[String, Map[String, String]] =
    Map("Phosphorylation" -> phosphoMap,
        "Ubiquitination" -> ubiqMap,
        "Sumosylation" -> sumoMap,
        "Glycosylation" -> glycoMap,
        "Acetylation" -> aceMap,
        "Farnesylation" -> farneMap,
        "Hydroxylation" -> hydroxMap,
        "Ribosylation" -> riboMap,
        "Methylation" -> methMap)
}
