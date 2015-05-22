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
      s"$eventsDir/translocation_events.yml")

    val ruleFiles = files map readResource mkString "\n\n"

    // Generate rules for templatic SIMPLE events
    val simpleEventTemplate = readResource(s"$templatesDir/simple-event_template.yml")
    val templaticEventRules = generateRulesFromTemplate(simpleEventTemplate, simpleEventMap)
    // println(templaticEventRules)

    // Generate rules for templatic ACTIVATION events
    val posActivationTemplate = readResource(s"$templatesDir/pos-activation_template.yml")
    val templaticPosActivationRules = generateRulesFromTemplateSingleEvent(posActivationTemplate, posActEventMap)
    val negActivationTemplate = readResource(s"$templatesDir/neg-activation_template.yml")
    val templaticNegActivationRules = generateRulesFromTemplateSingleEvent(negActivationTemplate, negActEventMap)

    // Generate rules for templatic REGULATION events
    val posRegTemplate = readResource(s"$templatesDir/pos-reg_template.yml")
    val templaticPosRegRules = generateRulesFromTemplateSingleEvent(posRegTemplate, posRegEventMap)
    val negRegTemplate = readResource(s"$templatesDir/neg-reg_template.yml")
    val templaticNegRegRules = generateRulesFromTemplateSingleEvent(negRegTemplate, negRegEventMap)


    ruleFiles +
      templaticEventRules +
      templaticPosActivationRules + templaticNegActivationRules +
      templaticPosRegRules + templaticNegRegRules
  }

  def readCorefRules(): String = {
    val files = Seq(
      // non-templatic grammars plus generics like "it" and "the protein"
      s"$corefDir/generic_entities.yml",
      s"$corefDir/generic_events.yml")

    val ruleFiles = files map readResource mkString "\n\n"

    ruleFiles + readModificationRules() + readEventRules()
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

    val simpleEventTemplate = readFile(templatesDir.getAbsolutePath + "/simple-event_template.yml")
    val templaticEvents = generateRulesFromTemplate(simpleEventTemplate, simpleEventMap)

    val posActTemplate = readFile(templatesDir.getAbsolutePath + "/pos-activation_template.yml")
    val templaticPosActs = generateRulesFromTemplateSingleEvent(posActTemplate, posActEventMap)
    val negActTemplate = readFile(templatesDir.getAbsolutePath + "/neg-activation_template.yml")
    val templaticNegActs = generateRulesFromTemplateSingleEvent(negActTemplate, negActEventMap)

    val posRegTemplate = readFile(templatesDir.getAbsolutePath + "/pos-reg_template.yml")
    val templaticPosRegs = generateRulesFromTemplateSingleEvent(posRegTemplate, posRegEventMap)
    val negRegTemplate = readFile(templatesDir.getAbsolutePath + "/neg-reg_template.yml")
    val templaticNegRegs = generateRulesFromTemplateSingleEvent(negRegTemplate, negRegEventMap)

    val entityRules = readRuleFilesFromDir(entitiesDir)
    val modificationRules = readRuleFilesFromDir(modificationsDir)

    val eventRules = readRuleFilesFromDir(eventsDir) +
      templaticEvents +
      templaticPosActs

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

  /** For when we have a single map */
  def generateRulesFromTemplateSingleEvent(template: String, varMap:TemplateMap):String = {
    replaceVars(template, varMap)
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

  val NEG_NOUNS = "inhibit|decreas|repress|supress"

  val posActEventMap: Map[String, String] =
    Map("labels" -> "Positive_activation, ComplexEvent, Event",
        "triggers" -> "acceler|activ|allow|augment|direct|elev|elicit|enhanc|increas|induc|initi|modul|necess|overexpress|potenti|produc|prolong|promot|rais|reactiv|recruit|rescu|respons|restor|retent|sequest|signal|support|synerg|synthes|trigger",
        "auxtriggers" -> "regul|activ",
        "negnouns" -> NEG_NOUNS)

  val negActEventMap: Map[String, String] =
    Map("labels" -> "Negative_activation, ActivationEvent, Event",
        "triggers" -> "inhibit|attenu|decreas|degrad|diminish|disrupt|impair|imped|knockdown|limit|lower|negat|reduc|reliev|repress|restrict|revers|slow|starv|supress")

  val posRegEventMap: Map[String, String] =
    Map("labels" -> "Positive_regulation, ComplexEvent, Event",
        "triggers" -> "acceler|accept|accompani|accumul|action|activ|allow|associ|augment|cataly|caus|cleav|compet|confer|consequ|contribut|convert|cooper|critic|direct|driv|elev|elicit|enhanc|escort|essenti|export|express|facilit|follow|free|gener|high|implic|import|inact|increas|induc|induct|initi|interact|interconvert|involv|lead|led|major|mediat|modif|modul|necess|overexpress|oxid|pivot|play|posit|potenti|proce|produc|prolong|promot|rais|reactiv|recruit|releas|render|requir|rescu|respons|restor|result|retent|sequest|serv|signal|stimul|suffici|sulfat|support|synerg|synthes|target|transcript|transduc|transfer|transport|trigger|unaffect|underli|uninduc|up-regul|upregul|util",
        "auxtriggers" -> "regul",
        "negnouns" -> NEG_NOUNS)

  val negRegEventMap: Map[String, String] =
    Map("labels" -> "Negative_regulation, ComplexEvent, Event",
        "triggers" -> "downreg|down-reg|abolish|abrog|absenc|antagon|arrest|attenu|block|blunt|decreas|defect|defici|degrad|delay|deplet|deregul|diminish|disengag|disrupt|down|drop|dysregul|elimin|impair|imped|inactiv|inhibit|interf|knockdown|lack|limit|loss|lost|lower|negat|neutral|nullifi|oppos|overc|perturb|prevent|reduc|reliev|remov|repress|resist|restrict|revers|shutdown|slow|starv|supress|uncoupl")

}
