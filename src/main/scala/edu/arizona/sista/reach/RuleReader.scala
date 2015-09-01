package edu.arizona.sista.reach

import java.io.File
import edu.arizona.sista.reach.reach.preprocessing.TemplateMap


/**
 * Utilities to read rule files
 */
object RuleReader {

  case class Rules(entities: String, modifications: String, events: String)

  val resourcesPath = "/edu/arizona/sista/odin/domains/bigmechanism/summer2015/biogrammar"
  val entitiesMasterFile = s"$resourcesPath/entities_master.yml"
  val modificationsMasterFile = s"$resourcesPath/modifications_master.yml"
  val eventsMasterFile = s"$resourcesPath/events_master.yml"

  def readResource(filename: String): String = {
    val source = io.Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }

  def mkRules(): Rules  = {

    val entities = readResource(entitiesMasterFile)
    val modifications = readResource(modificationsMasterFile)
    val events = readResource(eventsMasterFile)

    Rules(entities, modifications, events)
  }
}