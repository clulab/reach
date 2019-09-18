package org.clulab.reach

import scala.io.Source

/**
 * Utilities to read rule files
 */

// Testing git commit by Shraddha Thumsi
object RuleReader {

case class Rules(entities: String, modifications: String, events: String, context: String)

  val resourcesPath = "/org/clulab/reach/biogrammar"
  val entitiesMasterFile = s"$resourcesPath/entities_master.yml"
  val modificationsMasterFile = s"$resourcesPath/modifications_master.yml"
  val eventsMasterFile = s"$resourcesPath/events_master.yml"
  val contextRelationsFile = s"$resourcesPath/context/context_relations.yml"

  def readResource(filename: String): String = {
    println(s"size of Filename as received by readResource dir in RuleReader: ${filename}")
    val source = Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }

  def readFile(filename: String) = {
    val source = Source.fromFile(filename)
    val data = source.mkString
    source.close()
    data
  }

  def mkRules(): Rules  = {
    val entities = readResource(entitiesMasterFile)
    val modifications = readResource(modificationsMasterFile)
    val events = readResource(eventsMasterFile)
    val context = readResource(contextRelationsFile)

    Rules(entities, modifications, events, context)
  }

  // NOTE: rule reloading will NOT work because
  // the paths within the master file are still interpreted the same way
  // (i.e. they point to a location in the jar, NOT the modified rule files)
  // Currently, only changes to the master file are made visible by reloading
  def reload(): Rules = {
    val resourcesPrefix = s"src/main/resources"
    val entities = readFile(s"$resourcesPrefix/$entitiesMasterFile")
    val modifications = readFile(s"$resourcesPrefix/$modificationsMasterFile")
    val events = readFile(s"$resourcesPrefix/$eventsMasterFile")
    val context = readFile(s"$resourcesPrefix/$contextRelationsFile")

    Rules(entities, modifications, events, context)
  }
}
