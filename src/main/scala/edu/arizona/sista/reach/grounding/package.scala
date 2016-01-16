package edu.arizona.sista.reach

/**
  * Package object for grounding.
  *   Written by: Tom Hicks. 1/15/2016.
  *   Last Modified: Initial creation by refactoring.
  */
package object grounding {

  /** Type to map species name strings to KB entries for the same key. */
  type SpeciesEntryMap = scala.collection.mutable.Map[String, KBEntry]
  def  SpeciesEntryMap() = scala.collection.mutable.Map[String, KBEntry]()

  /** A KB maps key strings to species-differentiated entries. */
  type KnowledgeBase = scala.collection.mutable.Map[String, SpeciesEntryMap]
  def  KnowledgeBase() = scala.collection.mutable.Map[String, SpeciesEntryMap]()

  /** KB MetaInfo is, at minimum, a map of string keys/values. */
  type KBMetaInfo = scala.collection.mutable.HashMap[String, String]
  def  KBMetaInfo = scala.collection.mutable.HashMap[String, String]()

}
