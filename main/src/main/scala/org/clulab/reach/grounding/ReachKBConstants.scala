package org.clulab.reach.grounding

/**
  * Trait for defining constants used by grounding and entity checking code.
  *   Written by Tom Hicks. 10/22/2015.
  *   Last Modified: Move no species value and test here.
  */
object ReachKBConstants {

  /** The default namespace string for KBs. */
  val DefaultNamespace: String = "uaz"

  /** The string used to separate a namespace and an ID. */
  val NamespaceIdSeparator: String = ":"

  /** Constant which represents the lack of a species in a KB entry. */
  val NoSpeciesValue: String = ""

  /** Tell whether the given string is the special no-species constant or not. */
  def isNoSpeciesValue (species:String): Boolean = (species == NoSpeciesValue)


  /** File Path to the directory which holds the entity knowledge bases. */
  val KBDirFilePath = "src/main/resources/org/clulab/reach/kb"

  /** Resource Path to the directory which holds the entity knowledge bases. */
  val KBDirResourcePath = "/org/clulab/reach/kb"

}
