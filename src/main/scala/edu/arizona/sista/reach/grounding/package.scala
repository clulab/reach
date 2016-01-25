package edu.arizona.sista.reach

/**
  * Package object for grounding.
  *   Written by: Tom Hicks. 1/15/2016.
  *   Last Modified: Redo to return resolution sequences.
  */
package object grounding {

  /** KB MetaInfo is, at minimum, a map of string keys/values. */
  type KBMetaInfo = scala.collection.mutable.HashMap[String, String]
  def  KBMetaInfo = scala.collection.mutable.HashMap[String, String]()

  type Resolutions = Option[Iterable[KBResolution]]
}
