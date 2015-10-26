package edu.arizona.sista.reach.grounding2

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Encapsulate KB meta info class.
  */
class InMemoryKB (

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: KBMetaInfo,

  /** The filename of the external KB to be loaded into memory. */
  val kbFilename: String

) extends Speciated {

  def addEntry (entry:KBEntry) = {
  }

  def readAndFillKB () = {
    // TODO: IMPLEMENT LATER
  }

  def resolve (text:String): Option[KBEntry] = {
    return None                             // TODO: IMPLEMENT LATER
  }

  def resolveBySpecies (text:String, species:SpeciesNameSet): Option[KBEntry] = {
    return None                             // TODO: IMPLEMENT LATER
  }

}
