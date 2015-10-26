package edu.arizona.sista.reach.grounding2

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Initial creation.
  */
class InMemoryKB (

  /** The primary URI of the external KB (e.g., http://identifiers.org/uniprot/). */
  val baseURI:String,

  /** The namespace of the external KB (e.g., uniprot). */
  val namespace: String,

  /** The Resource Identifier for the primary resource location for this
    * knowledge base (e.g., MIR:00100164).
    * NB: This is MIRIAM registration ID of the external knowledge base, NOT an entity ID. */
  val resourceId: String,

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

