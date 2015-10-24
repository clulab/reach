package edu.arizona.sista.reach.grounding2

/**
  * Class representing a meta-information about an external Knowledge Base.
  *   Written by: Tom Hicks. 10/22/2015.
  *   Last Modified: Initial creation.
  */
class KBMetaInfo (
  /** The primary URI of the external KB (e.g., http://identifiers.org/uniprot/). */
  val baseURI:String,

  /** The namespace of the external KB (e.g., uniprot). */
  val namespace: String,

  /** The Resource Identifier for the primary resource location for this
    * knowledge base (e.g., MIR:00100164).
    * NB: This is MIRIAM registration ID of the external knowledge base, NOT an entity ID. */
  val resourceID: String
) {

  /**
    * Using the given ID string, generate a URI which references an entry
    * in the namespace of external knowledge base
    * (e.g., given "P2345" => "http://identifiers.org/uniprot/P2345").
    */
  def referenceURI (id:String): String = s"${baseURI}${id}"

}
