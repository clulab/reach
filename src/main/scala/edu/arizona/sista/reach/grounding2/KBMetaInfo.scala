package edu.arizona.sista.reach.grounding2

/**
  * Class representing meta-information about an external Knowledge Base.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Add toString method.
  */
class KBMetaInfo (

  /** The primary URI of the external KB (e.g., http://identifiers.org/uniprot/). */
  val baseURI: String = "http://org.clulab.reach/uazid/",

  /** The namespace of the external KB (e.g., uniprot). */
  val namespace: String = "uazid",

  /** The Resource Identifier for the primary resource location for this
    * knowledge base (e.g., MIR:00100164).
    * NB: This is MIRIAM registration ID of the external knowledge base, NOT an entity ID. */
  val resourceId: String = "MIR:00000000"   // fake MIRIAM registration number

) {

  /**
    * Using the given ID string, generate a URI which references an entry
    * in the namespace of external knowledge base
    * (e.g., given "P2345" => "http://identifiers.org/uniprot/P2345").
    */
  def referenceURI (id:String): String = s"${baseURI}${id}"

  /** Override method to provide logging/debugging printout. */
  override def toString(): String =
    s"<KBMetaInfo: ${baseURI} | ${namespace} | ${resourceId}>"
}
