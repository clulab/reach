package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._

/**
  * Traits and classes used to lookup information from external knowledge bases.
  *   Written by Tom Hicks. 10/19/2015.
  *   Last Modified: Initial refactoring.
  */
trait KnowledgeBaseLookup {
  /** Return type for all KB lookups. */
  type KBResolution = Map[String,String]

  /** The primary URI of the external KB (e.g., http://identifiers.org/uniprot/). */
  def baseURI: String = ""

  /** The namespace of the external KB (e.g., uniprot). */
  def namespace: String = ""

  /** The Resource Identifier for the primary resource location for this
    * knowledge base (e.g., MIR:00100164).
    * NB: This is MIRIAM registration ID of the external knowledge base, NOT an entity ID. */
  def resourceID: String = ""

  /** Canonicalize the given text string into a key for both storage and lookup. */
  def makeCanonicalKey (text:String): String = text // default: return unchanged string

  /**
    * Using the given ID string, generate a URI which references an entry
    * in the namespace of external knowledge base
    * (e.g., given "P2345" => "http://identifiers.org/uniprot/P2345").
    */
  def referenceURI (id:String): String = s"${baseURI}${id}"


  /** Resolve the given text to an entry in an external knowledge base
    * and return a map of keys and property values from that entry.
    * Default method to be overridden by each child knowledge base accessor.
    */
  def resolve (text:String): KBResolution = {
    return Map(
      "referenceID" -> "UNRESOLVED ID",     // the real resolver must return real value!
      "alternateIDs" -> "",                 // a list of alternate IDs might be available
      "baseURI" -> baseURI,                 // the primary URI of the KB
      "referenceURI" -> referenceURI(text), // URI referencing this entry in the KB
      "definition" -> "",                   // a term definition might be available
      "key" -> makeCanonicalKey(text),      // the canonical key
      "namespace" -> namespace,             // the namespace string of this accessor
      "resourceID" -> resourceID,           // MIRIAM registration ID
      "standardName" -> "",                 // standard nomenclature might be available
      "text" -> text                        // unadulterated text given to resolve
    )
  }

}

/** Trait Companion Object allows Mixin OR Import pattern. */
object KnowledgeBaseLookup extends KnowledgeBaseLookup
