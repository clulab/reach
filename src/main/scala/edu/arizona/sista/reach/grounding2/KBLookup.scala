package edu.arizona.sista.reach.grounding2

/**
  * Trait used to lookup information from external knowledge bases.
  *   Written by: Tom Hicks. 10/22/2015.
  *   Last Modified: Update for KBEntry.
  */
trait KBLookup {

  /** Resolve the given text string to an optional entry in a knowledge base
    * Return a resolution for the entry, if any found.
    * NB: This method assumes a single resolution KB.
    * NB: This is a default method to be overridden by each child class.
    */
  def resolve (text:String): Option[KBEntry] = None

}


/** Trait Companion Object allows Mixin OR Import pattern. */
object KBLookup extends KBLookup
