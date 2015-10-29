package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.odin._

import scala.io.Source

/**
  * Trait implementing common logic for local Knowledge Base Mention lookup classes.
  *   Written by Tom Hicks. 10/28/2015.
  *   Last Modified: Initial refactoring.
  */
trait LocalKBMentionLookup extends LocalKBLookup with KBMentionLookup {

  // override def resolve (mention:Mention): Option[KBEntry] = {
  // }

  // override def resolveHuman (mention:Mention): Option[KBEntry] = {
  // }

  // override def resolveBySpecies (mention:Mention, species:SpeciesNameSet): Option[KBEntry] = {
  // }

}
