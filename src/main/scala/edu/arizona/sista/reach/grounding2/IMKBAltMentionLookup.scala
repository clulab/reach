package edu.arizona.sista.reach.grounding2

/**
  * Trait implementing common logic for local Knowledge Base Mention lookup classes.
  * This trait simply mixes other traits to implement a string + Mention alternate lookups class.
  *   Written by Tom Hicks. 11/4/2015.
  *   Last Modified: Rename this trait.
  */
trait IMKBAltMentionLookup extends IMKBAltLookup with KBMentionLookup {
}
