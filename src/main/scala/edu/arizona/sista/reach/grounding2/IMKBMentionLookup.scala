package edu.arizona.sista.reach.grounding2

/**
  * Trait implementing common logic for local Knowledge Base Mention lookup classes.
  * This trait simply mixes other traits to implement a string + Mention lookup class.
  *   Written by Tom Hicks. 10/28/2015.
  *   Last Modified: Rename this trait.
  */
trait IMKBMentionLookup extends IMKBLookup with KBMentionLookup {
}
