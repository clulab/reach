package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.odin._

/**
  * Trait implementing common logic for local Knowledge Base Mention lookup classes.
  *   Written by Tom Hicks. 10/28/2015.
  *   Last Modified: Cleanup: remove commented code, unused import.
  */
trait LocalKBMentionLookup extends LocalKBLookup with KBMentionLookup {
  // Simply combines traits to implement a string + Mention lookup class.
}
