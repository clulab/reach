package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.odin._

/**
  * Trait implementing override logic for knowledge bases which try alternate resolutions.
  *   Written by Tom Hicks. 11/03/2015.
  *   Last Modified: Initial creation.
  */
trait LocalProteinKBML extends LocalKBMentionLookup {

  def tryKeyAndAlternates (key:String, alternates:Seq[String]): Option[KBEntry] = {
    memoryKB.lookup(key) orElse {
      alternates.foreach { altKey =>
        val entry = memoryKB.lookup(altKey)
        if (entry.isDefined) return entry
      }
      return None
    }
  }

  override def resolve (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)        // make a lookup key from the given text
    val altKeys = makeAlternateKeys(key, KBKeyTransforms.proteinKeyTransforms)
    memoryKB.newResolution(tryKeyAndAlternates(key, altKeys))
  }

  override def resolveByASpecies (text:String, species:String): Option[KBResolution] = {
    return None                             // IMPLEMENT LATER
  }

  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] = {
    return None                             // IMPLEMENT LATER
  }

  override def resolveHuman (text:String): Option[KBResolution] = {
    return None                             // IMPLEMENT LATER
  }

}
