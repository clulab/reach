package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.odin._

/**
  * Trait implementing override logic for knowledge bases which try alternate resolutions.
  *   Written by Tom Hicks. 11/03/2015.
  *   Last Modified: Update for key transforms class rename.
  */
trait LocalProteinKBML extends LocalKBMentionLookup {

  private def lookups (allKeys:Seq[String]): Option[KBEntry] = {
    allKeys.foreach { key =>
      val entry = memoryKB.lookup(key)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }

  override def resolve (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)
    val allKeys = key +: makeAlternateKeys(key, LocalKBKeyTransforms.proteinKeyTransforms)
    memoryKB.newResolution(lookups(allKeys))
  }


  private def lookupsByASpecies (allKeys:Seq[String], species:String): Option[KBEntry] = {
    allKeys.foreach { key =>
      val entry = memoryKB.lookupByASpecies(key, species)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }

  override def resolveByASpecies (text:String, species:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)
    val allKeys = key +: makeAlternateKeys(key, LocalKBKeyTransforms.proteinKeyTransforms)
    memoryKB.newResolution(lookupsByASpecies(allKeys, species))
  }


  private def lookupsBySpecies (allKeys:Seq[String], speciesSet:SpeciesNameSet): Option[Iterable[KBEntry]] = {
    allKeys.foreach { key =>
      val entries = memoryKB.lookupBySpecies(key, speciesSet)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }

  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] = {
    val key = makeCanonicalKey(text)
    val allKeys = key +: makeAlternateKeys(key, LocalKBKeyTransforms.proteinKeyTransforms)
    val entries = lookupsBySpecies(allKeys, speciesSet)
    entries.map(_.map(kbe => memoryKB.newResolution(kbe)))
  }


  private def lookupsHuman (allKeys:Seq[String]): Option[Iterable[KBEntry]] = {
    allKeys.foreach { key =>
      val entries = memoryKB.lookupHuman(key)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }

  override def resolveHuman (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)
    val allKeys = key +: makeAlternateKeys(key, LocalKBKeyTransforms.proteinKeyTransforms)
    val entries = lookupsHuman(allKeys)                   // find matching entries
    memoryKB.newResolution(entries.flatMap(_.headOption)) // return first entry or None
  }

}
