package edu.arizona.sista.reach.grounding2

/**
  * Trait implementing override logic for knowledge bases which try alternate resolutions.
  *   Written by Tom Hicks. 11/03/2015.
  *   Last Modified: Add transforms parameter to alt resolvers, move lookups to IMKB.
  */
trait LocalAltKBLookup extends LocalKBLookup {

  /** Create and try alternate key lookups. */
  def resolveAlt (text:String, transforms:KeyTransforms): Option[KBResolution] = {
    val key = makeCanonicalKey(text)
    val lcKey = text.toLowerCase            // transform lower cased keys
    val allKeys = key +: makeAlternateKeys(lcKey, transforms)
    memoryKB.newResolution(memoryKB.lookups(allKeys))
  }

  /** Create and try alternate key lookups. */
  def resolveByASpeciesAlt (text:String, species:String,
                            transforms:KeyTransforms): Option[KBResolution] =
  {
    val key = makeCanonicalKey(text)
    val lcKey = text.toLowerCase            // transform lower cased keys
    val allKeys = key +: makeAlternateKeys(lcKey, transforms)
    memoryKB.newResolution(memoryKB.lookupsByASpecies(allKeys, species))
  }

  /** Create and try alternate key lookups. */
  def resolveBySpeciesAlt (text:String, speciesSet:SpeciesNameSet,
                           transforms:KeyTransforms): Option[Iterable[KBResolution]] =
  {
    val key = makeCanonicalKey(text)
    val lcKey = text.toLowerCase            // transform lower cased keys
    val allKeys = key +: makeAlternateKeys(lcKey, transforms)
    val entries = memoryKB.lookupsBySpecies(allKeys, speciesSet)
    entries.map(_.map(kbe => memoryKB.newResolution(kbe)))
  }

  /** Create and try alternate key lookups. */
  def resolveHumanAlt (text:String, transforms:KeyTransforms): Option[KBResolution] = {
    val key = makeCanonicalKey(text)
    val lcKey = text.toLowerCase            // transform lower cased keys
    val allKeys = key +: makeAlternateKeys(lcKey, transforms)
    val entries = memoryKB.lookupsHuman(allKeys)
    memoryKB.newResolution(entries.flatMap(_.headOption)) // return first entry or None
  }

}
