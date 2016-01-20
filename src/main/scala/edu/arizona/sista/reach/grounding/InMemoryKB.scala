package edu.arizona.sista.reach.grounding

import scala.io.Source
import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Add namespace only with aux constructor: move it back to meta info.
  */
class InMemoryKB (

  /** Tell whether this KB contains species information. */
  val hasSpeciesInfo: Boolean = false,      // default for KBs without species info

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: IMKBMetaInfo = new IMKBMetaInfo()

) extends Speciated with ReachKBKeyTransforms {

  /** Auxiliary constructor to add namespace to meta information for reference. */
  def this (namespace: String, hasSpeciesInfo: Boolean, metaInfo: IMKBMetaInfo) = {
    this(hasSpeciesInfo, metaInfo)
    this.metaInfo.put("namespace", namespace)
  }

  /** The root data structure implementing this in-memory knowledge base. */
  val thisKB: KnowledgeBase = KnowledgeBase()


  /** Insert the given entry, merge it with an existing entry, or ignore it,
      depending on the contents and state of this KB. */
  def insertOrUpdateEntry (entry:KBEntry) = {
    var rentry= entry                       // default: prepare to add new entry
    var seMap = thisKB.get(entry.key).getOrElse(SpeciesEntryMap()) // get or make new SEMap
    val oldEntry = seMap.get(entry.species) // lookup the entry by species
    if (oldEntry.isDefined)                 // if there is existing entry for this species
      rentry = oldEntry.get.combine(entry)  // combined entry will replace existing entry

    // at this point, rentry contains a new or an updated entry to be re-stored
    seMap.put(rentry.species, rentry)       // add/replace entry under its species
    thisKB.put(rentry.key, seMap)           // put new/updated species-entry map into KB
  }


  /** Find the optional set of all KB entries for the given key. */
  def lookupAll (key:String): Option[Iterable[KBEntry]] =
    thisKB.get(key).map(spMap => spMap.values)

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsAll (allKeys:Seq[String]): Option[Iterable[KBEntry]] = {
    allKeys.foreach { key =>
      val entries = lookupAll(key)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }


  /** Find any optional KB entry for the given key. Returns the first
      KB entry found or None. */
  def lookupAny (key:String): Option[KBEntry] = {
    thisKB.get(key).flatMap(spMap => spMap.values.headOption)
  }

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsAny (allKeys:Seq[String]): Option[KBEntry] =
    applyLookupFn(lookupAny, allKeys)


  /** Find the optional KB entry, for the given key, which matches the given species.
      Returns the first KB entry found (should only be one) or None. */
  def lookupByASpecies (key:String, species:String): Option[KBEntry] = {
    thisKB.get(key).flatMap(spMap => spMap.get(species.toLowerCase))
  }

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsByASpecies (allKeys:Seq[String], species:String): Option[KBEntry] = {
    allKeys.foreach { key =>
      val entry = lookupByASpecies(key, species)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }


  /** Finds an optional set of KB entries, for the given key, which contains a
      species in the given set of species. */
  def lookupBySpecies (key:String, speciesSet:SpeciesNameSet): Option[Iterable[KBEntry]] = {
    val kbes = lookupAll(key)
    if (kbes.isDefined) {
      val matches = kbes.get.filter(kbe => isMemberOf(kbe.species,speciesSet))
      if (matches.isEmpty) None else Some(matches)
    }
    else None
  }

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsBySpecies (allKeys:Seq[String],
                        speciesSet:SpeciesNameSet): Option[Iterable[KBEntry]] =
  {
    allKeys.foreach { key =>
      val entries = lookupBySpecies(key, speciesSet)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }


  /** Finds an optional set of KB entries, for the given key, which have
      humans as the species. May return more than 1 entry because of synonyms. */
  def lookupHuman (key:String): Option[Iterable[KBEntry]] = {
    val kbes = lookupAll(key)
    if (kbes.isDefined) {
      val matches = kbes.get.filter(kbe => isHumanSpecies(kbe.species))
      if (matches.isEmpty) None else Some(matches)
    }
    else None
  }

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsHuman (allKeys:Seq[String]): Option[Iterable[KBEntry]] = {
    allKeys.foreach { key =>
      val entries = lookupHuman(key)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }


  /** Find the optional KB entry, for the given key, which does not contain a species.
      Returns the first KB entry found (should only be one) or None. */
  def lookupNoSpecies (key:String): Option[KBEntry] = {
    thisKB.get(key).flatMap(spMap => spMap.values.find((kbe) => kbe.hasNoSpecies()))
  }

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsNoSpecies (allKeys:Seq[String]): Option[KBEntry] =
    applyLookupFn(lookupNoSpecies, allKeys)


  /** Create and return a new KB resolution from this KB and the given KB entry. */
  def newResolution (entry:KBEntry): KBResolution = new KBResolution(entry, metaInfo)

  /** Create and return a new KB resolution from this KB and the given optional KB entry. */
  def newResolution (entry:Option[KBEntry]): Option[KBResolution] =
    entry.map(kbe => new KBResolution(kbe, metaInfo))


  /** Try lookup function on all given keys until one succeeds or all fail. */
  private def applyLookupFn (fn:(String) => Option[KBEntry],
                             allKeys:Seq[String]): Option[KBEntry] = {
    allKeys.foreach { key =>
      val entry = fn.apply(key)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }

}
