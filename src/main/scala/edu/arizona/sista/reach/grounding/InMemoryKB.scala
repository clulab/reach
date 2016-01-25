package edu.arizona.sista.reach.grounding

import scala.io.Source
import collection.mutable.{ HashMap, HashSet, Map, MultiMap, Set }

import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Redo to use Sets of KB entries. Move type aliases here.
  */
class InMemoryKB (

  /** Tell whether this KB contains species information. */
  val hasSpeciesInfo: Boolean = false,      // default for KBs without species info

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: IMKBMetaInfo = new IMKBMetaInfo()

) extends Speciated with ReachKBKeyTransforms {

  /** Additional constructor to add namespace to meta information for reference. */
  def this (namespace: String, hasSpeciesInfo: Boolean, metaInfo: IMKBMetaInfo) = {
    this(hasSpeciesInfo, metaInfo)
    this.metaInfo.put("namespace", namespace)
  }


  /** The root data structure implementing this in-memory knowledge base. */
  val theKB = new HashMap[String, Set[KBEntry]] with MultiMap[String, KBEntry]

  /** Add the given entry to this KB, if it is unique. */
  def addEntry (entry:KBEntry) = theKB.addBinding(entry.key, entry)


  /** Find the optional set of all KB entries for the given key. */
  def lookupAll (key:String): Option[Iterable[KBEntry]] = theKB.get(key).map(eset => eset)

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsAll (allKeys:Seq[String]): Option[Iterable[KBEntry]] =
    applyLookupFn(lookupAll, allKeys)


  /** Find any optional KB entry for the given key. Returns the first
      KB entry found or None. */
  def lookupAny (key:String): Option[KBEntry] = theKB.get(key).flatMap(eset => eset.headOption)

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsAny (allKeys:Seq[String]): Option[KBEntry] = {
    allKeys.foreach { key =>
      val entry = lookupAny(key)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }


  /** Find the optional KB entry, for the given key, which matches the given species.
      Returns the first KB entry found or None. */
  def lookupByASpecies (key:String, species:String): Option[Iterable[KBEntry]] =
    theKB.get(key).map(eset => eset.filter(kbe => kbe.species == species)).filter(_.nonEmpty)

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsByASpecies (allKeys:Seq[String], species:String): Option[Iterable[KBEntry]] = {
    allKeys.foreach { key =>
      val entry = lookupByASpecies(key, species)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }


  /** Finds an optional set of KB entries, for the given key, which contains a
      species in the given set of species. */
  def lookupBySpecies (key:String, speciesSet:SpeciesNameSet): Option[Iterable[KBEntry]] =
    theKB.get(key).map(eset => eset.filter(kbe => isMemberOf(kbe.species, speciesSet))).filter(_.nonEmpty)

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
  def lookupHuman (key:String): Option[Iterable[KBEntry]] =
    theKB.get(key).map(eset => eset.filter(kbe => isHumanSpecies(kbe.species))).filter(_.nonEmpty)

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsHuman (allKeys:Seq[String]): Option[Iterable[KBEntry]] =
    applyLookupFn(lookupHuman, allKeys)


  /** Find the optional KB entry, for the given key, which does not contain a species.
      Returns the first KB entry found or None. */
  def lookupNoSpecies (key:String): Option[Iterable[KBEntry]] =
    theKB.get(key).map(eset => eset.filter(kbe => kbe.hasNoSpecies())).filter(_.nonEmpty)

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsNoSpecies (allKeys:Seq[String]): Option[Iterable[KBEntry]] =
    applyLookupFn(lookupNoSpecies, allKeys)


  /** Wrap the given KB entry in a new KB resolution formed from this KB and the given KB entry. */
  def newResolution (entry: KBEntry): KBResolution = new KBResolution(entry) // no metaInfo for now

  /** Wrap the given sequence of KB entries in a sequence of resolutions formed from
      this KB and the given KB entries. */
  def newResolutions (entries: Option[Iterable[KBEntry]]): Resolutions =
    entries.map(_.map(kbe => newResolution(kbe)))

  /** Wrap the given KB entry in a new singleton-sequence of resolutions formed from
      this KB and the given KB entry. */
  def toResolutions (entry: KBEntry): Resolutions = Option(Iterable.apply(newResolution(entry)))


  /** Try lookup function on all given keys until one succeeds or all fail. */
  private def applyLookupFn (fn:(String) => Option[Iterable[KBEntry]],
                             allKeys:Seq[String]): Option[Iterable[KBEntry]] = {
    allKeys.foreach { key =>
      val entry = fn.apply(key)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }

}
