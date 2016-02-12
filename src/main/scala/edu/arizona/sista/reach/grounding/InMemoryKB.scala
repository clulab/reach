package edu.arizona.sista.reach.grounding

import collection.mutable.{ HashMap, HashSet, Map, MultiMap, Set }

import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Include meta info in resolutions.
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


  /** Return resolutions for the set of all KB entries for the given key. */
  def lookupAll (key:String): Resolutions =
    newResolutions(theKB.get(key).map(eset => eset))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsAll (allKeys:Seq[String]): Resolutions =
    applyLookupFn(lookupAll, allKeys)


  /** Find any optional KB entry for the given key. Returns a resolution
      for the first KB entry found or None. */
  def lookupAny (key:String): Option[KBResolution] =
    makeResolution(theKB.get(key).flatMap(eset => eset.headOption))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsAny (allKeys:Seq[String]): Option[KBResolution] = {
    allKeys.foreach { key =>
      val entry = lookupAny(key)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }


  /** Find the set of KB entries, for the given key, which match the given single species.
      Returns resolutions for matching entries or None. */
  def lookupByASpecies (key:String, species:String): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.filter(kbe => kbe.species == species))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsByASpecies (allKeys:Seq[String], species:String): Resolutions = {
    allKeys.foreach { key =>
      val entry = lookupByASpecies(key, species)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }


  /** Finds the set of KB entries, for the given key, which contains a species in the
      given set of species. Returns resolutions for matching entries or None. */
 def lookupBySpecies (key:String, speciesSet:SpeciesNameSet): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.filter(kbe => isMemberOf(kbe.species, speciesSet)))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsBySpecies (allKeys:Seq[String],
                        speciesSet:SpeciesNameSet): Resolutions =
  {
    allKeys.foreach { key =>
      val entries = lookupBySpecies(key, speciesSet)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }


  /** Finds the set of KB entries, for the given key, which have humans as the species.
      Returns resolutions for matching entries or None. */
  def lookupHuman (key:String): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.filter(kbe => isHumanSpecies(kbe.species)))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsHuman (allKeys:Seq[String]): Resolutions =
    applyLookupFn(lookupHuman, allKeys)


  /** Find the set of KB entries, for the given key, which do not contain a species.
      Returns resolutions for matching entries or None. */
  def lookupNoSpecies (key:String): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.filter(kbe => kbe.hasNoSpecies()))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsNoSpecies (allKeys:Seq[String]): Resolutions =
    applyLookupFn(lookupNoSpecies, allKeys)


  /** Wrap the given KB entry in a new KB resolution formed from this KB and the given KB entry. */
  def newResolution (entry: KBEntry): KBResolution = new KBResolution(entry, Some(metaInfo))

  /** Wrap the given sequence of KB entries in a sequence of resolutions formed from
      this KB and the given KB entries. */
  def newResolutions (entries: Option[Iterable[KBEntry]]): Resolutions =
    entries.map(_.map(kbe => newResolution(kbe)))

  /** Wrap the given KB entry in a new singleton-sequence of resolutions formed from
      this KB and the given KB entry. */
  def toResolutions (entry: KBEntry): Resolutions = Option(Iterable.apply(newResolution(entry)))

  /** Wrap the given optional KB entry in a new singleton-sequence of resolutions formed from
      this KB and the given KB entry. */
  def makeResolution (entry: Option[KBEntry]): Option[KBResolution] =
    entry.map(kbe => newResolution(kbe))


  /** Try lookup function on all given keys until one succeeds or all fail. */
  private def applyLookupFn (fn:(String) => Resolutions, allKeys:Seq[String]): Resolutions = {
    allKeys.foreach { key =>
      val entry = fn.apply(key)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }

}
