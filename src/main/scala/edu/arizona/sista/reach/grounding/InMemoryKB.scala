package edu.arizona.sista.reach.grounding

import collection.mutable.{ HashMap, HashSet, Map, MultiMap, Set }

import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Remove unused lookup methods.
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

  /** Return an sequence over the entries in this KB. */
  def entries: Seq[KBEntry] = theKB.values.flatten.toSeq


  /** Return resolutions for the set of all KB entries for the given key. */
  def lookupAll (key:String): Resolutions =
    newResolutions(theKB.get(key).map(eset => eset.toSeq))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsAll (allKeys:Seq[String]): Resolutions =
    applyLookupFn(lookupAll, allKeys)


  /** Find the set of KB entries, for the given key, which match the given single species.
      Returns resolutions for matching entries or None. */
  def lookupByASpecies (key:String, species:String): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.toSeq.filter(kbe => kbe.species == species))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsByASpecies (allKeys:Seq[String], species:String): Resolutions = {
    allKeys.foreach { key =>
      val entries = lookupByASpecies(key, species)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }


  /** Finds the set of KB entries, for the given key, which contains a species in the
      given set of species. Returns resolutions for matching entries or None. */
 def lookupBySpecies (key:String, speciesSet:SpeciesNameSet): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.toSeq.filter(kbe => isMemberOf(kbe.species, speciesSet)))
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
                   .map(eset => eset.toSeq.filter(kbe => isHumanSpecies(kbe.species)))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsHuman (allKeys:Seq[String]): Resolutions =
    applyLookupFn(lookupHuman, allKeys)


  /** Find the set of KB entries, for the given key, which do not contain a species.
      Returns resolutions for matching entries or None. */
  def lookupNoSpecies (key:String): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.toSeq.filter(kbe => kbe.hasNoSpecies))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupsNoSpecies (allKeys:Seq[String]): Resolutions =
    applyLookupFn(lookupNoSpecies, allKeys)


  /** Wrap the given KB entry in a new KB resolution formed from this KB and the given KB entry. */
  def newResolution (entry: KBEntry): KBResolution = new KBResolution(entry, Some(metaInfo))

  /** Wrap the given sequence of KB entries in a sequence of resolutions formed from
      this KB and the given KB entries. */
  def newResolutions (entries: Option[Seq[KBEntry]]): Resolutions =
    entries.map(_.map(kbe => newResolution(kbe))).map(_.sortBy(kbe => (kbe.species, kbe.id)))


  /** Wrap the given KB entry in a new singleton-sequence of resolutions formed from
      this KB and the given KB entry. */
  def toResolutions (entry: KBEntry): Resolutions = Option(Seq.apply(newResolution(entry)))

  /** Wrap the given optional KB entry in a new singleton-sequence of resolutions formed from
      this KB and the given KB entry. */
  def makeResolution (entry: Option[KBEntry]): Option[KBResolution] =
    entry.map(kbe => newResolution(kbe))


  /** Try lookup function on all given keys until one succeeds or all fail. */
  private def applyLookupFn (fn:(String) => Resolutions, allKeys:Seq[String]): Resolutions = {
    allKeys.foreach { key =>
      val entries = fn.apply(key)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }

}
