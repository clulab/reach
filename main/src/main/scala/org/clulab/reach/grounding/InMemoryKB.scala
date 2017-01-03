package org.clulab.reach.grounding

import collection.mutable.{ HashMap, HashSet, Map, MultiMap, Set }

// import org.clulab.reach.grounding.KBKeyTransforms._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachKBUtils._
import org.clulab.reach.grounding.Speciated._

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Redo lookup function signatures.
  */
class InMemoryKB (

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: IMKBMetaInfo = new IMKBMetaInfo(),

  /** Configuration record containing KB key transforms for this KB. */
  val keyTransforms: IMKBKeyTransforms = new IMKBKeyTransforms()

)  {

  /** The root data structure implementing this in-memory knowledge base. */
  val theKB = new HashMap[String, Set[KBEntry]] with MultiMap[String, KBEntry]

  /** Return a sequence of ALL the entries in this KB. */
  def entries: Seq[KBEntry] = theKB.values.flatten.toSeq

  /** Create and add zero or more KB entries for the given info.
    * Duplicate entries are not added. Return the number of entries added.
    */
  def addEntries (
    text: String,
    namespace: String,
    refId: String,
    species: String = KBEntry.NoSpeciesValue
  ): Int = {
    var addCount: Int = 0
    additionKeys(text).foreach { key =>
      storeEntry(new KBEntry(text, key, namespace.toLowerCase, refId, species.toLowerCase))
      addCount += 1
    }
    return addCount
  }

  /** Apply key transforms to the given text string to return all storage keys. */
  def additionKeys (text:String): KeyCandidates =
    KBKeyTransforms.applyAllTransforms(text, keyTransforms.additionKTs)

  /** Apply key transforms to the given text string to return all query keys. */
  def queryKeys (text:String): KeyCandidates =
    KBKeyTransforms.applyAllTransforms(text, keyTransforms.queryKTs)


  /** Return resolutions for the set of all KB entries for the given text string. */
  def lookup (text:String): Resolutions = lookupKeys(queryKeys(text))

  /** Return resolutions for the set of all KB entries indexed by the given key string. */
  def lookupKey (key:String): Resolutions =
    newResolutions(theKB.get(key).map(eset => eset.toSeq))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupKeys (allKeys:KeyCandidates): Resolutions =
    applyLookupFn(lookupKey, allKeys)


  /** Find the set of KB entries, for the given text string, which match the given
      single species. Returns resolutions for matching entries or None. */
  def lookupByASpecies (text:String, species:String): Resolutions =
    lookupKeysByASpecies(queryKeys(text), species)

  /** Find the set of KB entries, for the given key, which match the given single species.
      Returns resolutions for matching entries or None. */
  def lookupKeyByASpecies (key:String, species:String): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.toSeq.filter(kbe => kbe.species == species))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupKeysByASpecies (allKeys:KeyCandidates, species:String): Resolutions = {
    allKeys.foreach { key =>
      val entries = lookupKeyByASpecies(key, species)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }


  /** Finds the set of KB entries, for the given text string, which contains a species
      in the given set of species. Returns resolutions for matching entries or None. */
 def lookupBySpecies (text:String, speciesSet:SpeciesNameSet): Resolutions =
   lookupKeysBySpecies(queryKeys(text), speciesSet)

  /** Finds the set of KB entries, for the given key, which contains a species in the
      given set of species. Returns resolutions for matching entries or None. */
  def lookupKeyBySpecies (key:String, speciesSet:SpeciesNameSet): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.toSeq.filter(kbe => isMemberOf(kbe.species, speciesSet)))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupKeysBySpecies (allKeys:KeyCandidates,
                           speciesSet:SpeciesNameSet): Resolutions =
  {
    allKeys.foreach { key =>
      val entries = lookupKeyBySpecies(key, speciesSet)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }


  /** Finds the set of KB entries, for the given text string, which have humans as the species.
      Returns resolutions for matching entries or None. */
  def lookupHuman (text:String): Resolutions = lookupKeysHuman(queryKeys(text))

  /** Finds the set of KB entries, for the given key, which have humans as the species.
      Returns resolutions for matching entries or None. */
  def lookupKeyHuman (key:String): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.toSeq.filter(kbe => isHumanSpecies(kbe.species)))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupKeysHuman (allKeys:KeyCandidates): Resolutions =
    applyLookupFn(lookupKeyHuman, allKeys)


  /** Find the set of KB entries, for the given text string, which do not contain a species.
      Returns resolutions for matching entries or None. */
  def lookupNoSpecies (text:String): Resolutions = lookupKeysNoSpecies(queryKeys(text))

  /** Find the set of KB entries, for the given key, which do not contain a species.
      Returns resolutions for matching entries or None. */
  def lookupKeyNoSpecies (key:String): Resolutions =
    newResolutions(theKB.get(key)
                   .map(eset => eset.toSeq.filter(kbe => kbe.hasNoSpecies))
                   .filter(_.nonEmpty))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookupKeysNoSpecies (allKeys:KeyCandidates): Resolutions =
    applyLookupFn(lookupKeyNoSpecies, allKeys)



  /** Try lookup function on all given keys until one succeeds or all fail. */
  private def applyLookupFn (fn:(String) => Resolutions, allKeys:KeyCandidates): Resolutions = {
    allKeys.foreach { key =>
      val entries = fn.apply(key)
      if (entries.isDefined) return entries
    }
    return None                             // tried all keys: no success
  }

  /** Wrap the given KB entry in a new KB resolution formed from this KB and the given KB entry. */
  private def newResolution (entry: KBEntry): KBResolution =
    new KBResolution(entry, metaInfo.asInstanceOf[KBMetaInfo])

  /** Wrap the given sequence of KB entries as a sorted sequence of resolutions with
      meta information from this KB. */
  private def newResolutions (entries: Option[Seq[KBEntry]]): Resolutions = {
    val resSeq = entries.map(_.map(kbe => newResolution(kbe)))
    resSeq.map { resolutions =>
      // enforce the desired sorting of entries:
      selectHuman(resolutions) ++ selectNoSpecies(resolutions) ++ selectNotHuman(resolutions)
    }
  }

  /** Store the given entry in this KB, if not already present. */
  private def storeEntry (entry:KBEntry) = theKB.addBinding(entry.key, entry)

}
