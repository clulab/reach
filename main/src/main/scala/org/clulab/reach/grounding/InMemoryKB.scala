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
  *   Last Modified: Begin to totally redo internal data structures.
  */
class InMemoryKB (

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: IMKBMetaInfo = new IMKBMetaInfo(),

  /** Configuration record containing KB key transforms for this KB. */
  val keyTransforms: IMKBKeyTransforms = new IMKBKeyTransforms()

)  {

  /** MultiMap of NS/ID (namespace/id) to KB Entries. */
  val nsidMap = new HashMap[String, Set[KBEntry]] with MultiMap[String, KBEntry]

  /** MultiMap of varying text keys to NS/ID. */
  val keysMap = new HashMap[String, Set[String]] with MultiMap[String, String]


  /** Return a sequence of ALL the KB entries in this KB. */
  def entries: Seq[KBEntry] = nsidMap.values.flatten.toSeq

  /** Return a sequence of ALL the NS/ID keys in this KB. */
  def keys: Seq[String] = keysMap.keys.toSeq

  /** Return a sequence of ALL the NS/ID keys in this KB. */
  def nsIDs: Seq[String] = nsidMap.keys.toSeq

  def dump: Unit = {
    keys.sorted.foreach { key =>
      println(s"${key}:")
      keysMap.get(key).foreach { nsIdSet =>
        nsIdSet.toSeq.sorted.foreach { nsId =>
          println(s"  ${nsId}:")
          nsidMap.get(nsId).foreach { kbeSet =>
            kbeSet.foreach { kbe =>
              println(s"    $kbe")
            }
          }
        }
      }
    }
  }

  /** Create and add zero or more KB entries for the given info.
    * Duplicate entries are not added. Return the number of entries added.
    */
  def addEntries (
    text: String,
    namespace: String,
    refId: String,
    species: String = NoSpeciesValue
  ): Int = {
    var addCount: Int = 0
    val ns = namespace.toLowerCase          // canonicalize namespace
    val nsId = makeNamespaceId(ns, refId)   // make NS/ID key
    val kbe = new KBEntry(text, ns, refId, species.toLowerCase)
    storeEntry(nsId, kbe)                   // store KB entry under NS/ID key
    additionKeys(text).foreach { key =>     // transform text into 1 or more keys
      storeKey(key, nsId)                   // map each key to the same NS/ID key
      addCount += 1                         // count how many mutated keys
    }
    return addCount                         // return count of new keys added
  }


  /** Apply key transforms to the given text string to return all storage keys. */
  def additionKeys (text:String): KeyCandidates =
    KBKeyTransforms.applyAllTransforms(keyTransforms.additionKTs, text)

  /** Apply key transforms to the given text string to return all query keys. */
  def queryKeys (text:String): KeyCandidates =
    KBKeyTransforms.applyAllTransforms(keyTransforms.queryKTs, text)


  /** Return resolutions for the set of all KB entries for the given text string. */
  def lookup (text:String): Resolutions = lookupKeys(queryKeys(text))

  /** Return resolutions for the set of all KB entries indexed by the given key string. */
  def lookupKey (key:String): Resolutions =
    newResolutions(nsidMap.get(key).map(eset => eset.toSeq)) // TODO LATER

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
    newResolutions(nsidMap.get(key)         // TODO LATER
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
    newResolutions(nsidMap.get(key)         // TODO LATER
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
    newResolutions(nsidMap.get(key)         // TODO LATER
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
    newResolutions(nsidMap.get(key)         // TODO LATER
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

  /** Return a new KB resolution formed from given KB entry and this KB's meta information. */
  private def newResolution (entry: KBEntry): KBResolution = new KBResolution(
    entry.text, entry.namespace, entry.id, entry.species,
    metaInfo.asInstanceOf[KBMetaInfo]
  )

  /** Wrap the given sequence of KB entries as a sorted sequence of resolutions with
      meta information from this KB. */
  private def newResolutions (entries: Option[Seq[KBEntry]]): Resolutions = {
    val resSeq = entries.map(_.map(kbe => newResolution(kbe)))
    resSeq.map { resolutions =>
      // enforce the desired sorting of entries:
      selectHuman(resolutions) ++ selectNoSpecies(resolutions) ++ selectNotHuman(resolutions)
    }
  }

  /** Store the given KB entry under the given NS/ID, if the entry is not already present. */
  private def storeEntry (nsId: String, entry: KBEntry) = nsidMap.addBinding(nsId, entry)

  /** Store the given NS/ID under the given text key, if the NS/ID not already present. */
  private def storeKey (key: String, nsId: String) = keysMap.addBinding(key, nsId)

}
