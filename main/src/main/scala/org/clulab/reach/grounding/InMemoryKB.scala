package org.clulab.reach.grounding

import collection.mutable.{ HashMap, HashSet, Map, MultiMap, Set }

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachKBUtils._
import org.clulab.reach.grounding.Speciated._

import org.clulab.reach.grounding.InMemoryKB._

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Dont return add entries count. Rename NS/ID map.
  */
class InMemoryKB (

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: IMKBMetaInfo = new IMKBMetaInfo(),

  /** Configuration record containing KB key transforms for this KB. */
  val keyTransforms: IMKBKeyTransforms = new IMKBKeyTransforms()

)  {

  /** MultiMap of varying text keys to NS/ID. */
  val keysMap = new HashMap[String, Set[String]] with MultiMap[String, String]

  /** MultiMap of NS/ID (namespace/id) to KB Entries. */
  val nsidMap = new HashMap[String, Set[KBEntry]] with MultiMap[String, KBEntry]


  /** Return a sequence of ALL the KB entries in this KB. */
  def entries: Seq[KBEntry] = nsidMap.values.flatten.toSeq

  /** Return a sequence of ALL the NS/ID keys in this KB. */
  def keys: Seq[String] = keysMap.keys.toSeq

  /** Return a sequence of ALL the NS/ID keys in this KB. */
  def nsIds: Seq[String] = nsidMap.keys.toSeq

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
    * Duplicate entries are not added.
    */
  def addEntries (
    text: String,
    namespace: String,
    refId: String,
    species: String = NoSpeciesValue
  ): Unit = {
    val ns = namespace.toLowerCase          // canonicalize namespace
    val nsId = makeNamespaceId(ns, refId)   // make NS/ID key
    val kbe = new KBEntry(text, ns, refId, species.toLowerCase)
    storeEntry(nsId, kbe)                   // store KB entry under NS/ID key
    additionKeys(text).foreach { key =>     // transform text into 1 or more keys
      storeKey(key, nsId)                   // map each key to the same NS/ID key
    }
  }


  /** Apply key transforms to the given text string to return all storage keys. */
  def additionKeys (text:String): KeyCandidates =
    KBKeyTransforms.applyAllTransforms(keyTransforms.additionKTs, text)

  /** Apply key transforms to the given text string to return all query keys. */
  def queryKeys (text:String): KeyCandidates =
    KBKeyTransforms.applyAllTransforms(keyTransforms.queryKTs, text)


  /** Return resolutions for the set of KB entries for the given NS/ID string. */
  def lookupNsId (nsId:String): Resolutions = newResolutions(nsidMap.getOrElse(nsId, NoEntries))

  /** Return resolutions for the set of KB entries for the given namespace and ID strings. */
  def lookupNsId (namespace:String, id:String): Resolutions =
    lookupNsId(makeNamespaceId(namespace, id))

  /** Try lookups for all given NS/IDs until one succeeds or all fail. */
  def lookupNsIds (nsIds: NsIdSet): Resolutions = newResolutions(lookupEntries(nsIds))


  /** Return resolutions for the set of all KB entries for the given text string. */
  def lookup (text:String): Resolutions = lookupNsIds(lookupKeys(queryKeys(text)))

  /** Find the set of KB entries, for the given text string, which match the given
      single species. Returns resolutions for matching entries or None. */
  def lookupByASpecies (text:String, species:String): Resolutions =
    newResolutions(lookupEntries(lookupKeys(queryKeys(text)))
                   .filter(kbe => kbe.species == species))

  /** Finds the set of KB entries, for the given text string, which contains a species
      in the given set of species. Returns resolutions for matching entries or None. */
 def lookupBySpecies (text:String, speciesSet:SpeciesNameSet): Resolutions =
    newResolutions(lookupEntries(lookupKeys(queryKeys(text)))
                   .filter(kbe => isMemberOf(kbe.species, speciesSet)))

  /** Finds the set of KB entries, for the given text string, which have humans as the species.
      Returns resolutions for matching entries or None. */
  def lookupHuman (text:String): Resolutions =
    newResolutions(lookupEntries(lookupKeys(queryKeys(text)))
                   .filter(kbe => isHumanSpecies(kbe.species)))


  /** Find the set of KB entries, for the given text string, which do not contain a species.
      Returns resolutions for matching entries or None. */
  def lookupNoSpecies (text:String): Resolutions =
    newResolutions(lookupEntries(lookupKeys(queryKeys(text)))
                   .filter(kbe => kbe.hasNoSpecies))


  /** Try lookup function on all given keys until one succeeds or all fail. */
  private def tryKeyLookups (fn:(String) => NsIdSet, keys: KeyCandidates): NsIdSet = {
    keys.foreach { key =>
      val nsIdSet = fn.apply(key)
      if (!nsIdSet.isEmpty) return nsIdSet
    }
    return EmptyNsIdSet                          // tried all keys: no success
  }

  /** Return NS/IDs for the set of all KB entries indexed by the given key string. */
  private def lookupKey (key:String): NsIdSet = keysMap.getOrElse(key, EmptyNsIdSet)

  /** Lookup the given keys in order, returning the set of NS/IDs for the first key found. */
  private def lookupKeys (keys:KeyCandidates): NsIdSet = tryKeyLookups(lookupKey, keys)

  /** Return a combined Set of KB entries for all the given NS/IDs. */
  private def lookupEntries (nsIds: NsIdSet): Set[KBEntry] =
    nsIds.flatMap(nsidMap.get(_)).flatten

  /** Return a new KB resolution formed from given KB entry and this KB's meta information. */
  private def newResolution (entry: KBEntry): KBResolution = new KBResolution(
    entry.text, entry.namespace, entry.id, entry.species, metaInfo.asInstanceOf[KBMetaInfo]
  )

  /** Wrap the given sequence of KB entries as a sorted sequence of resolutions with
      meta information from this KB. */
  private def newResolutions (entries: Set[KBEntry]): Resolutions = {
    if (entries.isEmpty) None
    else Some(orderResolutions(entries.map(kbe => newResolution(kbe)).toSeq))
  }

  /** Store the given KB entry under the given NS/ID, if the entry is not already present. */
  private def storeEntry (nsId: String, entry: KBEntry) = nsidMap.addBinding(nsId, entry)

  /** Store the given NS/ID under the given text key, if the NS/ID not already present. */
  private def storeKey (key: String, nsId: String) = keysMap.addBinding(key, nsId)
}


object InMemoryKB {
  /** Type of the values in the NS/ID Map. */
  type KBEntries = Set[KBEntry]

  /** Constant denoting an empty set of KB entries. */
  val NoEntries = Set.empty[KBEntry]

  /** Type of the values in the Keys Map. */
  type NsIdSet = Set[String]

  /** Constant denoting an empty set of NS/IDs. */
  val EmptyNsIdSet = Set.empty[String]
}
