package org.clulab.reach.grounding

import scala.Serializable
import scala.util.hashing.MurmurHash3._
import collection.mutable.{ HashMap, HashSet, Map, MultiMap, Set }

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachKBUtils._
import org.clulab.reach.grounding.Speciated._

import org.clulab.reach.grounding.InMemoryKB._

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Replacement of reverse lookups: add species for NS/ID method.
  */
class InMemoryKB (

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: IMKBMetaInfo = new IMKBMetaInfo(),

  /** Configuration record containing KB key transforms for this KB. */
  val keyTransforms: KBKeyTransformsGroup = new KBKeyTransformsGroup()

)  {

  /** MultiMap of varying text keys to NS/ID. */
  private val keysMap = new HashMap[String, Set[String]] with MultiMap[String, String]

  /** MultiMap of NS/ID (namespace/id) to KB Entries. */
  private val nsidMap = new HashMap[String, Set[KBEntry]] with MultiMap[String, KBEntry]


  /** Return a sequence of ALL the KB resolutions in this KB. */
  def resolutions: Resolutions = newResolutions(nsidMap.values.flatten)

  /** Return a sequence of ALL the NS/ID keys in this KB. */
  def keys: Seq[String] = keysMap.keys.toSeq

  /** Return a sequence of ALL the NS/ID keys in this KB. */
  def nsIds: Seq[String] = nsidMap.keys.toSeq

  /** Print a textual representation of the IMKB contents to standard output. */
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

  /** Add zero or more KB entries for the given info. Duplicate entries are not added. */
  def addEntries (
    text: String,
    namespace: String,
    refId: String,
    species: String = NoSpeciesValue
  ): Unit = {
    val trimText = text.trim                // insure no trailing whitespace
    val ns = namespace.toLowerCase          // canonicalize namespace
    val nsId = makeNamespaceId(ns, refId)   // make NS/ID key
    val kbe = new KBEntry(trimText, ns, refId, species.toLowerCase)
    storeEntry(nsId, kbe)                   // store KB entry under NS/ID key
    // transform the given text into one or more keys for the key -> NS/ID map. */
    applyAllTransforms(keyTransforms.baseKTs, trimText).foreach { key =>
      storeKey(key, nsId)                   // map each key to the same NS/ID key
    }
  }

  /** Return resolutions for the set of KB entries for the given NS/ID string. */
  def lookupNsId (nsId:String): Resolutions = newResolutions(nsidMap.getOrElse(nsId.trim, NoEntries))

  /** Return resolutions for the set of KB entries for the given namespace and ID strings. */
  def lookupNsId (namespace:String, id:String): Resolutions =
    lookupNsId(makeNamespaceId(namespace, id)) // trimming handled in makeNamespaceId

  /** Try lookups for all given NS/IDs until one succeeds or all fail. */
  def lookupNsIds (nsIds: Set[String]): Resolutions = newResolutions(lookupEntries(nsIds))

  /** Return the set of species for the entries mapped by the given NS/ID key. */
  def speciesForNsId (nsId:String): SpeciesNameSet =
    nsidMap.getOrElse(nsId.trim, NoEntries).map(_.species).filter(_ != NoSpeciesValue).toSet


  /** Return resolutions for the set of all KB entries for the given text string. */
  def lookup (text:String): Resolutions = newResolutions(search(text, None))

  /** Find the set of KB entries, for the given text string, which match the given
      single species. Returns resolutions for matching entries or None. */
  def lookupByASpecies (text:String, species:String): Resolutions =
    newResolutions(search(text, Some((kbe:KBEntry) => kbe.species == species)))

  /** Finds the set of KB entries, for the given text string, which contains a species
      in the given set of species. Returns resolutions for matching entries or None. */
 def lookupBySpecies (text:String, speciesSet:SpeciesNameSet): Resolutions =
   newResolutions(search(text, Some((kbe:KBEntry) => isMemberOf(kbe.species, speciesSet))))

  /** Finds the set of KB entries, for the given text string, which have humans as the species.
      Returns resolutions for matching entries or None. */
  def lookupHuman (text:String): Resolutions =
    newResolutions(search(text, Some((kbe:KBEntry) => isHumanSpecies(kbe.species))))

  /** Find the set of KB entries, for the given text string, which do not contain a species.
      Returns resolutions for matching entries or None. */
  def lookupNoSpecies (text:String): Resolutions =
    newResolutions(search(text, Some((kbe:KBEntry) => kbe.hasNoSpecies)))


  /** Generate one or more keys from the given text string, look the keys up and
      return the first set of KB entries which passes the optionally given filter. */
  private def search (text: String, filterFn: Option[EntryFilter]): KBEntries = {
    val cleanText = stripAllKeysSuffixes(text.trim) // remove default stop suffixes

    keyTransforms.baseKTs.foreach { bktFn =>    // try each base key transform in order:
      bktFn.apply(cleanText).foreach { key =>   // walk list of candidate keys
        val entries = findAndFilterEntries(key, filterFn) // lookup key, maybe filter results
        if (entries.nonEmpty) return entries    // if qualifying entries found, return them
      }
    }

    keyTransforms.auxKTs.foreach { aktFn =>       // try any auxiliary key transforms
      aktFn.apply(cleanText).foreach { auxKey =>  // walk list of generated auxiliary keys
        keyTransforms.postKTs.foreach { pktFn =>  // transform each aux key w/ post processing
          pktFn.apply(auxKey).foreach { key =>    // walk list of post processed candidate keys
            val entries = findAndFilterEntries(key, filterFn) // lookup key, maybe filter results
            if (entries.nonEmpty) return entries        // if qualifying entries found, return them
          }
        }
      }
    }

    NoEntries                             // all possible keys tried w/o result
  }

  /** Lookup given key and filter any resulting KB entries through the optional given filter. */
  private def findAndFilterEntries (key: String, filterFn: Option[EntryFilter]): KBEntries = {
    if (filterFn.isDefined)              // optionally filter any entries found
      filteredEntries(lookupKey(key), filterFn.get)
    else
      lookupEntries(lookupKey(key))
  }


  /** Return NS/IDs for the set of all KB entries indexed by the given key string. */
  private def lookupKey (key:String): NsIdSet = keysMap.getOrElse(key, EmptyNsIdSet)

  /** Return a combined Set of KB entries for all the given NS/IDs. */
  private def lookupEntries (nsIds: NsIdSet): KBEntries =
    nsIds.flatMap(nsidMap.get(_)).flatten

  /** Return a combined Set of KB entries for all the given NS/IDs. */
  private def filteredEntries (nsIds: NsIdSet, filterFn: EntryFilter): KBEntries =
    lookupEntries(nsIds).filter(filterFn)

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

  /** Wrap the given iterable of KB entries as a sorted sequence of resolutions with
      meta information from this KB. */
  private def newResolutions (entries: Iterable[KBEntry]): Resolutions = {
    if (entries.isEmpty) None
    else Some(orderResolutions(entries.toSeq.map(kbe => newResolution(kbe))))
  }

  /** Store the given KB entry under the given NS/ID, if the entry is not already present. */
  private def storeEntry (nsId: String, entry: KBEntry) = nsidMap.addBinding(nsId, entry)

  /** Store the given NS/ID under the given text key, if the NS/ID not already present. */
  private def storeKey (key: String, nsId: String) = keysMap.addBinding(key, nsId)
}


object InMemoryKB {
  /** Type of the values in the Keys Map. */
  type NsIdSet = Set[String]

  /** Constant denoting an empty set of NS/IDs. */
  val EmptyNsIdSet = Set.empty[String]

  /** Type of the values in the NS/ID Map. */
  private type KBEntries = Set[KBEntry]

  /** Type defining a filter function over KB entries. */
  private type EntryFilter = KBEntry => Boolean

  /** Constant denoting an empty set of KB entries. */
  private val NoEntries = Set.empty[KBEntry]


  /**
    * Class holding information about a specific entry in an InMemory Knowledge Base.
    *   Written by: Tom Hicks. 10/25/2015.
    *   Last Modified: Limit the scope of the KBEntry class by embedding it in IMKB.
    */
  class KBEntry (

    /** Text for this entry, loaded from the external KB. */
    val text: String,

    /** The external namespace for this entry (e.g., go, uniprot). */
    val namespace: String = DefaultNamespace,

    /** The reference ID, relative to the namespace for this entry (e.g., GO:0033110, P12345). */
    val id: String,

    /** The species associated with this entry, if any. Empty string represents no species. */
    val species: String = NoSpeciesValue

  ) extends Serializable {

    /** Helper method for equals redefinition. */
    def canEqual (other: Any): Boolean = other.isInstanceOf[KBEntry]

    /** Redefine instance equality based on matching some fields of this class. */
    override def equals (other: Any): Boolean = other match {
      case that: KBEntry => (
        that.canEqual(this) &&
          this.namespace == that.namespace &&
          this.id == that.id &&
          this.text.toLowerCase == that.text.toLowerCase &&
          this.species == that.species
      )
      case _ => false
    }

    /** Redefine hashCode. */
    override def hashCode: Int = {
      val h0 = stringHash("org.clulab.reach.grounding.KBEntry")
      val h1 = mix(h0, text.toLowerCase.hashCode)
      val h2 = mix(h1, namespace.hashCode)
      val h3 = mix(h2, id.hashCode)
      val h4 = mixLast(h3, species.hashCode)
      finalizeHash(h4, 4)
    }

    /** Tell whether this entry has an associated species or not. */
    def hasSpecies: Boolean = (species != NoSpeciesValue)
    def hasNoSpecies: Boolean = (species == NoSpeciesValue)

    /** Return a formatted string containing this entry's namespace and ID. */
    def nsId: String = ReachKBUtils.makeNamespaceId(namespace, id)

    /** Override method to provide logging/debugging printout. */
    override def toString: String =
      s"<KBEntry: ${text}, ${namespace}, ${id}, ${species}>"
  }

}
