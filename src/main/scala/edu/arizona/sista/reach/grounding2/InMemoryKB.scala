package edu.arizona.sista.reach.grounding2

import scala.io.Source

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Update for key transform trait rename.
  */
class InMemoryKB (

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: KBMetaInfo,

  /** The filename of the external KB to be loaded into memory. */
  val kbFilename: String = null,            // default for KBs with no file to load

  /** Tell whether this KB contains species information. */
  val hasSpeciesInfo: Boolean = false       // default for KBs without species info

) extends Speciated with ReachKeyTransforms {

  // type to map species name strings to KB entries for the same key:
  type SpeciesEntryMap = scala.collection.mutable.Map[String, KBEntry]
  def  SpeciesEntryMap() = scala.collection.mutable.Map[String, KBEntry]()

  type KnowledgeBase = scala.collection.mutable.Map[String, SpeciesEntryMap]
  def  KnowledgeBase() = scala.collection.mutable.Map[String, SpeciesEntryMap]()

  // the root data structure implementing this in-memory knowledge base
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

  /**
    * Load this KB from the given 2 or 3-column, tab-separated-value (TSV) text file.
    *   1st column is the name string,
    *   2nd column is the ID string (2-col file) or species (3-col file),
    *   3rd column is the ID string (3-col file).
    * If filename argument is null or the empty string, skip file loading.
    */
  def loadFromFile (filename:String) = {
    if ((filename != null) && !filename.trim.isEmpty) { // skip loading if filename missing
      val kbResourcePath = LocalKBUtils.makePathInKBDir(filename)
      val source = LocalKBUtils.sourceFromResource(kbResourcePath)
      source.getLines.map(tsvRowToFields(_)).filter(tsvValidateFields(_)).foreach { fields =>
        val flds = parseFields(fields)        // extract and order fields
        var text = flds(0)                    // assign fields in order
        var refId = flds(1)
        var species = flds(2)
        insertOrUpdateEntry(makeEntry(text, refId, species))  // store new entry
      }
      source.close()
    }
  }


  /** Find the optional set of KB entries for the given key. */
  def lookupAll (key:String): Option[Iterable[KBEntry]] = {
    thisKB.get(key).map(spMap => spMap.values)
  }


  /** Find the optional KB entry, for the given key, which does not contain a species.
      Returns the first KB entry found (should only be one) or None. */
  def lookup (key:String): Option[KBEntry] = {
    thisKB.get(key).flatMap(spMap => spMap.values.find((kbe) => kbe.hasNoSpecies()))
  }

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookups (allKeys:Seq[String]): Option[KBEntry] = {
    allKeys.foreach { key =>
      val entry = lookup(key)
      if (entry.isDefined) return entry
    }
    return None                             // tried all keys: no success
  }


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


  /** Create and return a new KB resolution from this KB and the given KB entry. */
  def newResolution (entry:KBEntry): KBResolution = new KBResolution(metaInfo, entry)

  /** Create and return a new KB resolution from this KB and the given optional KB entry. */
  def newResolution (entry:Option[KBEntry]): Option[KBResolution] =
    entry.map(kbe => new KBResolution(metaInfo, kbe))


  /** Make and return a KB entry from the given fields. */
  private def makeEntry (text:String, refId:String, species:String): KBEntry = {
    val key = makeCanonicalKey(text)        // make canonical storage key
    return new KBEntry(text, key, refId, species.toLowerCase)
  }

  /** Sort the columns of a 2-col or 3-col TSV row into correct order. */
  private def parseFields (fields:Seq[String]): Seq[String] = {
    if (hasSpeciesInfo)
      return Seq(fields(0), fields(2), fields(1))
    else if (fields.size == 2)              // w/o species
      return Seq(fields(0), fields(1), "")
    else                                    // should never happen if validation works
      throw new Exception(
        s"BAD INPUT: validation failed to remove row with missing required fields: ${fields}")
  }

  /** Convert a single row string from a TSV file to a sequence of string fields. */
  private def tsvRowToFields (row:String): Seq[String] = {
    return row.split("\t").map(_.trim)
  }

  /** Check for required fields in one row of a 2 or 3-column TSV input file. */
  private def tsvValidateFields (fields:Seq[String]): Boolean = {
    if (hasSpeciesInfo)
      ((fields.size == 3) && fields(0).nonEmpty && fields(2).nonEmpty)
    else
      ((fields.size == 2) && fields(0).nonEmpty && fields(1).nonEmpty)
  }


  //
  // MAIN - Load this KB from an external file upon Construction.
  //
  loadFromFile(kbFilename)                  // load in-memory KB on instantiation
  // thisKB.foreach { case (k, seMap) =>       // for DEBUGGING
  //   seMap.foreach { case (k2, ent) => println(ent.toString()) }} // for DEBUGGING
}
