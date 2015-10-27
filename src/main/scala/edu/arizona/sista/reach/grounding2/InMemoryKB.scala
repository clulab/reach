package edu.arizona.sista.reach.grounding2

import scala.io.Source

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Define internal KB, entry insertion.
  */
class InMemoryKB (

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: KBMetaInfo,

  /** The filename of the external KB to be loaded into memory. */
  val kbFilename: String

) extends Speciated with KBKeyTransforms {
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
    val seMap = thisKB.get(entry.key)       // lookup the given key in this KB
    if (seMap.isDefined) {                  // if key is already in this KB
      mergeOrIgnoreEntry(entry, seMap.get)  // handle the (possibly duplicate) entry
    }
    else {                                  // key not seen before
      val seMap = SpeciesEntryMap()         // allocate new species-entry map
      seMap.put(entry.species.getOrElse(""), entry) // add entry under its species
      thisKB.put(entry.key, seMap)          // add new species-entry map to KB
    }
  }

  /** Merge the given entry into this KB or ignore it, if it is a duplicate entry. */
  def mergeOrIgnoreEntry (entry:KBEntry, seMap:SpeciesEntryMap) = {
    // TODO: IMPLEMENT LATER
  }


  /** Create and return a new KB resolution from this KB and the given KB entry. */
  def newResolution (entry:KBEntry): KBResolution = {
    new KBResolution(metaInfo, entry)
  }

  /**
    * Load this KB from a 2 or 3-column, tab-separated-value (TSV) text file.
    *   1st column is the name string,
    *   2nd column is the ID string (2-col file) or species (3-col file),
    *   3rd column is the ID string (3-col file).
    */
  def readAndFillKB () = {
    val kbResourcePath = LocalKBUtils.makePathInKBDir(kbFilename)
    val source = LocalKBUtils.sourceFromResource(kbResourcePath)
    source.getLines.map(tsvRowToFields(_)).filter(tsvValidateFields(_)).foreach { fields =>
      val flds = parseFields(fields)        // extract and order fields
      var text = flds(0)                    // assign fields in order
      var refId = flds(1)
      var species = flds(2)
      insertOrUpdateEntry(makeEntry(text, refId, species)) // store new entry
    }
    source.close()
  }


  def resolve (text:String): Option[KBEntry] = {
    return None                             // TODO: IMPLEMENT LATER
  }

  def resolveBySpecies (text:String, species:SpeciesNameSet): Option[KBEntry] = {
    return None                             // TODO: IMPLEMENT LATER
  }


  private def compareAndSelect (oldEntry:KBEntry, newEntry:KBEntry): Option[KBEntry] = {
    return None                             // TODO: IMPLEMENT LATER
  }

  /** Make and return a KB entry from the given fields. */
  private def makeEntry (text:String, refId:String, species:String): KBEntry = {
    val key = makeCanonicalKey(text)        // make canonical storage key
    if (species == "")
      new KBEntry(text, key, refId)
    else
      new KBEntry(text, key, refId, Some(species.toLowerCase))
  }

  /** Sort the columns of a 2-col or 3-col TSV row into correct order. */
  private def parseFields (fields:Seq[String]): Seq[String] = {
    if (fields.size == 3)                   // with species
      return Seq(fields(0), fields(2), fields(1))
    else if (fields.size == 2)              // w/o species
      return Seq(fields(0), fields(1), "")
    else                                  // should never happen if validation works
      throw new Exception(
        s"BAD INPUT: validation failed to remove row with missing required fields: ${fields}")
  }

  /** Convert a single row string from a TSV file to a sequence of string fields. */
  private def tsvRowToFields (row:String): Seq[String] = {
    return row.split("\t").map(_.trim)
  }

  /** Check for required fields in one row of a 2 or 3-column TSV input file. */
  private def tsvValidateFields (fields:Seq[String]): Boolean = {
    return ((fields.size == 3) && fields(0).nonEmpty && fields(2).nonEmpty) ||
           ((fields.size == 2) && fields(0).nonEmpty && fields(1).nonEmpty)
  }


  //
  // MAIN - Load this KB from an external file upon Construction.
  //
  readAndFillKB()                           // load in-memory KB on instantiation
//  thisKB.foreach { case (k, seMap) =>       // REMOVE LATER
//    seMap.foreach { case (k2, ent) => println(ent.toString()) }} // REMOVE LATER
}
