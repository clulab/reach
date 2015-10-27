package edu.arizona.sista.reach.grounding2

import scala.io.Source

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 10/25/2015.
  *   Last Modified: Refactor KB loading methods here.
  */
class InMemoryKB (

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: KBMetaInfo,

  /** The filename of the external KB to be loaded into memory. */
  val kbFilename: String

) extends Speciated with LocalKeyTransforms {

  def addEntry (entry:KBEntry) = {
  }


  /** Create and return a new KB resolution from this in-memory KB and the given KB entry. */
  def newResolution (entry:KBEntry): KBResolution = {
    new KBResolution(metaInfo, entry)
  }

  /**
    * Load the in-memory KB from a 2 or 3-column, tab-separated-value (TSV) text file.
    *   1st column is the name string,
    *   2nd column is the ID string (2-col file) or species (3-col file),
    *   3rd column is the ID string (3-col file).
    */
  def readAndFillKB () = {
    val kbResourcePath = LocalKBUtils.makePathInKBDir(kbFilename)
    val source = LocalKBUtils.sourceFromResource(kbResourcePath)
    source.getLines.map(tsvRowToFields(_)).filter(tsvValidateFields(_)).foreach { fields =>
      val flds = parseFields(fields)
      var text = flds(0)
      var refId = flds(1)
      var species = flds(2)
      val newEntry = makeEntry(text, refId, species)
    }
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
}
