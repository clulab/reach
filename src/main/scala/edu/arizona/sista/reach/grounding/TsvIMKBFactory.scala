package edu.arizona.sista.reach.grounding

import scala.io.Source
import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Factory class for creating and loading an in-memory KB from a namespaced TSV file.
  *   Written by: Tom Hicks. 1/19/2016.
  *   Last Modified: Initial creation.
  */
class TsvIMKBFactory (

  /** The external namespace for this entry (e.g., go, uniprot). */
  val namespace: String = DefaultNamespace,

  /** The filename of the external KB to be loaded into memory. */
  val kbFilename: String = null,            // default for KBs with no file to load

  /** Tell whether this KB contains species information. */
  val hasSpeciesInfo: Boolean = false,      // default for KBs without species info

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: IMKBMetaInfo = new IMKBMetaInfo()

) extends Speciated with ReachKBKeyTransforms {

  /** Auxiliary constructor for common instantiation. */
  def this (namespace: String, kbFilename: String, metaInfo: IMKBMetaInfo) =
    this(namespace, kbFilename, false, metaInfo)

  /** Auxiliary constructor for common instantiation. */
  def this (kbFilename: String) = this(DefaultNamespace, kbFilename, false, new IMKBMetaInfo())


  /** Create, fill and return an in-memory knowledge base. */
  def make (): InMemoryKB = {
    val thisKB: InMemoryKB = new InMemoryKB(namespace, hasSpeciesInfo, metaInfo)
    loadFromKBDir(thisKB, kbFilename)       // load new in-memory KB
    // thisKB.foreach { case (k, seMap) =>     // for DEBUGGING
    //   seMap.foreach { case (k2, ent) => println(ent.toString()) }} // for DEBUGGING
    return thisKB
  }


  /**
    * Load this KB from the given 2 or 3-column, tab-separated-value (TSV) text file.
    *   1st column is the name string,
    *   2nd column is the ID string (2-col file) or species (3-col file),
    *   3rd column is the ID string (3-col file).
    * If filename argument is null or the empty string, skip file loading.
    */
  private def loadFromKBDir (theKB:InMemoryKB, filename:String) = {
    if ((filename != null) && !filename.trim.isEmpty) { // skip loading if filename missing
      val kbResourcePath = ReachKBUtils.makePathInKBDir(filename)
      val source = ReachKBUtils.sourceFromResource(kbResourcePath)
      source.getLines.map(ReachKBUtils.tsvRowToFields(_)).filter(tsvValidateFields(_)).foreach {
        fields =>
          val flds = parseFields(fields)        // extract and order fields
          var text = flds(0)                    // assign fields in order
          var refId = flds(1)
          var species = flds(2)
          theKB.insertOrUpdateEntry(makeEntry(text, refId, species))  // store new entry
      }
      source.close()
    }
  }

  /** Make and return a KB entry from the given fields. */
  private def makeEntry (text:String, refId:String, species:String): KBEntry = {
    val key = makeCanonicalKey(text)        // make canonical storage key
    return new KBEntry(text, key, namespace, refId, species.toLowerCase)
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

  /** Check for required fields in one row of the TSV input file. */
  private def tsvValidateFields (fields:Seq[String]): Boolean = {
    if (hasSpeciesInfo)
      ((fields.size == 3) && fields(0).nonEmpty && fields(2).nonEmpty)
    else
      ((fields.size == 2) && fields(0).nonEmpty && fields(1).nonEmpty)
  }

}
