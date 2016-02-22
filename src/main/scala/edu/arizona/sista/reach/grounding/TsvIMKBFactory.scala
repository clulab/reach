package edu.arizona.sista.reach.grounding

import scala.io.Source
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Factory class for creating and loading an in-memory KB from a namespaced TSV file.
  *   Written by: Tom Hicks. 1/19/2016.
  *   Last Modified: Remove spurious import.
  */
class TsvIMKBFactory extends Speciated with ReachKBKeyTransforms {

  /** Main factory method to create, fill, and return an encapsulate knowledge base. */
  def make (
    namespace: String = DefaultNamespace,
    kbFilename: String = "",                // default for KBs with no file to load
    hasSpeciesInfo: Boolean = false,        // default to KBs without species info
    metaInfo: Option[IMKBMetaInfo] = None
  ): InMemoryKB = {
    val imkb: InMemoryKB = new InMemoryKB(namespace.toLowerCase, hasSpeciesInfo,
                                          metaInfo.getOrElse(new IMKBMetaInfo()))
    if (kbFilename != "")
      loadFromKBDir(imkb, kbFilename, namespace)     // load new in-memory KB
    // imkb.imkb.foreach { case (k, entries) =>   // for DEBUGGING
    //   entries.foreach { ent => println(ent.toString()) }} // for DEBUGGING
    return imkb
  }

  /** Additional factory method to workaround option. */
  def make (namespace: String, kbFilename: String,
            hasSpeciesInfo: Boolean, metaInfo: IMKBMetaInfo): InMemoryKB =
    make(namespace, kbFilename, hasSpeciesInfo, Some(metaInfo))

  /** Additional factory method to default unused arguments. */
  def make (namespace: String, kbFilename: String, metaInfo: IMKBMetaInfo): InMemoryKB =
    make(namespace, kbFilename, false, Some(metaInfo))

  /** Additional factory method to default unused arguments. */
  def make (kbFilename: String, metaInfo: IMKBMetaInfo): InMemoryKB =
    make(DefaultNamespace, kbFilename, false, Some(metaInfo))

  /** Additional factory method to default unused arguments. */
  def make (kbFilename: String): InMemoryKB = make(DefaultNamespace, kbFilename, false, None)


  /**
    * Load this KB from the given 2 or 3-column, tab-separated-value (TSV) text file.
    *   1st column is the name string,
    *   2nd column is the ID string (2-col file) or species (3-col file),
    *   3rd column is the ID string (3-col file).
    * If filename argument is null or the empty string, skip file loading.
    */
  private def loadFromKBDir (imkb:InMemoryKB, filename:String, namespace:String) = {
    if ((filename != null) && !filename.trim.isEmpty) { // skip loading if filename missing
      val kbResourcePath = ReachKBUtils.makePathInKBDir(filename)
      val source = ReachKBUtils.sourceFromResource(kbResourcePath)
      source.getLines.map(
        ReachKBUtils.tsvRowToFields(_)).filter(tsvValidateFields(_)).foreach {
        fields =>
          val flds = parseFields(fields)    // extract and order fields
          var text = flds(0)                // assign fields in order
          var refId = flds(1)
          var species = flds(2)
          imkb.addEntry(makeEntry(text, namespace, refId, species)) // store new entry
      }
      source.close()
    }
  }

  /** Make and return a KB entry from the given fields. */
  private def makeEntry (text:String, namespace:String,
                         refId:String, species:String): KBEntry = {
    val key = makeCanonicalKey(text)        // make canonical storage key
    return new KBEntry(text, key, namespace.toLowerCase, refId, species.toLowerCase)
  }

  /** Sort the columns of a 2-col or 3-col TSV row into correct order. */
  private def parseFields (fields:Seq[String]): Seq[String] = {
    if (fields.size == 3)
      return Seq(fields(0), fields(2), fields(1))
    else if (fields.size == 2)              // w/o species
      return Seq(fields(0), fields(1), "")
    else                                    // should never happen if validation works
      throw new Exception(
        s"BAD INPUT: validation failed to remove row with missing required fields: ${fields}")
  }

  /** Check for required fields in one row of the TSV input file. */
  private def tsvValidateFields (fields:Seq[String]): Boolean = {
    ( ((fields.size == 3) && fields(0).nonEmpty && fields(1).nonEmpty && fields(2).nonEmpty) ||
      ((fields.size == 2) && fields(0).nonEmpty && fields(1).nonEmpty) )
  }

}
