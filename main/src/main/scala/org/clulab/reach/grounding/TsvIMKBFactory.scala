package org.clulab.reach.grounding

import scala.io.Source
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBUtils._

/**
  * Factory class for creating and loading an in-memory KB from a namespaced TSV file.
  *   Written by: Tom Hicks. 1/19/2016.
  *   Last Modified: Update debugging dump of KB.
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
    // if (kbFilename == "NER-Grounding-Override.tsv.gz") {  // for DEBUGGING
    //   imkb.theKB.foreach { case (k, entries) =>   // for DEBUGGING
    //     entries.foreach { ent => println(ent.toString()) }} // for DEBUGGING
    // }
    imkb
  }

  /** Additional factory method to workaround option. */
  def make (namespace: String, kbFilename: String,
            hasSpeciesInfo: Boolean, metaInfo: IMKBMetaInfo): InMemoryKB =
    make(namespace, kbFilename, hasSpeciesInfo, Some(metaInfo))

  /** Additional factory method to default unused arguments. */
  def make (namespace: String, kbFilename: String, metaInfo: IMKBMetaInfo): InMemoryKB =
    make(namespace, kbFilename, hasSpeciesInfo = false, Some(metaInfo))

  /** Additional factory method to default unused arguments. */
  def make (kbFilename: String, metaInfo: IMKBMetaInfo): InMemoryKB =
    make(DefaultNamespace, kbFilename, hasSpeciesInfo = false, Some(metaInfo))

  /** Additional factory method to default unused arguments. */
  def make (kbFilename: String): InMemoryKB = make(DefaultNamespace, kbFilename, hasSpeciesInfo = false, None)


  /**
    * Load this KB from the given 2-5 column, tab-separated-value (TSV) text file.
    *   1st column (0) is the text string,
    *   2nd column (1) is the ID string,
    *   3rd column (2) is the Species string (optional content),
    *   4th column (3) is the Namespace string (ignored),
    *   5th column (4) is the Type string (ignored).
    * If filename argument is null or the empty string, skip file loading.
    * The namespace for each entry is given as argument and any namespace column values are ignored.
    */
  private def loadFromKBDir (imkb:InMemoryKB, filename:String, namespace:String) = {
    if ((filename != null) && !filename.trim.isEmpty) { // skip loading if filename missing
      val kbResourcePath = makePathInKBDir(filename)
      val source = sourceFromResource(kbResourcePath)
      source.getLines.map(tsvRowToFields(_)).filter(tsvValidateFields(_)).foreach { fields =>
        processFields(imkb, fields, namespace)
      }
      source.close()
    }
  }

  /** Extract particular fields and process them as needed. */
  private def processFields (imkb:InMemoryKB, fields:Seq[String], namespace:String): Unit = {
    val text = fields(0)
    val refId = fields(1)
    val species = if ((fields.size > 2) && (fields(2) != "")) fields(2)
                  else KBEntry.NoSpeciesValue
    imkb.addCanonicalEntry(text, namespace, refId, species) // store new entry
  }

  /** Check for required fields in one row of a TSV input file. */
  private def tsvValidateFields (fields:Seq[String]): Boolean = {
    (fields.size >= 2) && fields(0).nonEmpty && fields(1).nonEmpty
  }

}
