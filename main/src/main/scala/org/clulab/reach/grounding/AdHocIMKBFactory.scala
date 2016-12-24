package org.clulab.reach.grounding

import scala.io.Source
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBUtils._

/**
  * Factory class for creating and loading an in-memory KB from a mixed-namespaced TSV file.
  *   Written by: Tom Hicks. 1/19/2016.
  *   Last Modified: Update debugging dump of KB.
  */
class AdHocIMKBFactory extends Speciated with ReachKBKeyTransforms {

  /** Main factory method to create, fill, and return an encapsulated knowledge base. */
  def make (kbFilename:String = "", metaInfo:Option[IMKBMetaInfo] = None): InMemoryKB = {
    // all adhoc KBs have species info because human is assumed if species not given:
    val imkb: InMemoryKB = new InMemoryKB(true, metaInfo.getOrElse(new IMKBMetaInfo()))
    if (kbFilename != "")
      loadFromKBDir(imkb, kbFilename)     // load new in-memory KB
    // if (kbFilename == "NER-Grounding-Override.tsv.gz") {  // for DEBUGGING
    //   imkb.theKB.foreach { case (k, entries) =>   // for DEBUGGING
    //     entries.foreach { ent => println(ent.toString()) }} // for DEBUGGING
    // }
    return imkb
  }

  /** Additional factory method to workaround meta info option. */
  def make (kbFilename: String, metaInfo: IMKBMetaInfo): InMemoryKB =
    make(kbFilename, Some(metaInfo))


  /**
    * Load this KB from the given 4-5 column, tab-separated-value (TSV) text file.
    *   1st column (0) is the text string,
    *   2nd column (1) is the ID string,
    *   3rd column (2) is the Species string (optional content),
    *   4th column (3) is the Namespace string (required),
    *   5th column (4) is the Type string (ignored).
    * If filename argument is null or the empty string, skip file loading.
    */
  private def loadFromKBDir (imkb:InMemoryKB, filename:String) = {
    if ((filename != null) && !filename.trim.isEmpty) { // skip loading if filename missing
      val kbResourcePath = makePathInKBDir(filename)
      val source = sourceFromResource(kbResourcePath)
      source.getLines.map(tsvRowToFields(_)).filter(tsvValidateFields(_)).foreach { fields =>
        processFields(imkb, fields)
      }
      source.close()
    }
  }

  /** Extract particular fields and process them as needed. */
  private def processFields (imkb:InMemoryKB, fields: Seq[String]): Unit = {
    val text = fields(0)
    val refId = fields(1)
    val species = if (fields(2) != "") fields(2) else Speciated.Human
    val namespace = fields(3)
    imkb.addCanonicalEntry(text, namespace, refId, species) // store new entry
  }

  /** Check for required fields in one row of the TSV input file. */
  private def tsvValidateFields (fields:Seq[String]): Boolean = {
    if (fields.size < 4) return false       // sanity check
    return fields(0).nonEmpty && fields(1).nonEmpty && fields(3).nonEmpty
  }

}
