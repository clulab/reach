package org.clulab.reach.grounding

import scala.io.Source
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachKBUtils._
import org.clulab.reach.grounding.Speciated._

import java.io.File

/**
  * Factory class for creating and loading an in-memory KB from a mixed-namespaced TSV file.
  *   Written by: Tom Hicks. 1/19/2016.
  *   Last Modified: Add inactive debugging dump.
  */
class AdHocIMKBFactory {

  /** Main factory method to create, fill, and return an encapsulated knowledge base. */
  def make (
    metaInfo: IMKBMetaInfo,
    keyTransforms: KBKeyTransformsGroup = new KBKeyTransformsGroup()
  ): InMemoryKB = {
    val imkb: InMemoryKB = new InMemoryKB(metaInfo)
    // load new in-memory KB, if filename specified:
    metaInfo.kbFilename.foreach { filename =>
      loadFromKBDir(imkb, filename)         // load new in-memory KB
      // if (filename == "NER-Grounding-Override.tsv.gz") { // DEBUGGING
      //   imkb.dump                                        // DEBUGGING
      // }
    }
    return imkb
  }


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
      val kbResourcePath = makePathInKBDir(new File(filename).getName)
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
    imkb.addEntries(text, namespace, refId, species) // store new KB entries
  }

  /** Check for required fields in one row of the TSV input file. */
  private def tsvValidateFields (fields:Seq[String]): Boolean = {
    if (fields.size < 4) return false       // sanity check
    return fields(0).nonEmpty && fields(1).nonEmpty && fields(3).nonEmpty
  }

}
