package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachKBUtils._
import org.clulab.reach.grounding.Speciated._

import java.io.File

/**
  * Factory class for creating and loading an in-memory KB from a namespaced TSV file.
  *   Written by: Tom Hicks. 1/19/2016.
  *   Last Modified: Fix: remove redundant load.
  */
class TsvIMKBFactory {

  /** Main factory method to create, fill, and return an encapsulate knowledge base. */
  def make (
    metaInfo: IMKBMetaInfo,
    keyTransforms: KBKeyTransformsGroup = new KBKeyTransformsGroup()
  ): InMemoryKB = {
    val imkb: InMemoryKB = new InMemoryKB(metaInfo, keyTransforms)
    // load new in-memory KB, if filename specified:
    metaInfo.kbFilename.foreach { filename =>
      loadFromKBDir(imkb, filename, metaInfo.namespace) // load new in-memory KB
      // if (filename == "XYZ-KB.tsv.gz") {       // DEBUGGING
      //   imkb.dump                              // DEBUGGING
      // }
    }
    return imkb
  }


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
      val kbResourcePath = makePathInKBDir(new File(filename).getName)
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
    if ((fields.size > 2) && (fields(2) != ""))
      imkb.addEntries(text, namespace, refId, fields(2)) // store new KB entries w/ species
    else
      imkb.addEntries(text, namespace, refId)            // store new KB entries w/o species
  }

  /** Check for required fields in one row of a TSV input file. */
  private def tsvValidateFields (fields:Seq[String]): Boolean = {
    (fields.size >= 2) && fields(0).nonEmpty && fields(1).nonEmpty
  }

}
