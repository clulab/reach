package edu.arizona.sista.reach.grounding

import scala.io.Source
import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Factory class for creating and loading an in-memory KB from a mixed-namespaced TSV file.
  *   Written by: Tom Hicks. 1/19/2016.
  *   Last Modified: Initial creation.
  */
class AdHocIMKBFactory (

  /** The filename of the external KB to be loaded into memory. */
  val kbFilename: String = "",              // default for KBs with no file to load

  /** Tell whether this KB contains species information. */
  val hasSpeciesInfo: Boolean = false,      // default to KBs without species info

  /** Meta information about the external KB from which this KB was created. */
  val metaInfo: IMKBMetaInfo = new IMKBMetaInfo()

) extends Speciated with ReachKBKeyTransforms {

  /** Auxiliary constructor for instantiation without a file (unusual). */
  def this (hasSpeciesInfo: Boolean, metaInfo: IMKBMetaInfo) =
    this("", hasSpeciesInfo, metaInfo)

  /** Create, fill and return an in-memory knowledge base. */
  def make (): InMemoryKB = {
    val thisKB: InMemoryKB = new InMemoryKB(hasSpeciesInfo, metaInfo)
    if (kbFilename != "")
      loadFromKBDir(thisKB, kbFilename)     // load new in-memory KB
    // thisKB.foreach { case (k, seMap) =>     // for DEBUGGING
    //   seMap.foreach { case (k2, ent) => println(ent.toString()) }} // for DEBUGGING
    return thisKB
  }


  /**
    * Load this KB from the given 3 or 4-column, tab-separated-value (TSV) text file.
    *   1st column is the name string,
    *   2nd column is the namespace string,
    *   3rd column is the ID string,
    *   4th column is the (optional) species string (assumed human if not given).
    * If filename argument is null or the empty string, skip file loading.
    */
  private def loadFromKBDir (theKB:InMemoryKB, filename:String) = {
    if ((filename != null) && !filename.trim.isEmpty) { // skip loading if filename missing
      val kbResourcePath = ReachKBUtils.makePathInKBDir(filename)
      val source = ReachKBUtils.sourceFromResource(kbResourcePath)
      source.getLines.map(ReachKBUtils.tsvRowToFields(_)).filter(tsvValidateFields(_)).foreach { flds =>
        val species = if (flds.size > 3) flds(3) else Speciated.Human
        theKB.insertOrUpdateEntry(makeEntry(flds(0), flds(1), flds(2), species))  // store new entry
      }
      source.close()
    }
  }

  /** Make and return a KB entry from the given fields. */
  private def makeEntry (text:String, namespace:String,
                         refId:String, species:String): KBEntry = {
    val key = makeCanonicalKey(text)        // make canonical storage key
    return new KBEntry(text, key, namespace, refId, species.toLowerCase)
  }

  /** Check for required fields in one row of the TSV input file. */
  private def tsvValidateFields (fields:Seq[String]): Boolean = {
    return fields(0).nonEmpty && fields(1).nonEmpty && fields(2).nonEmpty
  }

}
