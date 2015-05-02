package edu.arizona.sista.odin.extern.export.fries

import java.io._
import java.util.Date
import edu.arizona.sista.utils.DateUtils

import scala.collection.mutable.MutableList
import scala.collection.mutable.Map

import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

import edu.arizona.sista.processors._
import edu.arizona.sista.odin._

/**
  * Defines classes and methods used to build and output FRIES models.
  *   Written by Tom Hicks. 4/30/2015.
  *   Last Modified: Begin JSON output.
  */
class FriesOutput {
  type MuteMap = scala.collection.mutable.HashMap[String, Any]

  // Constants:
  val MapsToPhysicalEntity = Set("Gene_or_gene_product", "Protein",
                                 "Protein_with_site", "Simple_chemical")
  val Now = DateUtils.formatUTC(new Date())

  // used for json output serialization
//  implicit val formats = native.Serialization.formats(NoTypeHints)
  implicit val formats = org.json4s.DefaultFormats

  // create mention manager and cache
  protected val mentionMgr = new MentionManager()

  // the map containing value for FRIES output
  protected var fries:MuteMap = new MuteMap
  fries("reading_started") = Now
  fries("reading_ended") = Now
  fries("submitter") = "UAZ"
  fries("reader_type") = "machine"
  fries("extracted_information") = new MuteMap

  //
  // Public API:
  //

  /** Output a JSON object representing the FRIES output for the given mentions. */
  def toJSON (mentions:Seq[Mention], doc:Document, fos:FileOutputStream) = {
    fries("pmc_id") = doc.id.getOrElse("DOC-ID")
    writeJsonToFile(fos)
  }


  //
  // Private Methods
  //
  def writeJsonToFile (fos:FileOutputStream) = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(fos)))
    out.println(Serialization.write(fries))
    out.flush()
    out.close()
  }
}


object EntityType extends Enumeration {
  type EntityType = Value
  val Protein, Chemical, Gene = Value
}

// Binding
// Hydrolysis
// Phosphorylation
// Positive_regulation
// Protein_with_site
// Transport
