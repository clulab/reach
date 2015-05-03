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
  *   Last Modified: Modify output structure to list of index cards.
  */
class FriesOutput {
  type MuteMap = scala.collection.mutable.HashMap[String, Any]
  type MuteList = scala.collection.mutable.MutableList[MuteMap]  // has O(c) append

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
  protected val fries:MuteMap = new MuteMap

  //
  // Public API:
  //

  /** Output a JSON object representing the FRIES output for the given mentions. */
  def toJSON (mentions:Seq[Mention], doc:Document, fos:FileOutputStream) = {
    val cards = new MuteList
    // TODO: filter mentions
    mentions.foreach { m =>
      val card = beginNewCard(doc)
      // TODO: process current mention, add data to card
      cards += card
    }
    fries("cards") = cards
    writeJsonToFile(fos)
  }


  //
  // Private Methods
  //

  /** Return a new index card (map) initialized with the repeated document information. */
  def beginNewCard (doc:Document): MuteMap = {
    val card:MuteMap = new MuteMap
    card("pmc_id") = doc.id.getOrElse("DOC-ID-MISSING")
    card("reading_started") = Now
    card("reading_ended") = Now
    card("submitter") = "UAZ"
    card("reader_type") = "machine"
    card("extracted_information") = new MuteMap
    return card
  }

  /** Convert the entire output data structure to JSON and write it to the given file. */
  def writeJsonToFile (fos:FileOutputStream) = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(fos)))
    Serialization.writePretty(fries, out)
    out.println()                           // add final newline which serialization omits
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
