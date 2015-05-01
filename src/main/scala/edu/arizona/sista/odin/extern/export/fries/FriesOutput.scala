package edu.arizona.sista.odin.extern.export.fries

import java.io._

import scala.collection.mutable.MutableList
import scala.collection.mutable.Map

import edu.arizona.sista.processors._
import edu.arizona.sista.odin._

/**
  * Defines classes and methods used to build and output FRIES models.
  *   Written by Tom Hicks. 4/30/2015.
  *   Last Modified: Initial creation.
  */
class FriesOutput {
  // Constants:
  val MapsToPhysicalEntity = Set("Gene_or_gene_product", "Protein",
                                 "Protein_with_site", "Simple_chemical")

  // create mention manager and cache
  protected val mentionMgr = new MentionManager()


  //
  // Public API:
  //

  /** Output a JSON object representing the FRIES output for the given mentions. */
  def toJSON (mentions:Seq[Mention], doc:Document, out:FileOutputStream) = {
    mentionMgr.mergeEventMentions(mentions) // merge all instances of same mention
    // mentionMgr.mergedEvents.foreach {       // walk the merge mentions to generate BioPAX
    // }
  }



  //
  // Private Methods
  //

}
