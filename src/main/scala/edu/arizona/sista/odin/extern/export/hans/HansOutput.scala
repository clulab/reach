package edu.arizona.sista.odin.extern.export.hans

import java.io._
import java.util.Date
import edu.arizona.sista.utils.DateUtils

import scala.collection.mutable.Map
import scala.collection.mutable.MutableList

import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

import edu.arizona.sista.processors._
import edu.arizona.sista.odin._
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.odin.extern.export.JsonOutputter

/**
  * Defines classes and methods used to build and output HANS format models.
  *   Written by Mihai Surdeanu. 5/22/2015.
  *   Last Modified: Initial creation of infrastructure.
  */
class HansOutput extends JsonOutputter {
  type IDed = scala.collection.mutable.HashMap[Mention, String]
  type PropMap = scala.collection.mutable.HashMap[String, Any]
  type FrameList = scala.collection.mutable.MutableList[PropMap]  // has O(c) append

  // Constants:
  val AssumedProteins = Set("Family", "Gene_or_gene_product", "Protein", "Protein_with_site")
  val Now = DateUtils.formatUTC(new Date())

  // used by json output serialization:
  implicit val formats = org.json4s.DefaultFormats

  // incrementing ID for numbering entities
  protected val idCntr = new IncrementingId()


  //
  // Public API:
  //

  /** Output a JSON object representing the HANS format output for the given mentions. */
  // def toJSON (allMentions:Seq[Mention], startTime:Date, endTime:Date, outFile:File) = {
  override def toJSON (allMentions:Seq[Mention], outFile:File) = {
    val model:PropMap = new PropMap
    val mentions = allMentions.filter(allowableRootMentions)
    val mIds = assignMentionIds(mentions, new IDed)
    val frames = new FrameList              // top level list of mention maps
    mentions.foreach { mention =>
      // REACH creates a data structure for each mention, stores it in frames list:
      // val frame = beginNewFrame(mention, startTime, endTime, mIds)
      // frames += doMention(mention, mIds, frame)
    }
    model("frames") = frames
    writeJsonToFile(model, outFile)
  }


  //
  // Private Methods
  //

  /** Return true if the given mention is one that should be processed if it is an argument. */
  private def allowableArgumentMentions (mention:Mention): Boolean = {
    return (mention.isInstanceOf[EventMention] || mention.isInstanceOf[RelationMention])
  }

  /** Return true if the given mention is one that should be processed at the forest root. */
  private def allowableRootMentions (mention:Mention): Boolean = {
    return (mention.isInstanceOf[EventMention] ||
             (mention.isInstanceOf[RelationMention] && (mention.label != "Protein_with_site")) )
  }

  /** Assign all mentions a unique ID. */
  private def assignMentionIds (mentions:Seq[Mention], mIds:IDed): IDed = {
    mentions.foreach{ mention =>
      mIds.getOrElseUpdate(mention, idCntr.genNextId())
      assignMentionIds(mention.arguments.values.toSeq.flatten.filter(allowableArgumentMentions), mIds)
    }
    return mIds
  }



  /** Convert the entire output data structure to JSON and write it to the given file. */
  private def writeJsonToFile (model:PropMap, outFile:File) = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
    Serialization.writePretty(model, out)
    out.println()                           // add final newline which serialization omits
    out.flush()
    out.close()
  }
}


/** Implements an incrementing identification string for numbering entities. */
class IncrementingId {
  protected var cntr = 0

  /** Return the current identification string. */
  def currentId (): String = { s"${cntr}" }

  /** Increment counter and return new identification string. */
  def genNextId (): String = {
    cntr = cntr + 1
    return currentId()
  }

  /** Increment counter and return new identification string. */
  def genNextIdWithFormat (formatString:String): String = {
    cntr = cntr + 1
    return formatString.format(cntr)
  }
}
