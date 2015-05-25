package edu.arizona.sista.odin.extern.export.hans

import java.io._
import java.util.Date
import edu.arizona.sista.bionlp.FriesEntry
import edu.arizona.sista.processors.Document
import edu.arizona.sista.utils.DateUtils

import org.json4s.native.Serialization
import edu.arizona.sista.odin._
import edu.arizona.sista.odin.extern.export.JsonOutputter

import HansOutput._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
  val now = DateUtils.formatUTC(new Date())

  // used by json output serialization:
  implicit val formats = org.json4s.DefaultFormats

  // incrementing ID for numbering entities
  protected val idCntr = new IncrementingId()


  //
  // Public API:
  //

  override def toJSON (paperId:String,
                       allMentions:Seq[Mention],
                       paperPassages:Seq[FriesEntry],
                       startTime:Date,
                       endTime:Date,
                       outFilePrefix:String): Unit = {
    sentencesToJSON(paperId, allMentions, passagesToMap(paperPassages),
      startTime, endTime, new File(outFilePrefix + ".sentences.json"))
  }


  //
  // Private Methods
  //

  /** Creates a map of all FriesEntries, using chunkId as key */
  private def passagesToMap(paperPassages:Seq[FriesEntry]):Map[String, FriesEntry] = {
    val map = new mutable.HashMap[String, FriesEntry]()
    for(e <- paperPassages) map += e.chunkId -> e
    map.toMap
  }

  /** Outputs a JSON object containing all sections and sentences in this paper. */
  private def sentencesToJSON (paperId:String,
                               allMentions:Seq[Mention],
                               paperPassages:Map[String, FriesEntry],
                               startTime:Date,
                               endTime:Date,
                               outFile:File) {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime)

    // keeps track of all documents created for each entry
    val passageDocs = new mutable.HashMap[String, Document]()
    for(m <- allMentions)  {
      assert(m.document.id.isDefined)
      val did = m.document.id.get
      val chunkId = did.substring(did.lastIndexOf("_") + 1)
      if(! passageDocs.contains(chunkId)) {
        passageDocs += chunkId -> m.document
      }
    }

    // this stores all frames in this output
    val frames = new FrameList
    model("frames") = frames

    // now output all passages as individual frames
    for(chunkId <- passageDocs.keySet) {
      assert(paperPassages.contains(chunkId))
      frames += mkPassage(model, paperId, paperPassages.get(chunkId).get, passageDocs.get(chunkId).get)
      frames ++= mkSentences(model, paperId, paperPassages.get(chunkId).get, passageDocs.get(chunkId).get)
    }

    // write the JSON to the given file
    writeJsonToFile(model, outFile)
  }

  private def mkSentences(model:PropMap,
                          paperId:String,
                          passageMeta:FriesEntry,
                          passageDoc:Document): Seq[PropMap] = {
    val sents = new ListBuffer[PropMap]
    for(i <- passageDoc.sentences.indices) {
      sents += mkSentence(model, paperId, passageMeta, passageDoc, i)
    }
    sents.toList
  }
  private def mkSentence(model:PropMap,
                         paperId:String,
                         passageMeta:FriesEntry,
                         passageDoc:Document,
                         offset:Int): PropMap = {
    val sent = new PropMap
    sent("object-type") = "frame"
    val meta = new PropMap
    meta("object-type") = "meta-info"
    meta("component") = "BioNLPProcessor"
    sent("object-meta") = meta
    sent("frame-id") = mkSentenceId(paperId, passageMeta, offset)
    sent("passage") = mkPassageId(paperId, passageMeta)
    sent("start-pos") = mkRelativePosition(paperId, passageMeta, passageDoc.sentences(offset).startOffsets.head)
    sent("end-pos") = mkRelativePosition(paperId, passageMeta, passageDoc.sentences(offset).endOffsets.last)
    sent("text") = passageDoc.sentences(offset).getSentenceText()
    sent
  }

  private def mkRelativePosition(paperId:String,
                                 passageMeta:FriesEntry,
                                 characterOffset:Int): PropMap = {
    val pos = new PropMap
    pos("object-type") = "relative-pos"
    pos("reference") = mkPassageId(paperId, passageMeta)
    pos("offset") = characterOffset
    pos
  }

  private def mkPassageId(paperId:String, passageMeta:FriesEntry):String = {
    s"pass-$paperId-$ORGANIZATION-$RUN_ID-${passageMeta.chunkId}"
  }

  private def mkSentenceId(paperId:String, passageMeta:FriesEntry, offset:Int):String = {
    s"${mkPassageId(paperId, passageMeta)}-$offset"
  }

  private def mkPassage(model:PropMap,
                        paperId:String,
                        passageMeta:FriesEntry,
                        passageDoc:Document): PropMap = {
    val passage = new PropMap
    passage("object-type") = "frame"
    val meta = new PropMap
    meta("object-type") = "meta-info"
    meta("component") = "nxml2fries"
    passage("object-meta") = meta
    passage("frame-id") = mkPassageId(paperId, passageMeta)
    passage("frame-type") = "passage"
    passage("index") = passageMeta.chunkId
    passage("section-id") = passageMeta.sectionId
    passage("section-name") = passageMeta.sectionName
    passage("is-title") = passageMeta.isTitle
    assert(passageDoc.text.isDefined)
    passage("text") = passageDoc.text.get.replaceAll("\\n", " ")
    passage
  }

  /** Outputs a JSON object containing all events extracted from this paper. */
  private def eventsToJSON (allMentions:Seq[Mention], paperId:String, startTime:Date, endTime:Date, outFile:File): Unit = {
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

  /** Outputs a JSON object containing all entities extracted from this paper. */
  private def entitiesToJSON (allMentions:Seq[Mention], paperId:String, startTime:Date, endTime:Date, outFile:File): Unit = {

  }

  def addMetaInfo(model:PropMap, paperId:String, startTime:Date, endTime:Date): Unit = {
    model("object-type") = "frame-collection"

    val meta = new PropMap
    meta("object-type") = "meta-info"
    meta("component") = COMPONENT
    meta("component-type") = "machine"
    meta("organization") = ORGANIZATION
    meta("doc-id") = paperId
    meta("processing-start") = startTime
    meta("processing-end") = endTime
    model("object-meta") = meta
  }

  /** Return true if the given mention is one that should be processed if it is an argument. */
  private def allowableArgumentMentions (mention:Mention): Boolean = {
    mention.isInstanceOf[EventMention] || mention.isInstanceOf[RelationMention]
  }

  /** Return true if the given mention is one that should be processed at the forest root. */
  private def allowableRootMentions (mention:Mention): Boolean = {
    mention.isInstanceOf[EventMention] || (mention.isInstanceOf[RelationMention] && (mention.label != "Protein_with_site"))
  }

  /** Assign all mentions a unique ID. */
  private def assignMentionIds (mentions:Seq[Mention], mIds:IDed): IDed = {
    mentions.foreach{ mention =>
      mIds.getOrElseUpdate(mention, idCntr.genNextId())
      assignMentionIds(mention.arguments.values.toSeq.flatten.filter(allowableArgumentMentions), mIds)
    }
    mIds
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

object HansOutput {
  val RUN_ID = "r1"
  val COMPONENT = "REACH"
  val ORGANIZATION = "UAZ"
}


/** Implements an incrementing identification string for numbering entities. */
class IncrementingId {
  protected var cntr = 0

  /** Return the current identification string. */
  def currentId (): String = { s"$cntr" }

  /** Increment counter and return new identification string. */
  def genNextId (): String = {
    cntr = cntr + 1
    currentId()
  }

  /** Increment counter and return new identification string. */
  def genNextIdWithFormat (formatString:String): String = {
    cntr = cntr + 1
    formatString.format(cntr)
  }
}
