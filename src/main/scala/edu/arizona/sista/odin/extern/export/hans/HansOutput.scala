package edu.arizona.sista.odin.extern.export.hans

import java.io._
import java.util.Date
import edu.arizona.sista.bionlp.FriesEntry
import edu.arizona.sista.bionlp.mentions.{PTM, Modifications, Grounding, Display}
import edu.arizona.sista.processors.Document

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

  // incrementing ID for numbering entity mentions
  protected val entityIdCntr = new IncrementingId()
  // incrementing ID for numbering event mentions
  protected val eventIdCntr = new IncrementingId()

  // required for json output serialization:
  implicit val formats = org.json4s.DefaultFormats

  //
  // Public API:
  //

  override def toJSON (paperId:String,
                       allMentions:Seq[Mention],
                       paperPassages:Seq[FriesEntry],
                       startTime:Date,
                       endTime:Date,
                       outFilePrefix:String): Unit = {
    // map of FriesEntry, using chunkId as key
    val passageMap = passagesToMap(paperPassages)

    sentencesToJSON(paperId, allMentions, passageMap,
      startTime, endTime, new File(outFilePrefix + ".sentences.json"))

    // entityMap: map from entity pointers to unique ids
    val entityMap = entitiesToJSON(paperId, allMentions, passageMap,
      startTime, endTime, new File(outFilePrefix + ".entities.json"))

    eventsToJSON(paperId, allMentions, passageMap, entityMap,
      startTime, endTime, new File(outFilePrefix + ".events.json"))
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
      val chunkId = getChunkId(m)
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

  /** Outputs a JSON object containing all entity mentions extracted from this paper. */
  private def entitiesToJSON (paperId:String,
                              allMentions:Seq[Mention],
                              paperPassages:Map[String, FriesEntry],
                              startTime:Date,
                              endTime:Date,
                              outFile:File): IDed = {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime)
    val entityMap = new IDed

    // this stores all frames in this output
    val frames = new FrameList
    model("frames") = frames

    for(mention <- allMentions) {
      mention match {
        case em:TextBoundMention with Display with Grounding with Modifications =>
          val cid = getChunkId(em)
          assert(paperPassages.contains(cid))
          val passageMeta = paperPassages.get(cid).get
          frames += mkEntityMention(paperId, passageMeta, em, entityMap)
        case _ => // nothing to do here
      }
    }

    // write the JSON to the given file
    writeJsonToFile(model, outFile)
    entityMap
  }

  /** Outputs a JSON object containing all event mentions extracted from this paper. */
  private def eventsToJSON (paperId:String,
                            allMentions:Seq[Mention],
                            paperPassages:Map[String, FriesEntry],
                            entityMap:IDed,
                            startTime:Date,
                            endTime:Date,
                            outFile:File) {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime)

    // this stores all frames in this output
    val frames = new FrameList
    model("frames") = frames

    val eventMentions = allMentions.filter(isEventMention)
    for(mention <- eventMentions) {
      val cid = getChunkId(mention)
      assert(paperPassages.contains(cid))
      val passageMeta = paperPassages.get(cid).get
    }

    // write the JSON to the given file
    writeJsonToFile(model, outFile)
  }

  private def isEventMention(m:Mention):Boolean = {
    m.isInstanceOf[EventMention] || m.isInstanceOf[RelationMention]
  }

  private def mkEntityMention(paperId:String,
                              passageMeta:FriesEntry,
                              mention:TextBoundMention with Display with Grounding with Modifications,
                              entityMap: IDed): PropMap = {
    val f = startFrame(COMPONENT)
    f("frame-type") = "entity-mention"
    val eid = mkEntityId(paperId, passageMeta, mention.sentence)
    entityMap += mention -> eid
    f("frame-id") = eid
    f("sentence") = mkSentenceId(paperId, passageMeta, mention.sentence)
    f("start-pos") = mkRelativePosition(paperId, passageMeta, mention.startOffset)
    f("end-pos") = mkRelativePosition(paperId, passageMeta, mention.endOffset)
    f("text") = mention.text
    f("type") = prettifyLabel(mention.displayLabel)
    val xrefs = new FrameList
    mention.xref.foreach(r => xrefs += mkXref(r))
    f("xrefs") = xrefs
    if(mention.isModified) {
      val ms = new FrameList
      for(m <- mention.modifications) {
        m match {
          case ptm: PTM => ms += mkPTM(ptm)
          case _ => // we do not export anything else
        }
      }
      if(ms.nonEmpty)
        f("modifications") = ms
    }
    // TODO: add "index", i.e., the sentence-local number for this mention from this component
    f
  }

  private def mkPTM(ptm:PTM):PropMap = {
    val m = new PropMap
    m("type") = ptm.label
    if(ptm.site.isDefined) {
      m("site") = ptm.site
    }
    m
  }

  private def startFrame(component:String):PropMap = {
    val f = new PropMap
    f("object-type") = "frame"
    val meta = new PropMap
    meta("object-type") = "meta-info"
    meta("component") = component
    f("object-meta") = meta
    f
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
    val sent = startFrame("BioNLPProcessor")
    sent("frame-id") = mkSentenceId(paperId, passageMeta, offset)
    sent("passage") = mkPassageId(paperId, passageMeta)
    sent("start-pos") = mkRelativePosition(paperId, passageMeta, getSentenceStartCharacterOffset(passageDoc, offset))
    sent("end-pos") = mkRelativePosition(paperId, passageMeta, getSentenceEndCharacterOffset(passageDoc, offset))
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

  private def mkPassageId(paperId:String, passageMeta:FriesEntry):String =
    s"pass-$paperId-$ORGANIZATION-$RUN_ID-${passageMeta.chunkId}"
  private def mkSentenceId(paperId:String, passageMeta:FriesEntry, offset:Int):String =
    s"sent-$paperId-$ORGANIZATION-$RUN_ID-${passageMeta.chunkId}-$offset"
  private def mkEntityId(paperId:String, passageMeta:FriesEntry, offset:Int):String =
    s"ment-$paperId-$ORGANIZATION-$RUN_ID-${passageMeta.chunkId}-$offset-${entityIdCntr.genNextId()}"
  private def mkEventId(paperId:String, passageMeta:FriesEntry, offset:Int):String =
    s"evem-$paperId-$ORGANIZATION-$RUN_ID-${passageMeta.chunkId}-$offset-${eventIdCntr.genNextId()}"

  private def mkXref(xref:Grounding.Xref):PropMap = {
    val m = new PropMap
    m("object-type") = "db-reference"
    m("namespace") = xref.namespace
    m("id") = xref.id
    m
  }

  private def mkPassage(model:PropMap,
                        paperId:String,
                        passageMeta:FriesEntry,
                        passageDoc:Document): PropMap = {
    val passage = startFrame("nxml2fries")
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

  /** Convert the entire output data structure to JSON and write it to the given file. */
  private def writeJsonToFile (model:PropMap, outFile:File) = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
    Serialization.writePretty(model, out)
    out.println()                           // add final newline which serialization omits
    out.flush()
    out.close()
  }

  private def getChunkId(m:Mention):String = {
    assert(m.document.id.isDefined)
    val did = m.document.id.get
    // the chunk id is the string following the underscore in the document ids
    val chunkId = did.substring(did.lastIndexOf("_") + 1)
    chunkId
  }

  private def getSentenceStartCharacterOffset(doc:Document, sentOffset:Int):Int = {
    doc.sentences(sentOffset).startOffsets.head
  }
  private def getSentenceEndCharacterOffset(doc:Document, sentOffset:Int):Int = {
    doc.sentences(sentOffset).endOffsets.last
  }

  private def prettifyLabel(label:String):String = label.toLowerCase.replaceAll("_", "-")
}

object HansOutput {
  val RUN_ID = "r1"
  val COMPONENT = "REACH"
  val ORGANIZATION = "UAZ"
  val AssumedProteins = Set("Family", "Gene_or_gene_product", "Protein", "Protein_with_site")
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
}
