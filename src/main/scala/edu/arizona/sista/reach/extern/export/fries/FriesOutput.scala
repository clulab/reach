package edu.arizona.sista.reach.extern.export.fries

import java.io._
import java.util.Date
import org.json4s.native.Serialization

import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.ReachConstants._
import edu.arizona.sista.reach.display._
import edu.arizona.sista.reach.extern.export._
import edu.arizona.sista.reach.grounding.KBResolution
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.nxml.FriesEntry

import JsonOutputter._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Defines classes and methods used to build and output the FRIES format.
  *   Written by Mihai Surdeanu. 5/22/2015.
  *   Last Modified: Output new isDirect event field.
  */
class FriesOutput extends JsonOutputter {
  type IDed = scala.collection.mutable.HashMap[Mention, String]

  // incrementing ID for numbering entity mentions
  protected val entityIdCntr = new IncrementingId()
  // incrementing ID for numbering event mentions
  protected val eventIdCntr = new IncrementingId()

  var currEvent = ""

  //
  // Public API:
  //

  /**
    * Returns the given mentions in the Fries JSON format, as one big string. The normally
    * separate representations for sentences, entities, and events are combined and
    * returned as a single JSON string with corresponding top-level fields.
    */
  override def toJSON (paperId:String,
                       allMentions:Seq[Mention],
                       paperPassages:Seq[FriesEntry],
                       startTime:Date,
                       endTime:Date,
                       outFilePrefix:String): String = {

    val otherMetaData = extractOtherMetaData(paperPassages)
    val passageMap = passagesToMap(paperPassages) // map of FriesEntry, chunkId as key

    val sentModel = sentencesToModel(paperId, allMentions, passageMap,
                                     startTime, endTime, otherMetaData)
    val (entityModel, entityMap) = entitiesToModel(paperId, allMentions, passageMap,
                                                   startTime, endTime, otherMetaData)
    // entityMap: map from entity pointers to unique ids
    val eventModel = eventsToModel(paperId, allMentions, passageMap, entityMap,
                                   startTime, endTime, otherMetaData)

    val uniModel:PropMap = new PropMap      // combine models into one
    uniModel("sentences") = sentModel
    uniModel("entities") = entityModel
    uniModel("events") = eventModel

    writeJsonToString(uniModel)
  }


  /**
    * Writes the given mentions to output files in Fries JSON format.
    * Separate output files are written for sentences, entities, and events.
    * Each output file is prefixed with the given prefix string.
    */
  override def writeJSON (paperId:String,
                          allMentions:Seq[Mention],
                          paperPassages:Seq[FriesEntry],
                          startTime:Date,
                          endTime:Date,
                          outFilePrefix:String): Unit = {

    val otherMetaData = extractOtherMetaData(paperPassages)
    val passageMap = passagesToMap(paperPassages) // map of FriesEntry, chunkId as key

    val sentModel = sentencesToModel(paperId, allMentions, passageMap,
                                     startTime, endTime, otherMetaData)
    writeJsonToFile(sentModel, new File(outFilePrefix + ".uaz.sentences.json"))

    // entityMap: map from entity pointers to unique ids
    val (entityModel, entityMap) = entitiesToModel(paperId, allMentions, passageMap,
                                                   startTime, endTime, otherMetaData)
    writeJsonToFile(entityModel, new File(outFilePrefix + ".uaz.entities.json"))

    val eventModel = eventsToModel(paperId, allMentions, passageMap, entityMap,
                                   startTime, endTime, otherMetaData)
    writeJsonToFile(eventModel, new File(outFilePrefix + ".uaz.events.json"))
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

  /** Returns a data-structure representing all sections and sentences in this paper. */
  private def sentencesToModel (paperId:String,
                                allMentions:Seq[Mention],
                                paperPassages:Map[String, FriesEntry],
                                startTime:Date,
                                endTime:Date,
                                otherMetaData:Map[String, String]): PropMap = {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime, otherMetaData)

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
    model
  }


  /** Returns a 2-tuple of a model object, representing all entity mentions extracted
      from this paper, and a map from entity pointers to unique IDs. */
  private def entitiesToModel (paperId:String,
                               allMentions:Seq[Mention],
                               paperPassages:Map[String, FriesEntry],
                               startTime:Date,
                                endTime:Date,
                                otherMetaData:Map[String, String]): (PropMap, IDed) = {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime, otherMetaData)
    val entityMap = new IDed

    // this stores all frames in this output
    val frames = new FrameList
    model("frames") = frames

    // dereference all coreference mentions:
    val derefedMentions = allMentions.map(m => m.antecedentOrElse(m))

    for(mention <- derefedMentions) {
      mention match {
        case em:TextBoundMention =>
          val cid = getChunkId(em)
          assert(paperPassages.contains(cid))
          val passageMeta = paperPassages.get(cid).get
          frames += mkEntityMention(paperId, passageMeta, em.toBioMention.asInstanceOf[BioTextBoundMention], entityMap)
        case _ => // these are events; we will export them later
      }
    }
    (model, entityMap)
  }


  /** Returns a model object representing all event mentions extracted from this paper. */
  private def eventsToModel (paperId:String,
                             allMentions:Seq[Mention],
                             paperPassages:Map[String, FriesEntry],
                             entityMap:IDed,
                             startTime:Date,
                             endTime:Date,
                             otherMetaData:Map[String, String]): PropMap = {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime, otherMetaData)

    // this stores all frames in this output
    val frames = new FrameList
    model("frames") = frames

    // stores all ids for simple events
    val eventMap = new IDed

    // dereference all coreference mentions:
    val derefedMentions = allMentions.map(m => m.antecedentOrElse(m))

    // keeps just events:
    val eventMentions = derefedMentions.filter(MentionManager.isEventMention)

    // first, print all non regulation events
    for (mention <- eventMentions) {
      if (!REGULATION_EVENTS.contains(mention.label)) {
        val cid = getChunkId(mention)
        assert(paperPassages.contains(cid))
        val passageMeta = paperPassages.get(cid).get
        frames += mkEventMention(paperId, passageMeta, mention.toBioMention, entityMap, eventMap)
      }
    }

    // now, print all regulation events, which control the above events
    for (mention <- eventMentions) {
      if (REGULATION_EVENTS.contains(mention.label)) {
        val cid = getChunkId(mention)
        assert(paperPassages.contains(cid))
        val passageMeta = paperPassages.get(cid).get
        frames += mkEventMention(paperId, passageMeta, mention.toBioMention, entityMap, eventMap)
      }
    }
    model
  }

  private def mkEventMention(paperId:String,
                             passageMeta:FriesEntry,
                             mention:BioMention,
                             entityMap: IDed,
                             eventMap:IDed): PropMap = {
    currEvent = mention.text

    val f = startFrame(COMPONENT)
    f("frame-type") = "event-mention"
    val eid = mkEventId(paperId, passageMeta, mention.sentence)
    eventMap += mention -> eid

    //println(s"ADDED EVENT: $mention -> $eid")
    //println(s"HASH CODE: " + mention.hashCode())
    //displayMention(mention)

    f("frame-id") = eid
    f("sentence") = mkSentenceId(paperId, passageMeta, mention.sentence)
    f("start-pos") = mkRelativePosition(paperId, passageMeta, mention.startOffset)
    f("end-pos") = mkRelativePosition(paperId, passageMeta, mention.endOffset)
    f("text") = mention.text
    if (mention.isInstanceOf[BioEventMention]) {
      f("trigger") = mention.asInstanceOf[BioEventMention].trigger.text
      f("is-direct") = mention.asInstanceOf[BioEventMention].isDirect
    }
    f("verbose-text") = cleanVerbose(mention.sentenceObj.getSentenceText)
    f("found-by") = mention.foundBy

    val evType = mkEventType(mention)
    f("type") = evType
    if(evType != "complex-assembly")
      f("subtype") = prettifyLabel(mention.label)

    var arguments:Option[Map[String, Seq[Mention]]] = None
    mention match {
      case em:BioEventMention => arguments = Some(em.arguments)
      case rm:BioRelationMention => arguments = Some(rm.arguments)
    }

    if (arguments.isDefined) {
      val argList = new FrameList
      for (key <- arguments.get.keys) {
        arguments.get.get(key).foreach { argSeq =>
          for (i <- argSeq.indices) {
            val derefedArg = argSeq(i).antecedentOrElse(argSeq(i).toCorefMention)
            argList += mkArgument(key, derefedArg, i, entityMap, eventMap)
          }
        }
      }
      f("arguments") = argList
    }

    // event modifications
    if (MentionManager.isNegated(mention))
      f("is-negated") = true
    if (MentionManager.isHypothesized(mention))
      f("is-hypothesis") = true

    // context features
    if (mention.context.exists(! _.isEmpty))
      f("context") = mention.context.get

    // TODO (optional): add "index", i.e., the sentence-local number for this mention from this component
    f
  }


  private def mkArgument(name:String,
                         arg:Mention,
                         argIndex:Int,
                         entityMap: IDed,
                         eventMap:IDed):PropMap = {
    val m = new PropMap
    m("object-type") = "argument"
    m("argument-label") = prettifyLabel(name)
    val argType = mkArgType(arg)
    m("argument-type") = argType
    m("index") = argIndex
    m("text") = arg.text

    argType match {
      case "complex" =>
        // this is a complex: print the participants
        assert(arg.isInstanceOf[RelationMention])
        val participants = new PropMap
        val complexParticipants = arg.asInstanceOf[RelationMention].arguments
        for(key <- complexParticipants.keySet) {
          val ms: Seq[Mention] = complexParticipants.get(key).get
          for ((p, i) <- ms.zipWithIndex) {
            assert(p.isInstanceOf[TextBoundMention])
            assert(entityMap.contains(p))
            participants(s"$key${i + 1}") = entityMap.get(p).get
          }
        }
        m("args") = participants
      case "entity" =>
        // this is an entity: fetch its id from the entity map
        if(! entityMap.contains(arg)) {
          /*
          println("Document:")
          for(i <- arg.document.sentences.indices) {
            println(s"Sentence #$i: ${arg.document.sentences(i).words.mkString(" ")}")
          }
          println("entityMap:")
          for(e <- entityMap.keySet) {
            println(s"\t${e.text} with labels ${e.labels.mkString(", ")}")
          }
          */

          throw new RuntimeException(s"Found entity argument [${arg.text} [mods: ${arg.toCorefMention.modifications.map(_.toString).mkString(" ")}}]] not in entityMap \nin event [$currEvent] \nin sentence[${arg.document.sentences(arg.sentence).words.mkString(" ")}]:\n" + arg.json(pretty = true))

        }
        // assert(entityMap.contains(arg))
        m("arg") = entityMap.get(arg).get
      case "event" =>
        // this is an event, which we MUST have seen before
        // if (! eventMap.contains(arg)) {
        //   println("CANNOT FIND ARG: " + arg + " with HASH CODE: " + arg.hashCode())
        //   displayMention(arg)
        // }
        if (!eventMap.contains(arg)) {
          val msg = s"$arg with labels (${arg.labels}) found by ${arg.foundBy} is missing from the eventMap"
          throw new Exception(msg)
        }
        m("arg") = eventMap.get(arg).get
      case _ =>
        // we should never be here!
        throw new RuntimeException("ERROR: unknown event type: " + arg.labels)
    }

    m
  }

  private def mkEntityMention(paperId:String,
                              passageMeta:FriesEntry,
                              mention:BioTextBoundMention,
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
    val groundings = new FrameList
    mention.grounding.foreach(grnd => groundings += mkGrounding(grnd))
    f("xrefs") = groundings
    if(mention.isModified) {
      val ms = new FrameList
      for(m <- mention.modifications) {
        m match {
          case ptm: PTM => ms += mkPTM(ptm)
          case m: Mutant => ms += mkMutant(m)
          case _ => // we do not export anything else
        }
      }
      if(ms.nonEmpty)
        f("modifications") = ms
    }

    // TODO (optional): add "index", i.e., the sentence-local number for this mention from this component
    f
  }

  private def mkPTM(ptm:PTM):PropMap = {
    val m = new PropMap
    m("object-type") = "modification"
    m("type") = ptm.label
    if(ptm.site.isDefined) {
      m("site") = ptm.site.get.text
    }
    m
  }

  private def mkMutant(mutant: Mutant): PropMap = {
    val m = new PropMap
    m("object-type") = "modification"
    m("type") = "mutation"
    m("evidence") = mutant.evidence.text
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
    sent("frame-type") = "sentence"
    sent("frame-id") = mkSentenceId(paperId, passageMeta, offset)
    sent("passage") = mkPassageId(paperId, passageMeta)
    sent("start-pos") = mkRelativePosition(paperId, passageMeta, MentionManager.sentenceStartCharacterOffset(passageDoc, offset))
    sent("end-pos") = mkRelativePosition(paperId, passageMeta, MentionManager.sentenceEndCharacterOffset(passageDoc, offset))
    sent("text") = passageDoc.sentences(offset).getSentenceText
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

  private def mkGrounding(grounding:KBResolution):PropMap = {
    val m = new PropMap
    m("object-type") = "db-reference"
    m("namespace") = grounding.namespace
    m("id") = grounding.id
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

  def addMetaInfo(model:PropMap, paperId:String, startTime:Date, endTime:Date,
                  otherMetaData:Map[String, String]): Unit = {
    model("object-type") = "frame-collection"

    val meta = new PropMap
    meta("object-type") = "meta-info"
    meta("component") = COMPONENT
    meta("component-type") = "machine"
    meta("organization") = ORGANIZATION
    meta("doc-id") = paperId
    meta("processing-start") = startTime
    meta("processing-end") = endTime
    otherMetaData.foreach { case(k, v) => meta(k) = v } // add other meta data key/value pairs
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

}

