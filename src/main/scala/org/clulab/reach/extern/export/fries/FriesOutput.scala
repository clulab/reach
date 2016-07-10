package org.clulab.reach.extern.export.fries

import java.io._
import java.util.Date
import org.clulab.assembly.export.{CausalPrecedence, Equivalence, AssemblyLink}
import org.json4s.native.Serialization

import org.clulab.assembly._
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.reach.ReachConstants._
import org.clulab.reach.context._
import org.clulab.reach.display._
import org.clulab.reach.extern.export._
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions._
import org.clulab.reach.nxml.FriesEntry

import JsonOutputter._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Defines classes and methods used to build and output the FRIES format.
  *   Written by Mihai Surdeanu. 5/22/2015.
  *   Last Modified: Issue 199 redux: prevent ComplexEvents from controlling other ComplexEvents.
  */
class FriesOutput extends JsonOutputter {
  // local type definitions:
  type IDed = scala.collection.mutable.HashMap[Mention, String]
  type CtxIDed = scala.collection.mutable.HashMap[ContextMap, String]

  // incrementing ID for numbering context frames
  protected val contextIdCntr = new IncrementingId()

  // incrementing ID for numbering entity mentions
  protected val entityIdCntr = new IncrementingId()

  // incrementing ID for numbering event mentions
  protected val eventIdCntr = new IncrementingId()

  // incrementing ID for numbering links
  protected val linkIdCntr = new IncrementingId()

  var currEvent = ""

  // map incoming Reach context type names to FRIES-spec context type names
  val contextNameMap = Map(
    "CellLine" -> "cell-line",
    "CellType" -> "cell-type",
    "TissueType" -> "tissue-type",
    "Cellular_component" -> "location",
    "Species" -> "organism",
    "Organ" -> "organ"
  )

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

    val contextIdMap = new CtxIDed

    val sentModel = sentencesToModel(paperId, allMentions, passageMap,
                                     startTime, endTime, otherMetaData)

    // entityMap: map from entity pointers to unique ids
    val (entityModel, entityMap) = entitiesToModel(paperId, allMentions, passageMap, contextIdMap,
                                                   startTime, endTime, otherMetaData)

    // eventMap: map from entity pointers to unique ids
    val (eventModel, eventMap) = eventsToModel(paperId, allMentions, passageMap, contextIdMap,
                                               entityMap, startTime, endTime, otherMetaData)

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

    val contextIdMap = new CtxIDed

    val sentModel = sentencesToModel(paperId, allMentions, passageMap,
                                     startTime, endTime, otherMetaData)
    writeJsonToFile(sentModel, new File(outFilePrefix + ".uaz.sentences.json"))

    // entityMap: map from entity pointers to unique ids
    val (entityModel, entityMap) = entitiesToModel(paperId, allMentions, passageMap, contextIdMap,
                                                   startTime, endTime, otherMetaData)
    writeJsonToFile(entityModel, new File(outFilePrefix + ".uaz.entities.json"))

    // eventMap: map from entity pointers to unique ids
    val (eventModel, eventMap) = eventsToModel(paperId, allMentions, passageMap, contextIdMap,
                                               entityMap, startTime, endTime, otherMetaData)
    writeJsonToFile(eventModel, new File(outFilePrefix + ".uaz.events.json"))
  }


  /**
    * Writes the given mentions to output files in Fries JSON format.
    * Separate output files are written for sentences, links, entities, and events.
    * Each output file is prefixed with the given prefix string.
    */
  def writeJSON (paperId:String,
                 allMentions:Seq[Mention],
                 paperPassages:Seq[FriesEntry],
                 startTime:Date,
                 endTime:Date,
                 outFilePrefix:String,
                 assemblyAPI: Assembler): Unit = {

    val otherMetaData = extractOtherMetaData(paperPassages)
    val passageMap = passagesToMap(paperPassages) // map of FriesEntry, chunkId as key

    val contextIdMap = new CtxIDed

    val sentModel = sentencesToModel(paperId, allMentions, passageMap,
                                     startTime, endTime, otherMetaData)
    writeJsonToFile(sentModel, new File(outFilePrefix + ".uaz.sentences.json"))

    // entityMap: map from entity pointers to unique ids
    val (entityModel, entityMap) = entitiesToModel(paperId, allMentions, passageMap, contextIdMap,
                                                   startTime, endTime, otherMetaData)
    writeJsonToFile(entityModel, new File(outFilePrefix + ".uaz.entities.json"))

    // eventMap: map from entity pointers to unique ids
    val (eventModel, eventMap) = eventsToModel(paperId, allMentions, passageMap, contextIdMap,
                                               entityMap, startTime, endTime, otherMetaData)
    writeJsonToFile(eventModel, new File(outFilePrefix + ".uaz.events.json"))

    val assemblyModel:PropMap = mkAssemblyModel(paperId, allMentions, passageMap, entityMap, eventMap,
                                                startTime, endTime, otherMetaData, assemblyAPI)
    writeJsonToFile(assemblyModel, new File(outFilePrefix + ".uaz.links.json"))
  }


  //
  // Private Methods
  //

  /** Add fixed pieces of meta data to the given model, along with additional meta data
      from the given Map. */
  private def addMetaInfo(model:PropMap, paperId:String, startTime:Date, endTime:Date,
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

  /** Returns a 2-tuple of a model object, representing all entity mentions extracted
      from this paper, and a map from entity pointers to unique IDs. */
  private def entitiesToModel (paperId:String,
                               allMentions:Seq[Mention],
                               passageMap:Map[String, FriesEntry],
                               contextIdMap:CtxIDed,
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
        case em:BioTextBoundMention =>
          val passage = getPassageForMention(passageMap, em)
          frames ++= mkEntityMention(paperId, passage, em.toBioMention.asInstanceOf[BioTextBoundMention], contextIdMap, entityMap)
        case _ => // these are events; we will export them later
      }
    }
    (model, entityMap)
  }


  /** Returns a model object representing all event mentions extracted from this paper. */
  private def eventsToModel (paperId:String,
                             allMentions:Seq[Mention],
                             passageMap:Map[String, FriesEntry],
                             contextIdMap:CtxIDed,
                             entityMap:IDed,
                             startTime:Date,
                             endTime:Date,
                             otherMetaData:Map[String, String]): (PropMap, IDed) = {
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
      if (!mention.matches("ComplexEvent")) {
        val passage = getPassageForMention(passageMap, mention)
        frames ++= mkEventMention(paperId, passage, mention.toBioMention, contextIdMap, entityMap, eventMap)
      }
    }

    // val mentionManager = new MentionManager()   // DEBUGGING

    // now, print all allowed ComplexEvents, which control the above events:
    for (mention <- eventMentions.filterNot(disallowedEvent(_))) {
      if (mention.matches("ComplexEvent")) {

        // println("++++++++++++++++++++++++++++++++++++++++")  // DEBUGGING
        // println("PARENT CE:")
        // mentionManager.mentionToStrings(mention).foreach(println(_))
        // val controlledCEs = mention.arguments.getOrElse("controlled", Nil).filter(_.matches("ComplexEvent"))
        // println("Child CEs:")
        // for (m <- controlledCEs) {
        //   mentionManager.mentionToStrings(m).foreach(println(_))
        //   println()
        // }

        // process current complex event
        val passage = getPassageForMention(passageMap, mention)
        frames ++= mkEventMention(paperId, passage, mention.toBioMention, contextIdMap, entityMap, eventMap)

        // println("----------------------------------------")
      }
    }
    (model, eventMap)
  }

  /** Disallow any ComplexEvent with a ComplexEvent as a controlled argument. */
  private def disallowedEvent (mention: Mention): Boolean = {
    mention.matches("ComplexEvent") &&
    mention.arguments.getOrElse("controlled", Nil).exists(_.matches("ComplexEvent"))
  }


  private def getChunkId(m:Mention):String = {
    assert(m.document.id.isDefined)
    val did = m.document.id.get
    // the chunk id is the string following the underscore in the document ids
    val chunkId = did.substring(did.lastIndexOf("_") + 1)
    chunkId
  }

  /** Return the passage selected by the given mention's chunk ID. */
  private def getPassageForMention (passageMap:Map[String, FriesEntry],
                                    mention:Mention): FriesEntry = {
    val chunkId = getChunkId(mention)
    assert(passageMap.contains(chunkId))
    passageMap.get(chunkId).get
  }


  /** Lookup the context for the given mention. Return a newly created context ID and frame;
      or an existing context ID,=; or neither, if the mention has no context. */
  private def lookupContext (paperId: String,
                             passage: FriesEntry,
                             mention: BioMention,
                             contextIdMap: CtxIDed): Tuple2[Option[String], Option[PropMap]] =
  {
    var contextId: Option[String] = None
    var contextFrame: Option[PropMap] = None

    if (mention.hasContext()) {
      val context = mention.context.get
      contextId = contextIdMap.get(context) // get the context ID for the context
      if (!contextId.isDefined) {           // if this is a new context
        val ctxid = mkContextId(paperId, passage, mention.sentence) // generate new context ID
        contextIdMap.put(context, ctxid)      // save the new context ID keyed by the context
        contextFrame = Some(mkContextFrame(paperId, passage, mention, ctxid, context))
        contextId = Some(ctxid)               // save the new context ID
      }
    }

    return (contextId, contextFrame)
  }

  private def lookupMentionId (mention: Mention,
                               entityMap:IDed,
                               eventMap:IDed): Option[String] = mention match {
    case tbm: TextBoundMention => entityMap.get(tbm)
    case evm: EventMention => eventMap.get(evm)
    case _ => None
  }

  private def mkContextId(paperId:String, passage:FriesEntry, offset:Int):String =
    s"cntx-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}-$offset-${contextIdCntr.genNextId()}"

  private def mkEntityId(paperId:String, passage:FriesEntry, offset:Int):String =
    s"ment-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}-$offset-${entityIdCntr.genNextId()}"

  private def mkEventId(paperId:String, passage:FriesEntry, offset:Int):String =
    s"evem-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}-$offset-${eventIdCntr.genNextId()}"

  private def mkLinkId(paperId:String, passage:FriesEntry, offset:Int):String =
    s"link-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}-$offset-${linkIdCntr.genNextId()}"

  private def mkPassageId(paperId:String, passage:FriesEntry):String =
    s"pass-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}"

  private def mkSentenceId(paperId:String, passage:FriesEntry, offset:Int):String =
    s"sent-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}-$offset"


  private def mkArgument(name:String,
                         arg:Mention,
                         argIndex:Int,
                         entityMap: IDed,
                         eventMap:IDed):PropMap = {
    val m = new PropMap
    m("object-type") = "argument"
    m("type") = prettifyLabel(name)
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

  private def mkAssemblyModel (paperId:String,
                               allMentions:Seq[Mention],
                               passageMap:Map[String, FriesEntry],
                               entityMap:IDed,
                               eventMap:IDed,
                               startTime:Date,
                               endTime:Date,
                               otherMetaData:Map[String, String],
                               assemblyAPI: Assembler): PropMap = {
    val assemblyModel:PropMap = new PropMap
    addMetaInfo(assemblyModel, paperId, startTime, endTime, otherMetaData)
    val frames = new FrameList
    assemblyModel("frames") = frames

    for (mention <- eventMap.keys.toList.sortBy(m => (m.sentence, m.tokenInterval.start))) {
      val fromId = lookupMentionId(mention, entityMap, eventMap)
      if (fromId.isDefined) {
        frames ++= mk1toMLinkFrames(paperId, mention, fromId.get,
                                    assemblyAPI.getEquivalenceLinks(mention), "equivalent-to",
                                    passageMap, entityMap, eventMap)
        frames ++= mk1to1LinkFrames(paperId, mention, fromId.get,
                                    assemblyAPI.getCausalPredecessors(mention), "preceded-by",
                                    passageMap, entityMap, eventMap)
      }
    }

    assemblyModel                           // return the constructed assembly model
  }


  private def mkContextFrame (paperId:String,
                              passage:FriesEntry,
                              mention:BioMention,
                              contextId:String,
                              context:ContextMap): PropMap = {
    val f = startFrame()
    f("frame-type") = "context"
    f("frame-id") = contextId
    f("scope") = mkSentenceId(paperId, passage, mention.sentence)
    f("facets") = mkContextFacets(context)
    f
  }

  private def mkContextFacets (context: ContextMap): PropMap = {
    val m = new PropMap
    m("object-type") = "facet-set"
    context.foreach { case(k, v) =>
      val friesFacetName = contextNameMap.getOrElse(k, k)
      m(friesFacetName) = v
    }
    m
  }

  private def mkEntityMention(paperId:String,
                              passageMeta:FriesEntry,
                              mention:BioTextBoundMention,
                              contextIdMap:CtxIDed,
                              entityMap: IDed): List[PropMap] = {
    val f = startFrame()
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
    if (mention.isModified) {
      val ms = new FrameList
      for(m <- mention.modifications) {
        m match {
          case ptm: PTM => ms += mkPTM(ptm)
          case m: Mutant => ms += mkMutant(m)
          case _ => // we do not export anything else
        }
      }
      if (ms.nonEmpty)
        f("modifications") = ms
    }

    // TODO (optional): add "index", i.e., the sentence-local number for this mention from this component

    // handle context features:
    val (contextId, contextFrame) = lookupContext(paperId, passageMeta, mention, contextIdMap)

    // the context for a frame is a list of context frame refs (currently a singleton list)
    if (contextId.isDefined)
      f("context") = List(contextId.get)

    // return 1 or more frames: both context and event frames or just an event frame:
    if (contextFrame.isDefined)
      List(contextFrame.get, f)
    else
      List(f)
  }

  private def mkEventMention(paperId:String,
                             passageMeta:FriesEntry,
                             mention:BioMention,
                             contextIdMap: CtxIDed,
                             entityMap: IDed,
                             eventMap:IDed): List[PropMap] = {
    currEvent = mention.text

    val f = startFrame()
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

    // handle event arguments
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

    // TODO (optional): add "index", i.e., the sentence-local number for this mention
    //                  from this component.

    // handle context features:
    val (contextId, contextFrame) = lookupContext(paperId, passageMeta, mention, contextIdMap)

    // the context for a frame is a list of context frame refs (currently a singleton list)
    if (contextId.isDefined)
      f("context") = List(contextId.get)

    // return 1 or more frames: both context and event frames or just an event frame:
    if (contextFrame.isDefined)
      List(contextFrame.get, f)
    else
      List(f)
  }

  private def mkGrounding (grounding:KBResolution): PropMap = {
    val m = new PropMap
    m("object-type") = "db-reference"
    m("namespace") = grounding.namespace
    m("id") = grounding.id
    m
  }


  private def mkLinkArgumentFrame (arg:String,
                                   argType:String,
                                   index:Option[Integer]=None): PropMap = {
    val m = new PropMap
    m("object-type") = "argument"
    m("type") = argType
    m("arg") = arg
    if (index.isDefined)
      m("index") = index.get
    m
  }

  /** Create and return a new binary Link frame. */
  private def mkLinkFrame (linkId: String,
                           frameType: String,
                           foundBy: String,
                           args: List[PropMap]): PropMap = {
    val f = startFrame()
    f("frame-id") = linkId
    f("frame-type") = "link"
    f("type") = frameType
    f("found-by") = foundBy
    if (!args.isEmpty)
      f("arguments") = args
    // f("is-negated") = false                 // optional: in schema for future use
    // f("score") = 0.0                        // optional: in schema for future use
    f
  }

  /** Return a list of link frames made from the given mention and set of assembly links. */
  private def mk1to1LinkFrames [L <: AssemblyLink] (
    paperId: String,
    from: Mention,
    fromId: String,
    links: Set[L],
    frameType: String,
    passageMap: Map[String, FriesEntry],
    entityMap: IDed,
    eventMap: IDed
  ): List[PropMap] = {
    val frames: ListBuffer[PropMap] = new ListBuffer()
    val passage = getPassageForMention(passageMap, from)
    links foreach { link =>
      val linkId = mkLinkId(paperId, passage, from.sentence) // generate next link ID
      link match {
        case CausalPrecedence(before, after, foundBy) =>
          // validate: from == after, "link should represent predecessors of 'from' Mention"
          val m2Id = lookupMentionId(before, entityMap, eventMap)
          if (m2Id.isDefined) {
            val args = List(mkLinkArgumentFrame(fromId, "after"),
                            mkLinkArgumentFrame(m2Id.get, "before"))
            frames += mkLinkFrame(linkId, frameType, foundBy, args)
          }
        case unknown =>
          System.err.println(s"Cannot create 1-to-1 links for type '${unknown.getClass}'")
      }
    }
    frames.toList
  }

  /** Return a singleton list of link frame made from the given mention and set of assembly links. */
  private def mk1toMLinkFrames [L <: AssemblyLink] (
    paperId: String,
    from: Mention,
    fromId: String,
    links: Set[L],
    frameType: String,
    passageMap: Map[String, FriesEntry],
    entityMap: IDed,
    eventMap: IDed
  ): List[PropMap] = {
    val frames: ListBuffer[PropMap] = new ListBuffer()
    if (links.size > 0) {               // ignore empty maps
      val passage = getPassageForMention(passageMap, from)
      val linkId = mkLinkId(paperId, passage, from.sentence) // generate single link ID

      val foundBy = links.head.foundBy  // get foundBy from arbitrary element
      links.head match {
        case Equivalence(mention1, m2, foundBy) =>
          val args: ListBuffer[PropMap] = new ListBuffer()
          args += mkLinkArgumentFrame(fromId, "from") // add FROM as argument zero
          args ++= links.zipWithIndex.flatMap { case(link:Equivalence, ndx:Int) =>
            lookupMentionId(link.m2, entityMap, eventMap).map(m2Id =>
              mkLinkArgumentFrame(m2Id, s"to${ndx}"))
          }
          if (args.size > 1)                  // must have at least one TO argument
            frames += mkLinkFrame(linkId, frameType, foundBy, args.toList)

        case unknown =>
          System.err.println(s"Cannot create 1-to-M links for type '${unknown.getClass}'")
      }
    }
    frames.toList
  }


  private def mkMutant(mutant: Mutant): PropMap = {
    val m = new PropMap
    m("object-type") = "modification"
    m("type") = "mutation"
    m("evidence") = mutant.evidence.text
    m
  }

  /** Create and return a new Passage frame. */
  private def mkPassage(model:PropMap,
                        paperId:String,
                        passage:FriesEntry,
                        passageDoc:Document): PropMap = {
    val f = startFrame(Some("nxml2fries"))
    f("frame-id") = mkPassageId(paperId, passage)
    f("frame-type") = "passage"
    f("index") = passage.chunkId
    f("section-id") = passage.sectionId
    f("section-name") = passage.sectionName
    f("is-title") = passage.isTitle
    assert(passageDoc.text.isDefined)
    f("text") = passageDoc.text.get.replaceAll("\\n", " ")
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

  private def mkRelativePosition(paperId:String,
                                 passageMeta:FriesEntry,
                                 characterOffset:Int): PropMap = {
    val pos = new PropMap
    pos("object-type") = "relative-pos"
    pos("reference") = mkPassageId(paperId, passageMeta)
    pos("offset") = characterOffset
    pos
  }

  private def mkSentence(model:PropMap,
                         paperId:String,
                         passageMeta:FriesEntry,
                         passageDoc:Document,
                         offset:Int): PropMap = {
    val sent = startFrame(Some("BioNLPProcessor"))
    sent("frame-type") = "sentence"
    sent("frame-id") = mkSentenceId(paperId, passageMeta, offset)
    sent("passage") = mkPassageId(paperId, passageMeta)
    sent("start-pos") = mkRelativePosition(paperId, passageMeta, MentionManager.sentenceStartCharacterOffset(passageDoc, offset))
    sent("end-pos") = mkRelativePosition(paperId, passageMeta, MentionManager.sentenceEndCharacterOffset(passageDoc, offset))
    sent("text") = passageDoc.sentences(offset).getSentenceText
    sent
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


  /** Creates a map of all FriesEntries, using chunkId as key */
  private def passagesToMap(paperPassages:Seq[FriesEntry]):Map[String, FriesEntry] = {
    val map = new mutable.HashMap[String, FriesEntry]()
    for(e <- paperPassages) map += e.chunkId -> e
    map.toMap
  }

  /** Returns a data-structure representing all sections and sentences in this paper. */
  private def sentencesToModel (paperId:String,
                                allMentions:Seq[Mention],
                                passageMap:Map[String, FriesEntry],
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
      assert(passageMap.contains(chunkId))
      frames += mkPassage(model, paperId, passageMap.get(chunkId).get, passageDocs.get(chunkId).get)
      frames ++= mkSentences(model, paperId, passageMap.get(chunkId).get, passageDocs.get(chunkId).get)
    }
    model
  }


  private def startFrame (component:Option[String]=None): PropMap = {
    val f = new PropMap
    f("object-type") = "frame"
    if (component.isDefined) {              // no need to override default meta-data each time
      val meta = new PropMap
      meta("object-type") = "meta-info"
        meta("component") = component
      f("object-meta") = meta
    }
    f
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

