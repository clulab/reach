package org.clulab.reach.extern.export.fries

import java.io._
import java.util.Date
import org.clulab.assembly.export.{CausalPrecedence, Equivalence}
import org.json4s.native.Serialization
import org.clulab.assembly.{Assembler, RoleWithFeatures}
import org.clulab.assembly.export.AssemblyLink
import org.clulab.assembly.representations.{PTM => AssemblyPTM}
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.reach.context._
import org.clulab.reach.display._
import org.clulab.reach.extern.export._
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions._
import JsonOutputter._
import org.clulab.reach.FriesEntry
import org.clulab.reach.darpa.OutputDegrader
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
  * Defines classes and methods used to build and output the FRIES format.
  *   Written by: Mihai Surdeanu and Tom Hicks.
  *   Last Modified: Continue revamp.
  */
class FriesOutputter extends JsonOutputter {

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
  val contextNameMap = Map (
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

    val (sentModel, entityModel, eventModel, _) =
      makeModels(paperId, allMentions, paperPassages, startTime, endTime)

    val uniModel:PropMap = new PropMap      // combine models into one
    uniModel("sentences") = sentModel
    uniModel("entities") = entityModel
    uniModel("events") = eventModel
    writeJsonToString(uniModel)             // write out combined model
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

    val (sentModel, entityModel, eventModel, _) =
      makeModels(paperId, allMentions, paperPassages, startTime, endTime)

    writeJsonToFile(sentModel, new File(outFilePrefix + ".uaz.sentences.json"))
    writeJsonToFile(entityModel, new File(outFilePrefix + ".uaz.entities.json"))
    writeJsonToFile(eventModel, new File(outFilePrefix + ".uaz.events.json"))
  }


  /**
    * Writes the given mentions to output files in Fries JSON format.
    * Separate output files are written for sentences, links, entities, and events.
    * Each output file is prefixed with the given prefix string.
    */
  override def writeJSON (paperId:String,
                          allMentions:Seq[Mention],
                          paperPassages:Seq[FriesEntry],
                          startTime:Date,
                          endTime:Date,
                          outFilePrefix:String,
                          assemblyApi: Assembler): Unit = {

    val (sentModel, entityModel, eventModel, assemblyModel) =
      makeModels(paperId, allMentions, paperPassages, startTime, endTime, Some(assemblyApi))

    writeJsonToFile(sentModel, new File(outFilePrefix + ".uaz.sentences.json"))
    writeJsonToFile(entityModel, new File(outFilePrefix + ".uaz.entities.json"))
    writeJsonToFile(eventModel, new File(outFilePrefix + ".uaz.events.json"))
    if (assemblyModel.isDefined)
      writeJsonToFile(assemblyModel.get, new File(outFilePrefix + ".uaz.links.json"))
  }


  //
  // Private Methods
  //

  /** Make and return the Sentence, Entity, and Event data models for output. */
  def makeModels (paperId:String,
                  allMentions:Seq[Mention],
                  paperPassages:Seq[FriesEntry],
                  startTime:Date,
                  endTime:Date,
                  assemblyApi: Option[Assembler] = None): (PropMap, PropMap, PropMap, Option[PropMap]) = {

    // flatten mentions (per MITRE requirements), deduplicate, etc.
    val sanitizedMentions = OutputDegrader.prepareForOutput(allMentions)

    // dereference all coreference mentions:
    val derefedMentions = sanitizedMentions.map(m => m.antecedentOrElse(m))

    val otherMetaData = extractOtherMetaData(paperPassages)
    val passageMap = passagesToMap(paperPassages) // map of FriesEntry, chunkId as key

    // make Sentence Model
    val sentModel = sentencesToModel(paperId, derefedMentions, passageMap,
                                     startTime, endTime, otherMetaData)

    // make Entity Map and Event Map: mappings from entities & events to unique ids
    val (entityMap, eventMap) = makeEMaps(paperId, derefedMentions, passageMap)

    // make Entity Model
    val contextIdMap = new CtxIDed
    val entityModel = entitiesToModel(paperId, derefedMentions, passageMap, entityMap,
                                      contextIdMap, startTime, endTime, otherMetaData)

    // make Event Model
    val eventModel = eventsToModel(paperId, derefedMentions, passageMap, entityMap,
                                   eventMap, contextIdMap, startTime, endTime,
                                   otherMetaData, assemblyApi)

    // make optional Assembly Model
    val assemblyModel =
      if (assemblyApi.isDefined)
        Some(assemblyToModel(paperId, derefedMentions, passageMap, entityMap,
                             eventMap, startTime, endTime, otherMetaData, assemblyApi.get))
      else None

    (sentModel, entityModel, eventModel, assemblyModel)
  }


  /** Add fixed pieces of meta data to the given model, along with additional meta data
      from the given Map. */
  private def addMetaInfo (model:PropMap,
                           paperId:String,
                           startTime:Date,
                           endTime:Date,
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


  /** Return an Assembly Model, representing selected assembly information for this paper. */
  private def assemblyToModel (paperId:String,
                               mentions:Seq[Mention],
                               passageMap:Map[String, FriesEntry],
                               entityMap:IDed,
                               eventMap:IDed,
                               startTime:Date,
                               endTime:Date,
                               otherMetaData:Map[String, String],
                               assemblyApi: Assembler): PropMap = {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime, otherMetaData)

    val frames = new FrameList
    model("frames") = frames

    for (mention <- eventMap.keys.toList.sortBy(m => (m.sentence, m.tokenInterval.start))) {
      val fromId = lookupMentionId(mention, entityMap, eventMap)
      if (fromId.isDefined) {
        frames ++= mk1toMLinkFrames(paperId, mention, fromId.get,
                                    assemblyApi.getEquivalenceLinks(mention), "equivalent-to",
                                    passageMap, entityMap, eventMap)
        frames ++= mk1to1LinkFrames(paperId, mention, fromId.get,
                                    assemblyApi.getCausalPredecessors(mention), "preceded-by",
                                    passageMap, entityMap, eventMap)
      }
    }
    model                              // return the constructed assembly model
  }


  /** Return an Entity Model, representing all entity mentions extracted from this paper. */
  private def entitiesToModel (paperId: String,
                               mentions: Seq[Mention],
                               passageMap: Map[String, FriesEntry],
                               entityMap: IDed,
                               contextIdMap: CtxIDed,
                               startTime: Date,
                               endTime: Date,
                               otherMetaData: Map[String, String]): PropMap = {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime, otherMetaData)

    val frames = new FrameList
    model("frames") = frames

    for (mention <- mentions) {
      mention match {
        case em:BioTextBoundMention =>
          val passage = getPassageForMention(passageMap, em)
          frames ++= mkEntityMention(paperId, passage, em, entityMap, contextIdMap)
        case _ =>                           // these are events or relations
      }
    }
    model
  }


  /** Returns a model object representing all event mentions extracted from this paper. */
  private def eventsToModel (paperId:String,
                             mentions:Seq[Mention],
                             passageMap:Map[String, FriesEntry],
                             entityMap:IDed,
                             eventMap:IDed,
                             contextIdMap:CtxIDed,
                             startTime:Date,
                             endTime:Date,
                             otherMetaData:Map[String, String],
                             assemblyApi: Option[Assembler] = None): PropMap = {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime, otherMetaData)

    val frames = new FrameList
    model("frames") = frames

    val eventMentions = mentions.filter(MentionManager.isEventMention) // only process events
    for (mention <- eventMentions) {
      val passage = getPassageForMention(passageMap, mention)
      frames ++= mkEventMention(paperId, passage, mention.toBioMention, entityMap,
                                eventMap, contextIdMap, assemblyApi)
    }
    model
  }


  private def getChunkId (m:Mention): String = {
    assert(m.document.id.isDefined, s"object ${m} has no document ID")

    val did = m.document.id.get
    // the chunk id is the string following the underscore in the document ids
    val chunkId = did.substring(did.lastIndexOf("_") + 1)
    chunkId
  }

  /** Return the passage selected by the given mention's chunk ID. */
  private def getPassageForMention (passageMap: Map[String, FriesEntry],
                                    mention: Mention): FriesEntry = {
    val chunkId = getChunkId(mention)
    assert(passageMap.contains(chunkId), s"passageMap missing chunkId ${chunkId}")
    passageMap.get(chunkId).get
  }

  /** Return the unique id string for the given mention, fetched from the given map. */
  private def getUniqueId (eMap:IDed, m: Mention): String = {
    val uid = eMap.get(m)
    assert(uid.isDefined, s"Should never happen: Mention ${m} has no unique ID")
    uid.get
  }


  /** Lookup the context for the given mention. Return a newly created context ID and frame,
      or an existing context ID, or neither if the mention has no context. */
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
      if (contextId.isEmpty) {           // if this is a new context
        val ctxid = mkContextId(paperId, passage, mention.sentence) // generate new context ID
        contextIdMap.put(context, ctxid)      // save the new context ID keyed by the context
        contextFrame = Some(makeContextFrame(paperId, passage, mention, ctxid, context))
        contextId = Some(ctxid)               // save the new context ID
      }
    }

    (contextId, contextFrame)
  }


  private def lookupMentionId (mention: Mention,
                               entityMap: IDed,
                               eventMap: IDed): Option[String] = mention match {
    case tbm: TextBoundMention => entityMap.get(tbm)
    case evm: EventMention => eventMap.get(evm)
    case rem: RelationMention => eventMap.get(rem)
    case _ => None
  }


  private def makeArgument (name: String,
                            arg: Mention,
                            argIndex: Int,
                            entityMap: IDed,
                            eventMap: IDed,
                            argFeatures:Option[RoleWithFeatures] = None): PropMap = {
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
        assert(MentionManager.isRelationMention(arg),
          "Complex 'arg' is not an instance of RelationMention")
        val participants = new PropMap
        val complexParticipants = arg.asInstanceOf[RelationMention].arguments
        for (key <- complexParticipants.keySet) {
          // FIXME: resolve each participant.  Should this be done elsewhere?
          val ms: Seq[Mention] = complexParticipants.get(key).get.map(m => m.antecedentOrElse(m))
          for ((p, i) <- ms.zipWithIndex) {
            assert(p.isInstanceOf[TextBoundMention], s"complex participant is not an instance of TextBoundMention: ${p}")
            if (!entityMap.contains(p)) {
              throw new RuntimeException(s"Complex participant [${p.text} [mods: ${p.toCorefMention.modifications.map(_.toString).mkString(" ")}}]] not in entityMap \nin event [$currEvent] \nin sentence[${p.document.sentences(p.sentence).words.mkString(" ")}]:\n" + p.json(pretty = true))
            }
            participants(s"$key${i + 1}") = entityMap.get(p).get
          }
        }
        m("args") = participants

      case "entity" =>
        // this is an entity: fetch its id from the entity map
        if (!entityMap.contains(arg)) {
          /*
          println("Document:")
          for (i <- arg.document.sentences.indices) {
            println(s"Sentence #$i: ${arg.document.sentences(i).words.mkString(" ")}")
          }
          println("entityMap:")
          for (e <- entityMap.keySet) {
            println(s"\t${e.text} with labels ${e.labels.mkString(", ")}")
          }
          */
          throw new RuntimeException(s"Entity argument [${arg.text} [mods: ${arg.toCorefMention.modifications.map(_.toString).mkString(" ")}}]] not in entityMap \nin event [$currEvent] \nin sentence[${arg.document.sentences(arg.sentence).words.mkString(" ")}]:\n" + arg.json(pretty = true))
        }
        m("arg") = entityMap.get(arg).get

        // output any participant features associated with this entity by assembly:
        if (argFeatures.isDefined) {
          val features = mkParticipantFeatures(argFeatures.get)
          if (features.nonEmpty)
            m("participant-features") = features
        }

      case "event" =>
        // this is an event, which we MUST have seen before
        // if (!eventMap.contains(arg)) {
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


  private def makeContextFrame (paperId:String,
                                passage:FriesEntry,
                                mention:BioMention,
                                contextId:String,
                                context:ContextMap): PropMap = {
    val f = startFrame()
    f("frame-type") = "context"
    f("frame-id") = contextId
    f("scope") = mkSentenceId(paperId, passage, mention.sentence)
    f("facets") = makeContextFacets(context)
    f
  }

  private def makeContextFacets (context: ContextMap): PropMap = {
    val m = new PropMap
    m("object-type") = "facet-set"
    context.foreach { case(k, v) =>
      val friesFacetName = contextNameMap.getOrElse(k, k)
      m(friesFacetName) = v
    }
    m
  }


  /** Return a tuple of maps mapping entities to unique IDs and events to unique IDs. */
  private def makeEMaps (paperId: String,
                         mentions: Seq[Mention],
                         passageMap: Map[String, FriesEntry]): (IDed, IDed) = {
    val entityMap = new IDed
    val eventMap = new IDed

    for (mention <- mentions) {
      mention match {
        case em:BioTextBoundMention =>
          makeEntityMapEntry(paperId, em, passageMap, entityMap)

        case evm:BioEventMention =>
          makeEventMapEntry(paperId, evm, passageMap, entityMap, eventMap)

        case rem:BioRelationMention =>
          makeEventMapEntry(paperId, rem, passageMap, entityMap, eventMap)

        case _ => println(s"(makeEMaps): UNTYPED=${mention.toString()}") // DEBUGGING
      }
    }

    (entityMap, eventMap)
  }

  private def makeEntityMapEntry (paperId: String,
                                  mention: BioTextBoundMention,
                                  passageMap: Map[String, FriesEntry],
                                  entityMap: IDed): Unit = {
    val passage = getPassageForMention(passageMap, mention)
    entityMap += mention -> mkEntityId(paperId, passage, mention.sentence)
  }

  private def makeEventMapEntry (paperId: String,
                                 mention: BioMention,
                                 passageMap: Map[String, FriesEntry],
                                 entityMap: IDed,
                                 eventMap: IDed): Unit = {
    val passage = getPassageForMention(passageMap, mention)
    eventMap += mention -> mkEventId(paperId, passage, mention.sentence)
    mention.arguments.foreach {
      case (_, argMentions) => argMentions.foreach { argMention =>
        argMention match {
          case evm:BioEventMention =>
            makeEventMapEntry(paperId, evm, passageMap, entityMap, eventMap)

          case rem:BioRelationMention =>
            makeEventMapEntry(paperId, rem, passageMap, entityMap, eventMap)

          case em:BioTextBoundMention =>
            makeEntityMapEntry(paperId, em, passageMap, entityMap)

          case _ => println(s"argMention=${argMention}") // DEBUGGING
        }
      }
    }
  }


  private def mkEntityMention(paperId:String,
                              passageMeta:FriesEntry,
                              mention:BioTextBoundMention,
                              entityMap: IDed,
                              contextIdMap:CtxIDed): List[PropMap] = {
    val f = startFrame()
    f("frame-type") = "entity-mention"
    f("frame-id") = getUniqueId(entityMap, mention)
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
      for (m <- mention.modifications) {
        m match {
          case ptm: PTM => ms += mkPTM(ptm)
          case m: Mutant => ms += mkMutant(m)
          case _ =>                         // we do not export anything else
        }
      }
      if (ms.nonEmpty)
        f("modifications") = ms
    }

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


  private def mkEventMention (paperId: String,
                              passageMeta: FriesEntry,
                              mention: BioMention,
                              entityMap: IDed,
                              eventMap: IDed,
                              contextIdMap: CtxIDed,
                              assemblyApi: Option[Assembler] = None): List[PropMap] = {
    currEvent = mention.text

    val f = startFrame()
    f("frame-type") = "event-mention"
    f("frame-id") = getUniqueId(eventMap, mention)
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
    if (evType != "complex-assembly")
      f("subtype") = prettifyLabel(mention.label)

    var arguments: Option[Map[String, Seq[Mention]]] =
      if (MentionManager.isEventMention(mention)) Some(mention.arguments) else None

    // if using the Assembly subsystem, get all input participant features for this event
    val inputFeatures = assemblyApi.map(_.getInputFeaturesByParticipants(mention))

    // handle event arguments
    if (arguments.isDefined) {
      val argList = new FrameList

      // process the arguments:
      for (key <- arguments.get.keys) {
        arguments.get.get(key).foreach { argSeq =>
          for (i <- argSeq.indices) {
            val derefedArg = argSeq(i).antecedentOrElse(argSeq(i).toCorefMention)
            val argFeatures = inputFeatures.flatMap(_.get(derefedArg))
            argList += makeArgument(key, derefedArg, i, entityMap, eventMap, argFeatures)
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

    // OPTIONAL TODO: add "index": the sentence-local number for this mention from this component.

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
    if (args.nonEmpty)
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
    if (links.nonEmpty) {               // ignore empty maps
      val passage = getPassageForMention(passageMap, from)
      val linkId = mkLinkId(paperId, passage, from.sentence) // generate single link ID

      val foundBy = links.head.foundBy  // get foundBy from arbitrary element
      links.head match {
        case Equivalence(_, m2, _) =>
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


  private def mkMutant (mutant: Mutant): PropMap = {
    val m = new PropMap
    m("object-type") = "modification"
    m("type") = "mutation"
    m("evidence") = mutant.evidence.text
    m
  }


  /** Return a list of participant features for the given role/argument features object. */
  private def mkParticipantFeatures (rwfs: RoleWithFeatures): FrameList = {
    (new FrameList) ++= rwfs.features.map{ f:AssemblyPTM => mkParticipantFeature(f) }
  }

  /** Return a participant feature map for the given modification object. */
  private def mkParticipantFeature (ptm: AssemblyPTM): PropMap = {
    val m = new PropMap
    m("object-type") = "feature"
    m("type") = ptm.label
    m("negated") = ptm.negated
    if (ptm.site.isDefined)
      m("site") = ptm.site.get
    m
  }


  /** Create and return a new Passage frame. */
  private def mkPassage (model: PropMap,
                         paperId: String,
                         passage: FriesEntry,
                         passageDoc: Document): PropMap = {
    val f = startFrame(Some("nxml2fries"))
    f("frame-id") = mkPassageId(paperId, passage)
    f("frame-type") = "passage"
    f("index") = passage.chunkId
    f("section-id") = passage.sectionId
    f("section-name") = passage.sectionName
    f("is-title") = passage.isTitle
    assert(passageDoc.text.isDefined, s"passageDoc has no text")
    f("text") = passageDoc.text.get.replaceAll("\\n", " ")
    f
  }

  private def mkPTM (ptm: PTM): PropMap = {
    val m = new PropMap
    m("object-type") = "modification"
    m("type") = ptm.label
    m("negated") = ptm.negated
    if (ptm.site.isDefined) {
      m("site") = ptm.site.get.text
    }
    m
  }

  private def mkRelativePosition (paperId: String,
                                  passageMeta: FriesEntry,
                                  characterOffset: Int): PropMap = {
    val pos = new PropMap
    pos("object-type") = "relative-pos"
    pos("reference") = mkPassageId(paperId, passageMeta)
    pos("offset") = characterOffset
    pos
  }

  private def mkSentence (model: PropMap,
                          paperId: String,
                          passageMeta: FriesEntry,
                          passageDoc: Document,
                          offset: Int): PropMap = {
    val f = startFrame(Some("BioNLPProcessor"))
    f("frame-type") = "sentence"
    f("frame-id") = mkSentenceId(paperId, passageMeta, offset)
    f("passage") = mkPassageId(paperId, passageMeta)
    f("start-pos") = mkRelativePosition(paperId, passageMeta, MentionManager.sentenceStartCharacterOffset(passageDoc, offset))
    f("end-pos") = mkRelativePosition(paperId, passageMeta, MentionManager.sentenceEndCharacterOffset(passageDoc, offset))
    f("text") = passageDoc.sentences(offset).getSentenceText
    f
  }

  private def mkSentences (model: PropMap,
                           paperId: String,
                           passageMeta: FriesEntry,
                           passageDoc: Document): Seq[PropMap] = {
    val sents = new ListBuffer[PropMap]
    for (i <- passageDoc.sentences.indices) {
      sents += mkSentence(model, paperId, passageMeta, passageDoc, i)
    }
    sents.toList
  }


  private def mkContextId (paperId:String, passage:FriesEntry, offset:Int): String =
    s"cntx-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}-$offset-${contextIdCntr.genNextId()}"

  private def mkEntityId (paperId:String, passage:FriesEntry, offset:Int): String =
    s"ment-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}-$offset-${entityIdCntr.genNextId()}"

  private def mkEventId (paperId:String, passage:FriesEntry, offset:Int): String =
    s"evem-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}-$offset-${eventIdCntr.genNextId()}"

  private def mkLinkId (paperId:String, passage:FriesEntry, offset:Int): String =
    s"link-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}-$offset-${linkIdCntr.genNextId()}"

  private def mkPassageId (paperId:String, passage:FriesEntry): String =
    s"pass-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}"

  private def mkSentenceId (paperId:String, passage:FriesEntry, offset:Int): String =
    s"sent-$paperId-$ORGANIZATION-$RUN_ID-${passage.chunkId}-$offset"


  /** Creates a map of all FriesEntries, using chunkId as key */
  private def passagesToMap (paperPassages: Seq[FriesEntry]): Map[String, FriesEntry] = {
    val map = new mutable.HashMap[String, FriesEntry]()
    for (e <- paperPassages) map += e.chunkId -> e
    map.toMap
  }


  /** Returns a data-structure representing all sections and sentences in this paper. */
  private def sentencesToModel (paperId: String,
                                mentions: Seq[Mention],
                                passageMap: Map[String, FriesEntry],
                                startTime: Date,
                                endTime: Date,
                                otherMetaData: Map[String, String]): PropMap = {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime, otherMetaData)

    // keeps track of all documents created for each entry
    val passageDocs = new mutable.HashMap[String, Document]()
    for (m <- mentions)  {
      val chunkId = getChunkId(m)
      if (!passageDocs.contains(chunkId)) {
        passageDocs += chunkId -> m.document
      }
    }

    // store all frames in this output
    val frames = new FrameList
    model("frames") = frames

    // now output all passages as individual frames
    for (chunkId <- passageDocs.keySet) {
      assert(passageMap.contains(chunkId), s"passageMap missing chunkId ${chunkId}")
      frames += mkPassage(model, paperId, passageMap.get(chunkId).get, passageDocs.get(chunkId).get)
      frames ++= mkSentences(model, paperId, passageMap.get(chunkId).get, passageDocs.get(chunkId).get)
    }
    model
  }


  private def startFrame (component: Option[String] = None): PropMap = {
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
  private def writeJsonToFile (model: PropMap, outFile: File) = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
    Serialization.writePretty(model, out)
    out.println()                           // add final newline which serialization omits
    out.flush()
    out.close()
  }

}
