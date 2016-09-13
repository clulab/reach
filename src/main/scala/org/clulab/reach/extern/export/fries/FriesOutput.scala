package org.clulab.reach.extern.export.fries

import java.io._
import java.util.Date
import scala.collection.mutable.{HashMap, ListBuffer, Set => MSet}
import org.json4s.native.Serialization
import org.clulab.reach.assembly.export.{CausalPrecedence, Equivalence}
import org.clulab.reach.assembly.{Assembler, RoleWithFeatures}
import org.clulab.reach.assembly.export.AssemblyLink
import org.clulab.reach.assembly.representations.{PTM => AssemblyPTM}
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.reach.FriesEntry
import org.clulab.reach.context._
import org.clulab.reach.darpa.OutputDegrader
import org.clulab.reach.display._
import org.clulab.reach.extern.export._
import org.clulab.reach.extern.export.JsonOutputter._
import org.clulab.reach.extern.export.MentionManager._
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions._
import org.clulab.serialization.json
import org.clulab.serialization.json.JSONSerializer.formats


/**
  * Defines classes and methods used to build and output the FRIES format.
  *   Written by: Mihai Surdeanu and Tom Hicks.
  *   Last Modified: Sort mention maps by sentence order.
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

  // map incoming Reach context type names to FRIES-spec context type names
  val contextNameMap = Map (
    "CellLine" -> "cell-line",
    "CellType" -> "cell-type",
    "TissueType" -> "tissue-type",
    "Cellular_component" -> "location",
    "Species" -> "organism",
    "Organ" -> "organ"
  )

  // track which events have already been output
  val eventsDone: MSet[Mention] = MSet[Mention]()


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


  /** Make and return the Sentence, Entity, Event, and (optionally) Assembly data models. */
  def makeModels (paperId:String,
                  allMentions:Seq[Mention],
                  paperPassages:Seq[FriesEntry],
                  startTime:Date,
                  endTime:Date,
                  assemblyApi: Option[Assembler] = None): (PropMap, PropMap, PropMap, Option[PropMap]) = {
    // reset the set of events already output
    eventsDone.clear()

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
    val entityModel = entitiesToModel(paperId, passageMap, entityMap,
                                      contextIdMap, startTime, endTime, otherMetaData)

    // make Event Model
    val eventModel = eventsToModel(paperId, passageMap, entityMap,
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


  //
  // Private Methods
  //

  /** Add fixed pieces of meta data to the given model, along with additional meta data
      *from the given Map. */
  private def addMetaInfo (model: PropMap,
                           paperId: String,
                           startTime: Date,
                           endTime: Date,
                           otherMetaData: Map[String, String]): Unit = {
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
  private def assemblyToModel (paperId: String,
                               mentions: Seq[Mention],
                               passageMap: Map[String, FriesEntry],
                               entityMap: IDed,
                               eventMap: IDed,
                               startTime: Date,
                               endTime: Date,
                               otherMetaData: Map[String, String],
                               assemblyApi: Assembler): PropMap = {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime, otherMetaData)

    val frames = new FrameList
    model("frames") = frames

    for (mention <- eventMap.keys.toList.sortBy(m => (m.sentence, m.tokenInterval.start))) {
      val fromId = lookupMentionId(mention, entityMap, eventMap)
      if (fromId.isDefined) {
        frames ++= make1toMLinkFrames(paperId, mention, fromId.get,
                                      assemblyApi.getEquivalenceLinks(mention),
                                      "equivalent-to", passageMap, entityMap, eventMap)
        frames ++= make1to1LinkFrames(paperId, mention, fromId.get,
                                      assemblyApi.getCausalPredecessors(mention),
                                      "preceded-by", passageMap, entityMap, eventMap)
      }
    }
    model                              // return the constructed assembly model
  }


  /** Return an Entity Model, representing all entity mentions extracted from this paper. */
  private def entitiesToModel (
    paperId: String,
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

    entityMap.toSeq.sortBy(e => (e._1.sentence)).foreach { entry =>
      val entity = entry._1.toBioMention.asInstanceOf[BioTextBoundMention]
      val entityId = entry._2
      val passage = getPassageForMention(passageMap, entity)
      frames ++= makeEntityMention(paperId, passage, entity, entityId, contextIdMap)
    }

    model
  }


  /** Returns a model object representing all event mentions extracted from this paper. */
  private def eventsToModel (
    paperId: String,
    passageMap: Map[String, FriesEntry],
    entityMap: IDed,
    eventMap: IDed,
    contextIdMap: CtxIDed,
    startTime: Date,
    endTime: Date,
    otherMetaData: Map[String, String],
    assemblyApi: Option[Assembler] = None): PropMap = {

    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime, otherMetaData)

    val frames = new FrameList
    model("frames") = frames

    for {
      entry <- eventMap.toSeq.sortBy(e => (e._1.sentence))
      event = entry._1
      // make sure we haven't seen this event before
      if !eventsDone.contains(event)
    } {
      val passage = getPassageForMention(passageMap, event)
      frames ++= makeEventMention(paperId, passage, event.toBioMention, entityMap,
                                  eventMap, contextIdMap, assemblyApi)
    }

    model
  }


  /** Return the chunk ID for the given mention. */
  private def getChunkId (mention:Mention): String = {
    assert(mention.document.id.isDefined, s"object ${mention} has no document ID")
    // the chunk id is the string following the underscore in the document ids
    val did = mention.document.id.get
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
      *or an existing context ID, or neither if the mention has no context. */
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


  /** Lookup the unique ID, for the given mention, in the given maps, based on the type
      *of the mention. Returns an option for the ID found, or None if no ID found. */
  private def lookupMentionId (mention: Mention,
                               entityMap: IDed,
                               eventMap: IDed): Option[String] = mention match {
    case tbm: TextBoundMention => entityMap.get(tbm)
    case evm: EventMention => eventMap.get(evm)
    case rem: RelationMention => eventMap.get(rem)
    case _ => None
  }


  private def makeArgumentFrame (name: String,
                                 arg: Mention,
                                 argIndex: Int,
                                 parent: Mention,
                                 entityMap: IDed,
                                 eventMap: IDed,
                                 argFeatures:Option[RoleWithFeatures] = None): PropMap = {
    val pm = new PropMap                    // create new argument frame
    pm("object-type") = "argument"
    pm("type") = prettifyLabel(name)
    val argType = mkArgType(arg)
    pm("argument-type") = argType
    pm("index") = argIndex
    pm("text") = arg.text

    argType match {

      case "complex" =>                     // a Protein Complex: print the participants
        if (!isRelationMention(arg))
          throw new RuntimeException(s"Complex argument [${arg.text}] is not an instance of RelationMention")
        val participants = new PropMap
        val complexArgs = arg.asInstanceOf[RelationMention].arguments
        for (key <- complexArgs.keys.toList) {
          val argSeq = complexArgs.get(key).get
          for ((p, i) <- argSeq.zipWithIndex) {
            if (!isTextBoundMention(p))
              throw new RuntimeException(s"Complex participant [${p}] is not an instance of TextBoundMention")
            if (!entityMap.contains(p))
              throw new RuntimeException(s"Complex participant [${p.text} [mods: ${p.toCorefMention.modifications.map(_.toString).mkString(" ")}}]] not in entityMap \nin event [$parent.text] \nin sentence[${p.document.sentences(p.sentence).words.mkString(" ")}]:\n" + json.MentionOps(p).json(pretty = true))
            // participant checks have passed
            participants(s"$key${i + 1}") = entityMap.get(p).get
          }
        }
        pm("args") = participants

      case "entity" =>                      // an Entity: fetch its ID from the entity map
        if (!entityMap.contains(arg)) {
          throw new RuntimeException(s"Entity argument [${arg.text} [mods: ${arg.toCorefMention.modifications.map(_.toString).mkString(" ")}]] not in entityMap \nin event [$parent.text] \nin sentence[${arg.document.sentences(arg.sentence).words.mkString(" ")}]:\n" + json.MentionOps(arg).json(pretty = true))
        }
        pm("arg") = entityMap.get(arg).get

        // output any participant features associated with this entity by assembly:
        if (argFeatures.isDefined) {
          val features = makeParticipantFeatures(argFeatures.get)
          if (features.nonEmpty)
            pm("participant-features") = features
        }

      case "event" =>                       // an Event: fetch its ID from the event map
        if (!eventMap.contains(arg)) {
          throw new RuntimeException(s"Event argument [${arg}] with labels (${arg.labels}) found by ${arg.foundBy} is missing from the eventMap")
        }
        pm("arg") = eventMap.get(arg).get

      // default: should never happen!
      case _ =>
        throw new RuntimeException("ERROR: unknown argument type in makeArgumentFrame: " + arg.labels)
    }

    pm
  }


  /** Create and return a new context frame using the given mention and context info. */
  private def makeContextFrame (paperId: String,
                                passage: FriesEntry,
                                mention: BioMention,
                                contextId: String,
                                context: ContextMap): PropMap = {
    val f = startFrame()
    f("frame-type") = "context"
    f("frame-id") = contextId
    f("scope") = mkSentenceId(paperId, passage, mention.sentence)
    f("facets") = makeContextFacets(context)
    f
  }

  /** Create and return a new map containing the context facets from the given context info. */
  private def makeContextFacets (context: ContextMap): PropMap = {
    val pm = new PropMap
    pm("object-type") = "facet-set"
    context.foreach { case(k, v) =>
      val friesFacetName = contextNameMap.getOrElse(k, k)
      pm(friesFacetName) = v
    }
    pm
  }


  /** Return a tuple of maps mapping entities to unique IDs and events to unique IDs. */
  private def makeEMaps(
    paperId: String,
    mentions: Seq[Mention],
    passageMap: Map[String, FriesEntry]): (IDed, IDed) = {

    val entityMap = new IDed
    val eventMap = new IDed

    makeMapEntries(paperId, mentions, passageMap, entityMap, eventMap)
    (entityMap, eventMap)
  }


  /** Add an entry to the entity and event Maps for the given mention. Then,
    * recursively add the event's arguments to the appropriate maps. */
  private def makeMapEntries (
    paperId: String,
    mentions: Seq[Mention],
    passageMap: Map[String, FriesEntry],
    entityMap: IDed,
    eventMap: IDed,
    // keep track of mentions that have already been processed
    seen: Set[Mention] = Set.empty[Mention]): Unit = for {
      // inspect mention and its arguments
      mention <- mentions
      m <- Seq(mention) ++ mention.arguments.values.flatten.toSeq
      if !seen.contains(m)
  } {
    val passage = getPassageForMention(passageMap, m)
    m match {

      case event if isEventOrRelationMention(event) =>
        // if the key (mention) does not already exist, add mention to the event map
        if (!eventMap.contains(event)) eventMap += event -> mkEventId(paperId, passage, event.sentence)

      // handles sites, entities, and other tbs
      case tb: TextBoundMention =>
        // if the key (mention) does not already exist, add mention to the entity map
        if (!entityMap.contains(tb)) entityMap += tb -> mkEntityId(paperId, passage, tb.sentence)

      case other =>
        throw new RuntimeException(s"Unrecognized mention with label '${other.label}'")
    }

    // recurse on arguments, while avoiding processing same mention again
    makeMapEntries(paperId, Seq(m), passageMap, entityMap, eventMap, seen + m)
  }


  /** Create and return a new entity mention frame for the given entity mention
      *and, possibly, a related context frame. */
  private def makeEntityMention (
    paperId: String,
    passageMeta: FriesEntry,
    mention: BioTextBoundMention,
    entityId: String,
    contextIdMap: CtxIDed): FrameList = {

    val entityList = new FrameList

    val f = startFrame()
    f("frame-type") = "entity-mention"
    f("frame-id") = entityId
    f("sentence") = mkSentenceId(paperId, passageMeta, mention.sentence)
    f("start-pos") = makeRelativePosition(paperId, passageMeta, mention.startOffset)
    f("end-pos") = makeRelativePosition(paperId, passageMeta, mention.endOffset)
    f("text") = mention.text
    f("type") = prettifyLabel(mention.displayLabel)
    val groundings = new FrameList
    mention.grounding.foreach(grnd => groundings += makeGrounding(grnd))
    f("xrefs") = groundings
    if (mention.isModified) {
      val ms = new FrameList
      for (m <- mention.modifications) {
        m match {
          case ptm: PTM => ms += makePTM(ptm)
          case m: Mutant => ms += makeMutant(m)
          case _ =>                         // we do not export anything else
        }
      }
      if (ms.nonEmpty)
        f("modifications") = ms
    }

    // OPTIONAL TODO: add "index": the sentence-local number for this mention from this component.

    // handle context features:
    val (contextId, contextFrame) = lookupContext(paperId, passageMeta, mention, contextIdMap)

    // the context for a frame is a list of context frame refs (currently a singleton list)
    if (contextId.isDefined)
      f("context") = List(contextId.get)

    // return 1 or more frames: both context and event frames or just an entity frame:
    if (contextFrame.isDefined)
      entityList += contextFrame.get

    entityList += f
  }


  /** Create and return a new event mention frame for the given event mention
      *and, possibly, a related context frame. */
  private def makeEventMention (
    paperId: String,
    passageMeta: FriesEntry,
    mention: BioMention,
    entityMap: IDed,
    eventMap: IDed,
    contextIdMap: CtxIDed,
    assemblyApi: Option[Assembler] = None): FrameList = {

    val eventList = new FrameList

    val f = startFrame()
    f("frame-type") = "event-mention"
    f("frame-id") = getUniqueId(eventMap, mention)
    f("sentence") = mkSentenceId(paperId, passageMeta, mention.sentence)
    f("start-pos") = makeRelativePosition(paperId, passageMeta, mention.startOffset)
    f("end-pos") = makeRelativePosition(paperId, passageMeta, mention.endOffset)
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

    // if using the Assembly subsystem, get all input participant features for this event
    val inputFeatures = assemblyApi.map(_.getInputFeaturesByParticipants(mention))

    // add the argument frames for (already processed) arguments
    mention match {
      case em if isEventOrRelationMention(em) =>
        val arguments = em.arguments        // get the mention arguments Map
        val argList = new FrameList         // start new argument frame list

        for {
          role <- arguments.keys.toList
          (ithArg: Mention, i: Int) <- arguments(role).zipWithIndex
        } {
          val argFeatures = inputFeatures match {
            case Some(features) => Some(features(ithArg))
            case None => None
          }
          argList += makeArgumentFrame(role, ithArg, i, mention, entityMap, eventMap, argFeatures)
        }
        f("arguments") = argList

      case _ => ()
    }

    // event modifications
    if (isNegated(mention))
      f("is-negated") = true
    if (isHypothesized(mention))
      f("is-hypothesis") = true

    // OPTIONAL TODO: add "index": the sentence-local number for this mention from this component.

    // handle context features:
    val (contextId, contextFrame) = lookupContext(paperId, passageMeta, mention, contextIdMap)

    // the context for a frame is a list of context frame refs (currently a singleton list)
    if (contextId.isDefined)
      f("context") = List(contextId.get)

    // return 1 or more frames: context and/or event frame(s)
    if (contextFrame.isDefined)
      eventList += contextFrame.get

    eventsDone += mention                   // remember this mention has been output
    eventList += f                          // add new frame to returned list
  }


  /** Create and return a single grounding map. */
  private def makeGrounding (grounding:KBResolution): PropMap = {
    val pm = new PropMap
    pm("object-type") = "db-reference"
    pm("namespace") = grounding.namespace
    pm("id") = grounding.id
    pm
  }

  /** Create and return a single assembly link argument map. */
  private def makeLinkArgumentFrame (arg: String,
                                     argType: String,
                                     index: Option[Integer] = None): PropMap = {
    val pm = new PropMap
    pm("object-type") = "argument"
    pm("type") = argType
    pm("arg") = arg
    if (index.isDefined)
      pm("index") = index.get
    pm
  }

  /** Create and return a new binary Link frame. */
  private def makeLinkFrame (linkId: String,
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
  private def make1to1LinkFrames [L <: AssemblyLink] (
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
            val args = List(makeLinkArgumentFrame(fromId, "after"),
                            makeLinkArgumentFrame(m2Id.get, "before"))
            frames += makeLinkFrame(linkId, frameType, foundBy, args)
          }
        case unknown =>
          System.err.println(s"Cannot create 1-to-1 links for type '${unknown.getClass}'")
      }
    }
    frames.toList
  }

  /** Return a singleton list of link frame made from the given mention and set of assembly links. */
  private def make1toMLinkFrames [L <: AssemblyLink] (
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
          args += makeLinkArgumentFrame(fromId, "from") // add FROM as argument zero
          args ++= links.zipWithIndex.flatMap { case(link:Equivalence, ndx:Int) =>
            lookupMentionId(link.m2, entityMap, eventMap).map(m2Id =>
              makeLinkArgumentFrame(m2Id, s"to${ndx}"))
          }
          if (args.size > 1)                  // must have at least one TO argument
            frames += makeLinkFrame(linkId, frameType, foundBy, args.toList)

        case unknown =>
          System.err.println(s"Cannot create 1-to-M links for type '${unknown.getClass}'")
      }
    }
    frames.toList
  }


  /** Create and return a single mutant modification map. */
  private def makeMutant (mutant: Mutant): PropMap = {
    val pm = new PropMap
    pm("object-type") = "modification"
    pm("type") = "mutation"
    pm("evidence") = mutant.evidence.text
    pm
  }


  /** Return a list of participant features for the given role/argument features object. */
  private def makeParticipantFeatures (rwfs: RoleWithFeatures): FrameList = {
    (new FrameList) ++= rwfs.features.map{ f:AssemblyPTM => makeParticipantFeature(f) }
  }

  /** Return a participant feature map for the given modification object. */
  private def makeParticipantFeature (ptm: AssemblyPTM): PropMap = {
    val pm = new PropMap
    pm("object-type") = "feature"
    pm("type") = ptm.label
    pm("negated") = ptm.negated
    if (ptm.site.isDefined)
      pm("site") = ptm.site.get
    pm
  }


  /** Create and return a new passage frame. */
  private def makePassage (model: PropMap,
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

  /** Create and return a single PTM modification map. */
  private def makePTM (ptm: PTM): PropMap = {
    val pm = new PropMap
    pm("object-type") = "modification"
    pm("type") = ptm.label
    pm("negated") = ptm.negated
    if (ptm.site.isDefined) {
      pm("site") = ptm.site.get.text
    }
    pm
  }

  /** Create and return a single relative position map. */
  private def makeRelativePosition (paperId: String,
                                    passageMeta: FriesEntry,
                                    characterOffset: Int): PropMap = {
    val pm = new PropMap
    pm("object-type") = "relative-pos"
    pm("reference") = mkPassageId(paperId, passageMeta)
    pm("offset") = characterOffset
    pm
  }

  /** Create and return a sentence frame. */
  private def makeSentence (model: PropMap,
                            paperId: String,
                            passageMeta: FriesEntry,
                            passageDoc: Document,
                            offset: Int): PropMap = {
    val f = startFrame(Some("BioNLPProcessor"))
    f("frame-type") = "sentence"
    f("frame-id") = mkSentenceId(paperId, passageMeta, offset)
    f("passage") = mkPassageId(paperId, passageMeta)
    f("start-pos") = makeRelativePosition(paperId, passageMeta, sentenceStartCharacterOffset(passageDoc, offset))
    f("end-pos") = makeRelativePosition(paperId, passageMeta, sentenceEndCharacterOffset(passageDoc, offset))
    f("text") = passageDoc.sentences(offset).getSentenceText
    f
  }

  /** Create and return a sequence of sentence frames. */
  private def makeSentences (model: PropMap,
                             paperId: String,
                             passageMeta: FriesEntry,
                             passageDoc: Document): Seq[PropMap] = {
    val sents = new ListBuffer[PropMap]
    for (i <- passageDoc.sentences.indices) {
      sents += makeSentence(model, paperId, passageMeta, passageDoc, i)
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
    val map = new HashMap[String, FriesEntry]()
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
    val passageDocs = new HashMap[String, Document]()
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
      frames += makePassage(model, paperId, passageMap.get(chunkId).get, passageDocs.get(chunkId).get)
      frames ++= makeSentences(model, paperId, passageMap.get(chunkId).get, passageDocs.get(chunkId).get)
    }
    model
  }


  /** Make and return a new Frame optionally initialized with component type. */
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
