package org.clulab.reach

import scala.collection.immutable.HashSet
import scala.collection.mutable

import ai.lum.nxmlreader.NxmlDocument
import com.typesafe.scalalogging.LazyLogging

import org.clulab.coref.Alias
import org.clulab.coref.Coref
import org.clulab.odin._
import org.clulab.processors.{Document, Processor}
import org.clulab.reach.context._
import org.clulab.reach.context.ContextEngineFactory.Engine._
import org.clulab.reach.coserver.ProcessorCoreClient
import org.clulab.reach.darpa.{DarpaActions, MentionFilter, NegationHandler}
import org.clulab.reach.grounding._
import org.clulab.reach.mentions._
import RuleReader.{Rules, readResource}


class ReachSystem(
  rules: Option[Rules] = None,
  pcc: Option[ProcessorCoreClient] = None,
  contextEngineType: Engine = Dummy,
  contextParams: Map[String, String] = Map()
) extends LazyLogging {

  import ReachSystem._

  val entityRules = if (rules.isEmpty) readResource(RuleReader.entitiesMasterFile) else rules.get.entities
  val modificationRules = if (rules.isEmpty) readResource(RuleReader.modificationsMasterFile) else rules.get.modifications
  val eventRules = if (rules.isEmpty) readResource(RuleReader.eventsMasterFile) else rules.get.events
  val contextRules = if (rules.isEmpty) readResource(RuleReader.contextRelationsFile) else rules.get.context
  // initialize actions object
  val actions = new DarpaActions
  val entityLookup = new ReachEntityLookup // initialize entity lookup (find grounding candidates)
  val grounder = new ReachGrounder
  // start entity extraction engine
  // this engine extracts all physical entities of interest
  val entityEngine = ExtractorEngine(entityRules, actions)
  // start modification engine
  // this engine extracts modification features and attaches them to the corresponding entity
  val modificationEngine = ExtractorEngine(modificationRules, actions)
  // start event extraction engine
  // this engine extracts simple and recursive events and applies coreference
  val eventEngine = ExtractorEngine(eventRules, actions, actions.cleanupEvents)
  // initialize processor
  val processor = if (pcc.nonEmpty) pcc.get else new ProcessorCoreClient
  processor.annotate("something")

  /** returns string with all rules used by the system */
  def allRules: String =
    Seq(entityRules, modificationRules, eventRules, contextRules).mkString("\n\n")

  def mkDoc(text: String, docId: String, chunkId: String = ""): Document = {
    val doc = processor.annotate(text, keepText = true)
    val id = if (chunkId.isEmpty) docId else s"${docId}_${chunkId}"
    doc.id = Some(id)
    doc
  }

  def mkDoc(nxml: NxmlDocument): Document = {
    // we are using the PMC as the chunk-id because we now read
    // the whole paper in a single chunk
    mkDoc(nxml.text, nxml.pmc, nxml.pmc)
  }

  def extractFrom(entry: FriesEntry): Seq[BioMention] =
    extractFrom(entry.text, entry.name, entry.chunkId)

  def extractFrom(nxml: NxmlDocument): Seq[BioMention] = {
    // use standoff hashcode as the chunkId
    extractFrom(mkDoc(nxml), Some(nxml))
  }

  def extractFrom(doc: Document, nxmlDoc: Option[NxmlDocument]): Seq[BioMention] = {
    // initialize the context engine
    val contextEngine = ContextEngineFactory.buildEngine(contextEngineType, contextParams)

    val entities = extractEntitiesFrom(doc)
    contextEngine.infer(entities)
    val entitiesWithContext = contextEngine.assign(entities)
    val unfilteredEvents = extractEventsFrom(doc, entitiesWithContext)
    logger.debug(s"${unfilteredEvents.size} unfilteredEvents: ${display.summarizeMentions(unfilteredEvents,doc)}")
    val events = MentionFilter.keepMostCompleteMentions(unfilteredEvents, State(unfilteredEvents))
    logger.debug(s"${events.size} events after MentionFilter.keepMostCompleteMentions: ${display.summarizeMentions(events, doc)}")
    contextEngine.update(events)
    val eventsWithContext = contextEngine.assign(events)
    logger.debug(s"${eventsWithContext.size} events after contextEngine.assign: ${display.summarizeMentions(eventsWithContext, doc)}")
    val grounded = grounder(eventsWithContext)
    logger.debug(s"${grounded.size} events after grounder: ${display.summarizeMentions(grounded, doc)}")
    // Coref expects to get all mentions grouped
    // we group according to the standoff, if there is one
    // else we just make one group with all the mentions
    val groundedAndGrouped = nxmlDoc match {
      case Some(nxml) => groupMentionsByStandoff(grounded, nxml)
      case None => Seq(grounded)
    }
    logger.debug(s"${groundedAndGrouped.flatten.size} events after groundedAndGrouped: ${display.summarizeMentions(groundedAndGrouped.flatten, doc)}")
    val resolved = resolveCoref(groundedAndGrouped)
    logger.debug(s"${resolved.size} events after coref: ${display.summarizeMentions(resolved, doc)}")
    // Coref introduced incomplete Mentions that now need to be pruned
    val complete = MentionFilter.keepMostCompleteMentions(resolved, State(resolved)).map(_.toCorefMention)
    logger.debug(s"${complete.size} events after coref + 2nd MentionFilter.keepMostCompleteMentions: ${display.summarizeMentions(complete, doc)}")
    logger.debug(s"Resolving display...")
    resolveDisplay(complete)
  }

  def extractFrom(entries: Seq[FriesEntry]): Seq[BioMention] =
    extractFrom(entries, entries.map{
        e => mkDoc(e.text, e.name, e.chunkId)
    })

  def extractFrom(entries: Seq[FriesEntry], documents: Seq[Document]): Seq[BioMention] = {
    // initialize the context engine
    val contextEngine = ContextEngineFactory.buildEngine(contextEngineType, contextParams)

    val entitiesPerEntry = extractEntitiesFrom(documents)
    contextEngine.infer(entitiesPerEntry.flatten)
    val entitiesWithContextPerEntry = for (es <- entitiesPerEntry) yield contextEngine.assign(es)
    // get events
    val eventsPerEntry = for ((doc, es) <- documents zip entitiesWithContextPerEntry) yield {
        val events = extractEventsFrom(doc, es)
        MentionFilter.keepMostCompleteMentions(events, State(events))
    }
    contextEngine.update(eventsPerEntry.flatten)
    val eventsWithContext = contextEngine.assign(eventsPerEntry.flatten)
    val grounded = grounder(eventsWithContext)
    // Coref expects to get all mentions grouped by document
    val resolved = resolveCoref(groupMentionsByDocument(grounded, documents))
    // Coref introduced incomplete Mentions that now need to be pruned
    val complete = MentionFilter.keepMostCompleteMentions(resolved, State(resolved)).map(_.toCorefMention)

    resolveDisplay(complete)
  }

  // this method groups the mentions by document
  // the sequence of documents should be sorted in order of appearance in the paper
  def groupMentionsByDocument(mentions: Seq[BioMention], documents: Seq[Document]): Seq[Seq[BioMention]] = {
    for (doc <- documents) yield mentions.filter(_.document == doc)
  }

  /** group mentions according to their position in the nxml standoff */
  def groupMentionsByStandoff(mentions: Seq[BioMention], nxml: NxmlDocument): Seq[Seq[BioMention]] = {
    mentions.groupBy(m => nxml.standoff.getTerminals(m.startOffset, m.endOffset)).values.toVector
  }

  def extractFrom(text: String, docId: String, chunkId: String): Seq[BioMention] = {
    extractFrom(mkDoc(text, docId, chunkId))
  }

  def extractFrom(doc: Document): Seq[BioMention] = {
    require(doc.id.isDefined, "document must have an id")
    require(doc.text.isDefined, "document should keep original text")
    extractFrom(doc, None) // no nxml
  }

  def extractEntitiesFrom(doc: Document): Seq[BioMention] = {
    // extract entities
    val entities = entityEngine.extractByType[BioMention](doc)
    // attach mutations to entities
    // this step must precede alias search to prevent alias overmatching
    val mutationAddedEntities = entities flatMap {
      case m: BioTextBoundMention => mutationsToMentions(m)
      case m => Seq(m)
    }
    // use aliases to find more entities
    // TODO: attach mutations to these entities as well
    val entitiesWithAliases = Alias.canonizeAliases(mutationAddedEntities, doc)
    // attach modification features to entities
    val modifiedEntities = modificationEngine.extractByType[BioMention](doc, State(entitiesWithAliases))
    // add grounding candidates to entities
    entityLookup(modifiedEntities)
  }

  def extractEntitiesFrom(docs: Seq[Document]): Seq[Seq[BioMention]] = {
    // extract entities
    val entities = for (doc <- docs) yield entityEngine.extractByType[BioMention](doc)
    // attach mutations to entities
    // this step must precede alias search to prevent alias overmatching
    val mutationAddedEntities = for (entitiesInDoc <- entities) yield {
      entitiesInDoc flatMap {
        case m: BioTextBoundMention => mutationsToMentions(m)
        case m => Seq(m)
      }
    }
    // use aliases to find more entities
    // TODO: attach mutations to these entities as well
    val entitiesWithAliases = Alias.canonizeAliases(mutationAddedEntities, docs)

    for {
      i <- docs.indices
      doc = docs(i)
      docEntities = entitiesWithAliases(i)
    } yield {
      // attach modification features to entities
      val modifiedEntities = modificationEngine.extractByType[BioMention](doc, State(docEntities))
      // add grounding candidates to entities
      entityLookup(modifiedEntities)
    }
  }

  /** If the given mention has many mutations attached to it, return a mention for each mutation. */
  def mutationsToMentions(mention: BioTextBoundMention): Seq[BioMention] = {
    val mutations = mention.modifications.filter(_.isInstanceOf[Mutant])
    if (mutations.isEmpty || mutations.size == 1)
      Seq(mention)
    else {
      mutations.map { mut =>
        val tbm = new BioTextBoundMention(mention.labels, mention.tokenInterval,
                                          mention.sentence, mention.document,
                                          mention.keep, mention.foundBy)
        // copy all attachments
        BioMention.copyAttachments(mention, tbm)
        // remove all mutations
        tbm.modifications = tbm.modifications diff mutations
        // add desired mutation only
        tbm.modifications += mut
        tbm
      }.toSeq
    }
  }

  def extractEventsFrom(doc: Document, entities: Seq[BioMention]): Seq[BioMention] = {
    val mentions = eventEngine.extractByType[BioMention](doc, State(entities))
    // clean modified entities
    // remove ModificationTriggers
    // Make sure we don't have any "ModificationTrigger" Mentions
    val validMentions = mentions.filterNot(_ matches "ModificationTrigger")
    // handle multiple Negation modifications
    NegationHandler.handleNegations(validMentions)
    validMentions
  }

  // this method gets sequence composed of sequences of mentions, one per doc.
  // each doc corresponds to a chunk of the paper, and it expects them to be in order of appearance
  def resolveCoref(eventsPerDocument: Seq[Seq[BioMention]]): Seq[CorefMention] = {
    val coref = new Coref()
    coref(eventsPerDocument).flatten
  }

}

object ReachSystem extends LazyLogging {

  /** This function should set the right displayMention for each mention.
    * NB: By default the displayMention is set to the main label of the mention,
    *     so, after extraction, it should never be null.
    */
  def resolveDisplay (ms: Seq[CorefMention]): Seq[CorefMention] = {
    for (m <- ms) {
      m match {
        case em:TextBoundMention with Display with Grounding =>
          resolveDisplayForEntity(m)
        case rm:RelationMention with Display with Grounding =>
          resolveDisplayForArguments(m, new HashSet[String])
        case vm:EventMention with Display with Grounding =>
          resolveDisplayForArguments(m, new HashSet[String])
        case _ =>                           // nothing to do
      }
    }
    ms
  }

  /** Recursively traverse the arguments of events and handle GPP entities. */
  def resolveDisplayForArguments (em: CorefMention, parents: Set[String]) {
    if (em.labels.contains("Event")) {      // recursively traverse the arguments of events
      val newParents = new mutable.HashSet[String]()
      newParents ++= parents
      newParents += em.label
      em.arguments.values.foreach(ms => ms.foreach( m => {
        val crm = m.asInstanceOf[CorefMention]
        resolveDisplayForArguments(crm.antecedentOrElse(crm), newParents.toSet)
      }))
    }
    else if (em.labels.contains("Gene_or_gene_product")) { // we only need to disambiguate these
      resolveDisplayForEntity(em, Some(parents))
    }
  }

  /** Set the displayLabel for a single mention, using optional parent label set information. */
  def resolveDisplayForEntity (em: CorefMention, parents: Option[Set[String]] = None) {
    if (em.labels.contains("Gene_or_gene_product")) {
      if (em.isGrounded && ReachKBUtils.isFamilyGrounded(em)) {
        em.displayLabel = "Family"
      }
      else if (parents.exists(_.contains("Transcription"))) {
        em.displayLabel = "Gene"
      } else {
        em.displayLabel = "Protein"
      }
    }
  }

  // def chooseProcessor(procType:String):Processor = {
  //   val proc = procType.toLowerCase match {
  //     case "fastbionlp" =>
  //       logger.info("Choosing FastBio processor")
  //       new FastBioNLPProcessor(withChunks = false)
  //     case _ =>
  //       logger.info("Choosing Bio processor")
  //       new BioNLPProcessor(withChunks = false)
  //   }
  //   proc
  // }

}
