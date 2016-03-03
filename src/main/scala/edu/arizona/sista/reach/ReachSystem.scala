package edu.arizona.sista.reach

import edu.arizona.sista.coref.Coref
import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.mentions._
import RuleReader.{Rules, readResource}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import scala.collection.immutable.HashSet
import scala.collection.mutable
import edu.arizona.sista.reach.context._
import edu.arizona.sista.reach.context.ContextEngineFactory.Engine._

class ReachSystem(
    rules: Option[Rules] = None,
    proc: Option[BioNLPProcessor] = None,
    contextEngineType: Engine = Dummy,
    contextParams: Map[String, String] = Map()
) {

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
  // this engine extracts all physical entities of interest and grounds them
  val entityEngine = ExtractorEngine(entityRules, actions)
  // start modification engine
  // this engine extracts modification features and attaches them to the corresponding entity
  val modificationEngine = ExtractorEngine(modificationRules, actions)
  // start event extraction engine
  // this engine extracts simple and recursive events and applies coreference
  val eventEngine = ExtractorEngine(eventRules, actions, actions.cleanupEvents)
  // initialize processor
  val processor = if (proc.isEmpty) new BioNLPProcessor else proc.get
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

  def extractFrom(entries: Seq[FriesEntry]): Seq[BioMention] =
    extractFrom(entries, entries.map{
        e => mkDoc(e.text, e.name, e.chunkId)
    })

  def extractFrom(entries: Seq[FriesEntry], documents: Seq[Document]): Seq[BioMention] = {
    // initialize the context engine
    val contextEngine = ContextEngineFactory.buildEngine(contextEngineType, contextParams)

    val entitiesPerEntry = for (doc <- documents) yield extractEntitiesFrom(doc)
    contextEngine.infer(entries, documents, entitiesPerEntry)
    val entitiesWithContextPerEntry = for (es <- entitiesPerEntry) yield contextEngine.assign(es)
    val eventsPerEntry = for ((doc, es) <- documents zip entitiesWithContextPerEntry) yield {
        val events = extractEventsFrom(doc, es)
        MentionFilter.keepMostCompleteMentions(events, State(events))
    }
    contextEngine.update(eventsPerEntry.flatten)
    val eventsWithContext = contextEngine.assign(eventsPerEntry.flatten)
    System.err.println("******* BEFORE GROUNDING **********") // REMOVE LATER
    eventsWithContext.foreach { grounder.printMention }       // REMOVE LATER
    val grounded = grounder(eventsWithContext)
    System.err.println("******* AFTER GROUNDING **********") // REMOVE LATER
    grounded.foreach { grounder.printMention }               // REMOVE LATER
    // Coref expects to get all mentions grouped by document
    val resolved = resolveCoref(groupMentionsByDocument(grounded, documents))
    // Coref introduced incomplete Mentions that now need to be pruned
    val complete = MentionFilter.keepMostCompleteMentions(resolved, State(resolved)).map(_.toCorefMention)
    // val complete = MentionFilter.keepMostCompleteMentions(eventsWithContext, State(eventsWithContext)).map(_.toBioMention)

    resolveDisplay(complete)
  }

  // this method groups the mentions by document
  // the sequence of documents should be sorted in order of appearance in the paper
  def groupMentionsByDocument(mentions: Seq[BioMention], documents: Seq[Document]): Seq[Seq[BioMention]] = {
    for (doc <- documents) yield mentions.filter(_.document == doc)
  }

  // the extractFrom() methods are the main entry points to the reach system
  def extractFrom(entry: FriesEntry): Seq[BioMention] =
    extractFrom(entry.text, entry.name, entry.chunkId)

  def extractFrom(text: String, docId: String, chunkId: String): Seq[BioMention] = {
    extractFrom(mkDoc(text, docId, chunkId))
  }

  def extractFrom(doc: Document): Seq[BioMention] = {
    require(doc.id.isDefined, "document must have an id")
    require(doc.text.isDefined, "document should keep original text")
    extractFrom(Seq(FriesEntry(doc.id.get, "NoChunk", "NoSection", "NoSection", false, doc.text.get)), Seq(doc))
  }

  def extractEntitiesFrom(doc: Document): Seq[BioMention] = {
    // extract entities
    val entities = entityEngine.extractByType[BioMention](doc)
    // attach modification features to entities
    val modifiedEntities = modificationEngine.extractByType[BioMention](doc, State(entities))
    val mutationAddedEntities = modifiedEntities flatMap {
      case m: BioTextBoundMention => mutationsToMentions(m)
      case m => Seq(m)
    }
    // add grounding candidates to entities
    entityLookup(mutationAddedEntities)
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

object ReachSystem {

  // This function should set the right displayMention for each mention.
  // By default the displayMention is set to the main label of the mention,
  // so sometimes it may not require modification
  def resolveDisplay(ms: Seq[CorefMention]): Seq[CorefMention] = {
    // let's do a first attempt, using only preliminary grounding info
    // this is useful for entities that do not participate in events
    for(m <- ms) {
      m match {
        case em:TextBoundMention with Display with Grounding =>
          if (m.isGrounded) {
            if (ReachKBUtils.isFamilyGrounded(m))
              m.displayLabel = "Family"
            else if (ReachKBUtils.isProteinGrounded(m))
              m.displayLabel = "Protein"
          }
        case _ => // nothing to do
      }
    }

    // now let's try to disambiguate Gene_or_gene_product that participate in events
    for(m <- ms) {
      if(m.labels.contains("Event")) {
        val parents = new HashSet[String]
        resolveDisplayForArguments(m, parents)
      }
    }

    // last resort: displayLabel is set to the default value
    ms.foreach(m => if(m.displayLabel == null) m.displayLabel = m.label)
    ms
  }

  def resolveDisplayForArguments(em:CorefMention, parents:Set[String]) {
    if(em.labels.contains("Event")) { // recursively traverse the arguments of events
      val newParents = new mutable.HashSet[String]()
      newParents ++= parents
      newParents += em.label
      em.arguments.values.foreach(ms => ms.foreach( m => {
        val crm = m.asInstanceOf[CorefMention]
        resolveDisplayForArguments(crm.antecedentOrElse(crm), newParents.toSet)
      }))
    } else if(em.labels.contains("Gene_or_gene_product")) { // we only need to disambiguate these
      if (ReachKBUtils.isFamilyGrounded(em)) {
        // found a Family incorrectly labeled as protein
        em.displayLabel = "Family"
      } else if(parents.contains("Transcription")) {
        // found a Gene
        em.displayLabel = "Gene"
      } else {
        em.displayLabel = "Protein"
      }
    }
  }

}
