package edu.arizona.sista.reach

import edu.arizona.sista.coref.Coref
import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.LocalGrounder
import edu.arizona.sista.reach.context._
import edu.arizona.sista.reach.mentions._
import RuleReader.{Rules, readResource}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.reach.context.rulebased._
import scala.collection.immutable.HashSet
import scala.collection.mutable

class ReachSystem(
    rules: Option[Rules],
    proc: Option[BioNLPProcessor]
) {

  import ReachSystem._

  // overload constructor
  def this() = this(None, None)
  def this(rules: Option[Rules]) = this(rules, None)

  val entityRules = if (rules.isEmpty) readResource(RuleReader.entitiesMasterFile) else rules.get.entities
  val modificationRules = if (rules.isEmpty) readResource(RuleReader.modificationsMasterFile) else rules.get.modifications
  val eventRules = if (rules.isEmpty) readResource(RuleReader.eventsMasterFile) else rules.get.events
  val contextRules = if (rules.isEmpty) readResource(RuleReader.contextRelationsFile) else rules.get.context
  // initialize actions object
  val actions = new DarpaActions
  // initialize grounder
  val grounder = new LocalGrounder
  // start entity extraction engine
  // this engine extracts all physical entities of interest and grounds them
  val entityEngine = ExtractorEngine(entityRules, actions, grounder.apply)
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

  def mkDoc(entry: FriesEntry): Document = mkDoc(entry.text, entry.name, entry.chunkId)

  def mkDoc(text: String, docId: String, chunkId: String = ""): Document = {
    val doc = processor.annotate(text, keepText = true)
    val id = if (chunkId.isEmpty) docId else s"${docId}_${chunkId}"
    doc.id = Some(id)
    doc
  }

  def extractFrom(entries: Seq[FriesEntry]): Seq[BioMention] =
    extractFrom(entries, entries map mkDoc)

  def extractFrom(entries: Seq[FriesEntry], documents: Seq[Document]): Seq[BioMention] = {
    // initialize the context engine
    val contextEngine = new BoundedPaddingContext
    
    val entitiesPerEntry = for (doc <- documents) yield extractEntitiesFrom(doc)
    contextEngine.infer(entries, documents, entitiesPerEntry)
    val entitiesWithContextPerEntry = for (es <- entitiesPerEntry) yield contextEngine.assign(es)
    val eventsPerEntry = for ((doc, es) <- documents zip entitiesWithContextPerEntry) yield extractEventsFrom(doc, es)
    contextEngine.update(eventsPerEntry.flatten)
    val eventsWithContext = contextEngine.assign(eventsPerEntry.flatten)
    val resolved = resolve(eventsWithContext)
    // Coref introduced incomplete Mentions that now need to be pruned
    val complete = MentionFilter.keepMostCompleteMentions(resolved, State(resolved)).map(_.toBioMention)
    resolveDisplay(complete)
  }

  // the extractFrom() methods are the main entry points to the reach system
  def extractFrom(entry: FriesEntry): Seq[BioMention] =
    extractFrom(Seq(entry))

  def extractFrom(text: String, docId: String, chunkId: String): Seq[BioMention] =
    extractFrom(FriesEntry(docId, chunkId, "NoSection", "NoSection", false, text))

  def extractFrom(doc: Document): Seq[BioMention] = {
    require(doc.id.isDefined, "document must have an id")
    require(doc.text.isDefined, "document should keep original text")
    extractFrom(Seq(FriesEntry(doc.id.get, "NoChunk", "NoSection", "NoSection", false, doc.text.get)), Seq(doc))
  }

  def extractEntitiesFrom(doc: Document): Seq[BioMention] = {
    // extract entities and ground them
    val entities = entityEngine.extractByType[BioMention](doc)
    // attach modification features to entities
    val modifiedEntities = modificationEngine.extractByType[BioMention](doc, State(entities))
    modifiedEntities flatMap {
      case m: BioTextBoundMention =>
        // if a mention has many mutations attached to it return a mention for each mutation
        val mutations = m.modifications.filter(_.isInstanceOf[Mutant])
        if (mutations.isEmpty || mutations.size == 1) Seq(m)
        else {
          mutations map { mut =>
            val tbm = new BioTextBoundMention(m.labels, m.tokenInterval, m.sentence, m.document, m.keep, m.foundBy)
            tbm.modifications += mut
            tbm.xref = m.xref // mentions should stay grounded
            tbm
          }
        }

      case m => Seq(m)
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

  def resolve(events: Seq[BioMention]): Seq[BioMention] = {
    val coref = new Coref()
    coref(events)
  }
}

object ReachSystem {

  // This function should set the right displayMention for each mention.
  // By default the displayMention is set to the main label of the mention,
  // so sometimes it may not require modification
  def resolveDisplay(ms: Seq[BioMention]): Seq[BioMention] = {
    // let's do a first attempt, using only grounding info
    // this is useful for entities that do not participate in events
    for(m <- ms) {
      m match {
        case em:TextBoundMention with Display with Grounding =>
          if(m.isGrounded) {
            if(m.xref.get.namespace.contains("interpro"))
              m.displayLabel = "Family"
            else if(m.xref.get.namespace.contains("uniprot"))
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

  def resolveDisplayForArguments(em:BioMention, parents:Set[String]) {
    if(em.labels.contains("Event")) { // recursively traverse the arguments of events
      val newParents = new mutable.HashSet[String]()
      newParents ++= parents
      newParents += em.label
      em.arguments.values.foreach(ms => ms.foreach( m => {
        resolveDisplayForArguments(m.asInstanceOf[BioMention], newParents.toSet)
      }))
    } else if(em.labels.contains("Gene_or_gene_product")) { // we only need to disambiguate these
      if(em.xref.isDefined && em.xref.get.namespace.contains("interpro")) {
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
