package edu.arizona.sista.reach

import edu.arizona.sista.coref.Coref
import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.LocalGrounder
import edu.arizona.sista.reach.mentions._
import RuleReader.{Rules, readResource}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import scala.collection.immutable.HashSet
import scala.collection.mutable

class ReachSystem(
    rules: Option[Rules] = None,
    proc: Option[BioNLPProcessor] = None
) {

  import ReachSystem._

  val entityRules = if (rules.isEmpty) readResource(RuleReader.entitiesMasterFile) else rules.get.entities
  val modificationRules = if (rules.isEmpty) readResource(RuleReader.modificationsMasterFile) else rules.get.modifications
  val eventRules = if (rules.isEmpty) readResource(RuleReader.eventsMasterFile) else rules.get.events
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
    Seq(entityRules, modificationRules, eventRules).mkString("\n\n")

  def mkDoc(text: String, docId: String, chunkId: String = ""): Document = {
    val doc = processor.annotate(text, keepText = true)
    val id = if (chunkId.isEmpty) docId else s"${docId}_${chunkId}"
    doc.id = Some(id)
    doc
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
    val entities = extractEntitiesFrom(doc)
    val events = extractEventsFrom(doc, entities)
    val completeUnresolved = MentionFilter.keepMostCompleteMentions(events, State(events))
    val resolved = resolve(completeUnresolved)
    // Coref introduced incomplete Mentions that now need to be pruned
    val complete = MentionFilter.keepMostCompleteMentions(resolved, State(resolved))
    resolveDisplay(complete)
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

  def resolve(events: Seq[BioMention]): Seq[CorefMention] = {
    val coref = new Coref()
    coref(events)
  }
}

object ReachSystem {

  // This function should set the right displayMention for each mention.
  // By default the displayMention is set to the main label of the mention,
  // so sometimes it may not require modification
  def resolveDisplay(ms: Seq[CorefMention]): Seq[CorefMention] = {
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

  def resolveDisplayForArguments(em:CorefMention, parents:Set[String]) {
    if(em.labels.contains("Event")) { // recursively traverse the arguments of events
      val newParents = new mutable.HashSet[String]()
      newParents ++= parents
      newParents += em.label
      em.arguments.values.foreach(ms => ms.foreach( m => {
        // resolveDisplayForArguments(m.asInstanceOf[CorefMention].antecedent.getOrElse(m).asInstanceOf[CorefMention], newParents.toSet)
        val crm = m.asInstanceOf[CorefMention]
        resolveDisplayForArguments(crm.antecedentOrElse(crm), newParents.toSet)
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
