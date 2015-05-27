package edu.arizona.sista.bionlp

import edu.arizona.sista.struct.{Interval, DirectedGraph}
import edu.arizona.sista.odin._
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.odin.domains.bigmechanism.summer2015.{DarpaFlow, LocalGrounder, Coref}
import RuleReader._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

import scala.collection.immutable.HashSet
import scala.collection.mutable

class ReachSystem(rules: Option[Rules] = None,
                  proc: Option[BioNLPProcessor] = None) {
  import ReachSystem._

  val entityRules = if (rules.isEmpty) readEntityRules() else rules.get.entities
  val modificationRules = if (rules.isEmpty) readModificationRules() else rules.get.modifications
  val eventRules = if (rules.isEmpty) readEventRules() else rules.get.events
  val corefRules = if (rules.isEmpty) readCorefRules() else rules.get.coref
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
  // This will be our global action for the eventEngine
  val cleanupEvents = DarpaFlow(actions.siteSniffer) andThen DarpaFlow(actions.detectNegations) andThen DarpaFlow(actions.detectHypotheses) andThen DarpaFlow(actions.splitSimpleEvents)
  // this engine extracts simple and recursive events and applies coreference
  val eventEngine = ExtractorEngine(eventRules, actions, cleanupEvents.apply)
  // this engine extracts generic mentions that can be anaphora like "it" and tries to resolve them
  val corefEngine = ExtractorEngine(corefRules, actions, cleanupEvents.apply)
  // initialize processor
  val processor = if (proc.isEmpty) new BioNLPProcessor else proc.get
  processor.annotate("something")

  def mkDoc(text: String, docId: String, chunkId: String = ""): Document = {
    val doc = processor.annotate(text, keepText = true)
    val id = if (chunkId.isEmpty) docId else s"${docId}_${chunkId}"
    doc.id = Some(id)
    doc
  }

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
    val unresolved = extractResolvedFrom(doc, events)
    // initialize coref
    val coref = new Coref
    val finalMentions = coref(unresolved,State(unresolved))

    resolveDisplay(finalMentions)
  }

  def extractEntitiesFrom(doc: Document): Seq[BioMention] = {
    // extract entities and ground them
    val entities = entityEngine.extractByType[BioMention](doc)
    // attach modification features to entities
    val modifiedEntities = modificationEngine.extractByType[BioMention](doc, State(entities))
    modifiedEntities
  }

  def extractEventsFrom(doc: Document, ms: Seq[BioMention]): Seq[BioMention] = {
    val mentions = eventEngine.extractByType[BioMention](doc, State(ms))
    // clean modified entities
    // for example, remove sites that are part of a modification feature
    val cleanMentions = pruneMentions(mentions)
    // handle multiple Negation modifications
    handleNegations(cleanMentions)
    cleanMentions
  }

  //
  def extractResolvedFrom(doc: Document, ms: Seq[BioMention]): Seq[BioMention] = {
    val mentions = corefEngine.extractByType[BioMention](doc, State(ms))
    val cleanMentions = pruneMentions(mentions)
    cleanMentions
  }
}

object ReachSystem {

  // this function should remove mentions that were converted
  // into modifications of other mentions
  def pruneMentions(ms: Seq[BioMention]): Seq[BioMention] = {
    // Make sure we don't have any "ModificationTrigger" Mentions
    val validMentions = ms.filterNot(_ matches "ModificationTrigger")

      val (events, nonEvents) = validMentions.partition(_.isInstanceOf[BioEventMention])
      // We need to remove underspecified EventMentions of near-duplicate groupings
      // (ex. same phospho, but one is misssing a site)
      val mentionGroupings =
        events.map(_.asInstanceOf[BioEventMention]).groupBy(m => (m.trigger, m.label))

    // remove incomplete mentions
    val completeEventMentions =
      for ((k, ems) <- mentionGroupings) yield {
        val maxSize:Int = ems.map(_.arguments.size).max
        //println(s"\t$maxSize max args for ${k._2}")
        //println(s"${ems.size} before filtering: ")
        //ems foreach display.displayMention
        val filteredEMs = ems.filter(_.arguments.size == maxSize)
        //println(s"${filteredEMs.size} after filtering: ")
        //filteredEMs foreach display.displayMention
        //println("#"*30)
        filteredEMs
      }
    nonEvents ++ completeEventMentions.flatten.toSeq
  }

  // Alter Negation modifications in-place
  def handleNegations(ms: Seq[BioMention]): Unit = {
    ms foreach { m =>
      val (negMods, other) = m.modifications.partition(_.isInstanceOf[Negation])
      val negationModifications = negMods.map(_.asInstanceOf[Negation])

      // count the negations
      negationModifications match {
        // 0 or 1 Neg modifications means no change...
        case noChange if noChange.size <= 1 => ()
        // if we have an even number of Negations, remove them all
        case pos if pos.size % 2 == 0 =>
          m.modifications = other
        // if we have an odd number, report only the first...
        case neg if neg.size % 2 != 0 =>
          val singleNeg =
            negationModifications
              .toSeq
              .sortBy(_.evidence.tokenInterval)
              .head
          m.modifications = other + singleNeg
      }
    }
  }

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
