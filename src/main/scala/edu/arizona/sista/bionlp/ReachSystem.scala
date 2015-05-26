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
import scala.collection.mutable.ArrayBuffer

class ReachSystem(rules: Option[Rules] = None,
                  proc: Option[BioNLPProcessor] = None) {
  import ReachSystem._

  val entityRules = if (rules.isEmpty) readEntityRules() else rules.get.entities
  val modificationRules = if (rules.isEmpty) readModificationRules() else rules.get.modifications
  val eventRules = if (rules.isEmpty) readEventRules() else rules.get.events
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
  // initialize coref
  val coref = new Coref
  // start event extraction engine
  // This will be our global action for the eventEngine
  val cleanupEvents = DarpaFlow(actions.siteSniffer)andThen
    DarpaFlow(actions.detectNegations) andThen
    DarpaFlow(actions.splitSimpleEvents) andThen
    coref // Should pruneMentions happen in the global action?

  // this engine extracts simple and recursive events and applies coreference
  val eventEngine = ExtractorEngine(eventRules, actions, cleanupEvents.apply)
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
    val finalMentions = extractEventsFrom(doc, entities)
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
    // keep only the most complete events that are worth reporting
    // NOTE: This used to be handled by pruneMentions
    val cleanMentions = keepMostCompleteMentions(mentions, State(mentions))
    // handle multiple Negation modifications
    handleNegations(cleanMentions)
    cleanMentions
  }
}

object ReachSystem {

  // Avoid accidentally pruning SimpleEvents that are participating in Regulations
  def containedInRegulation(m: Mention, state: State): Boolean = m match {
    // if We're not dealing with a SimpleEvent, this check is meaningless
    case irrelevantCheck if !(irrelevantCheck matches "SimpleEvent") => false
    case simple =>
      // Get the arguments (Mentions) of all Regulations
      // that overlap with the current mention
      val regArgs: Seq[Mention] =
        state.mentionsFor(simple.sentence, simple.tokenInterval, "ComplexEvent")
          .flatMap(_.arguments.values.flatten)
      // If no regulations overlap this SimpleEvent, this check is meaningless
      if (regArgs.isEmpty) false
      // check if SimpleEvent exists in the arguments of any overlapping Regulations
      else regArgs.contains(m)
  }

  // this function should remove mentions that were converted
  // into modifications of other mentions
  def pruneMentions(ms: Seq[BioMention]): Seq[BioMention] = {

    // Make sure we don't have any "ModificationTrigger" Mentions
    val validMentions = ms.filterNot(_ matches "ModificationTrigger")

    val (events, nonEvents) = validMentions.partition(_.isInstanceOf[BioEventMention])
    // We need to remove underspecified EventMentions of near-duplicate groupings
    // (ex. same phospho, but one is missing a site)
    val mentionGroupings =
      events.map(_.asInstanceOf[BioEventMention]).groupBy(m => (m.trigger, m.label))

    // remove incomplete mentions
    val completeEventMentions =
      for ((k, ems) <- mentionGroupings) yield {
        val maxSize: Int = ems.map(_.arguments.size).max
        //println(s"\t$maxSize max args for ${k._2}")
        //println(s"${ems.size} before filtering: ")
        //ems foreach display.displayMention
        val filteredEMs = ems.filter(m => m.arguments.size == maxSize)
        //println(s"${filteredEMs.size} after filtering: ")
        //filteredEMs foreach display.displayMention
        //println("#"*30)
        filteredEMs
      }
    nonEvents ++ completeEventMentions.flatten.toSeq
  }

  def keepMostCompleteMentions(ms: Seq[BioMention], state: State): Seq[BioMention] = {
    // First pass at removing incomplete Mentions...should I do this at the end?
    val pruned = pruneMentions(ms)
    // We need to further examine ComplexEvents that are BioRelationMentions
    val (complexRels, other: Seq[BioMention]) =
      pruned.partition(m => m.isInstanceOf[BioRelationMention] && (m matches "ComplexEvent"))
    // We need to keep track of what SimpleEvents to remove from the state
    // whenever one is used to replace a "controlled" arg in a Regulation
    var toRemove = mutable.Set[BioMention]()
    // Check each Regulation that is a RelationMention
    // to see if there are any "more complete" Mentions
    // for the controlled available in the state
    val correctedRels =
      for {reg <- complexRels
           // it should only ever have ONE controlled
           controlled =
           reg.arguments("controlled")
             .head
             // treat it as a BioEventMention to simplify filtering
             .asInstanceOf[BioEventMention]
           // how many args does the controlled Mention have?
           argCount = controlled.arguments.size
      } yield {
        val replacementCandidates: Seq[BioEventMention] =
          state.mentionsFor(reg.sentence, controlled.tokenInterval, controlled.label)
            // If the label is the same, these MUST be BioEventMentions (i.e SimpleEvents)
            .map(_.asInstanceOf[BioEventMention])
            .filter(m => m.arguments.size > argCount && (m.trigger == controlled.trigger))
        // Do we have any "more complete" Mentions to substitute for the controlled?
        replacementCandidates match {
          // Use the current reg, since there aren't any "more complete"
          // candidate Mentions for the controlled
          case Nil => Seq(reg)
          // There are some more complete candidates for the controlled arg...
          case candidates =>
            // For each "more complete" SimpleEvent, create a new Regulation...
              for (r <- candidates) yield {
                val updatedArgs = reg.arguments updated ("controlled", Seq (r) )
                // Keep track of what we need to get rid of...
                toRemove += r
                // Create the "more complete" relationMentions
                val moreCompleteRel =
                  new BioRelationMention(
                    reg.labels,
                    updatedArgs,
                    reg.sentence,
                    reg.document,
                    reg.keep,
                    reg.foundBy)
                // Get the old relation mention's modifications
                moreCompleteRel.modifications = reg.modifications
                moreCompleteRel
              }
        }
      }

    (correctedRels.flatten ++ other)
      .filter(m => !toRemove.contains(m))
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
