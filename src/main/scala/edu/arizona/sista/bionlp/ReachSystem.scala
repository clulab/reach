package edu.arizona.sista.bionlp

import edu.arizona.sista.odin._
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.odin.domains.bigmechanism.summer2015.{ LocalGrounder, Coref }
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

import scala.collection.immutable.HashSet
import scala.collection.mutable

class ReachSystem {
  import ReachSystem._

  // initialize actions object
  val actions = new DarpaActions
  // initialize grounder
  val grounder = new LocalGrounder
  // start entity extraction engine
  // this engine extracts all physical entities of interest and grounds them
  val entityEngine = ExtractorEngine(readEntityRules(), actions, grounder.apply)
  // start modification engine
  // this engine extracts modification features and attaches them to the corresponding entity
  val modificationEngine = ExtractorEngine(readModificationRules(), actions)
  // initialize coref
  val coref = new Coref
  // start event extraction engine
  // this engine extracts simple and recursive events and applies coreference
  val eventEngine = ExtractorEngine(readEventRules(), actions, coref.apply)
  // initialize processor
  val processor = new BioNLPProcessor
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
    // clean modified entities
    // for example, remove sites that are part of a modification feature
    filterModifiedEntities(modifiedEntities)
  }

  def extractEventsFrom(doc: Document, ms: Seq[BioMention]): Seq[BioMention] =
    eventEngine.extractByType[BioMention](doc, State(ms))

}

object ReachSystem {
  val resourcesDir = "/edu/arizona/sista/odin/domains/bigmechanism/summer2015/biogrammar"
  val entitiesDir = s"$resourcesDir/entities"
  val modificationsDir = s"$resourcesDir/modifications"
  val eventsDir = s"$resourcesDir/events"

  def readRules(): String =
    readEntityRules() + "\n\n" + readModificationRules() + "\n\n" + readEventRules()

  def readEntityRules(): String = {
    val files = Seq(
      s"$entitiesDir/entities.yml")
    files map readResource mkString "\n\n"
  }

  def readModificationRules(): String =
    readResource(s"$modificationsDir/modifications.yml")

  def readEventRules(): String = {
    val files = Seq(
      s"$eventsDir/phospho_events.yml",
      s"$eventsDir/ubiq_events.yml",
      s"$eventsDir/hydrox_events.yml",
      s"$eventsDir/hydrolysis_events.yml",
      s"$eventsDir/bind_events.yml",
      s"$eventsDir/transcription_events.yml",
      s"$eventsDir/regulation_events.yml",
      s"$eventsDir/neg_reg_events.yml",
      s"$eventsDir/pos_reg_events.yml",
      s"$eventsDir/translocation_events.yml")
    files map readResource mkString "\n\n"
  }

  def readResource(filename: String) = {
    val source = io.Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }

  // FIXME placeholder
  // this function should remove mentions that were converted
  // into modifications of other mentions
  def filterModifiedEntities(ms: Seq[BioMention]): Seq[BioMention] = ms

  // This function should set the right displayMention for each mention.
  // By default the displayMention is set to the main label of the mention,
  // so sometimes it may not require modification
  def resolveDisplay(ms: Seq[BioMention]): Seq[BioMention] = {
    // first, let's make sure displayLabel is set to the default value
    ms.foreach(m => m.displayLabel = m.label)

    // now let's try to disambiguate Gene_or_gene_product that participate in events
    for(m <- ms) {
      if(m.labels.contains("Event")) {
        val parents = new HashSet[String]
        resolveDisplayForArguments(m, parents)
      }
    }
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
      if(em.xref.isDefined && em.xref.get.namespace == "interpro") {
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
