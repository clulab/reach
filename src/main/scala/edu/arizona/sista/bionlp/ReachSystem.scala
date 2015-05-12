package edu.arizona.sista.bionlp

import edu.arizona.sista.odin._
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.odin.domains.bigmechanism.summer2015.{ LocalGrounder, Coref }
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

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
  // this engine extracts simple and recursive events and applies correference
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
    extractEventsFrom(doc, entities)
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

  def readRules(): String =
    readEntityRules() + "\n\n" + readModificationRules() + "\n\n" + readEventRules()

  def readEntityRules(): String = {
    val files = Seq(
      s"$resourcesDir/custom_entities.yml",
      s"$resourcesDir/model_entities.yml")
    files map readResource mkString "\n\n"
  }

  def readModificationRules(): String =
    readResource(s"$resourcesDir/grouping_rules.yml")

  def readEventRules(): String = {
    val files = Seq(
      s"$resourcesDir/phospho_events.yml",
      s"$resourcesDir/ubiq_events.yml",
      s"$resourcesDir/hydrox_events.yml",
      s"$resourcesDir/hydrolysis_events.yml",
      s"$resourcesDir/bind_events.yml",
      s"$resourcesDir/exchange_events.yml",
      s"$resourcesDir/degrad_events.yml",
      s"$resourcesDir/transcription_events.yml",
      s"$resourcesDir/regulation_events.yml",
      s"$resourcesDir/neg_reg_events.yml",
      s"$resourcesDir/pos_reg_events.yml",
      s"$resourcesDir/transport_events.yml")
    files map readResource mkString "\n\n"
  }

  def readResource(filename: String) = {
    val source = io.Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }

  // placeholder
  def filterModifiedEntities(ms: Seq[BioMention]): Seq[BioMention] = ms

}
