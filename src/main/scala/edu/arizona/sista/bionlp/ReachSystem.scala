package edu.arizona.sista.bionlp

import java.io.File
import edu.arizona.sista.odin._
import edu.arizona.sista.odin.domains.bigmechanism.summer2015.{ LocalGrounder, Coref }
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

class ReachSystem {
  import ReachSystem._

  // read rule files
  val rules = readRules()
  // initialize actions object
  val actions = new DarpaActions
  // initialize grounder
  val grounder = new LocalGrounder
  // initialize coref
  val coref = new Coref
  // define flow
  val flow = grounder andThen coref
  // start engine
  val engine = ExtractorEngine(rules, actions, flow.apply)
  // initialize processor
  val processor = new BioNLPProcessor
  processor.annotate("something")

  def extractFrom(entry: FriesEntry): Seq[Mention] =
    extractFrom(entry.text, entry.name, entry.chunkId)

  def extractFrom(text: String, docId: String, chunkId: String): Seq[Mention] = {
    val name = s"${docId}_${chunkId}"
    val doc = processor.annotate(text, keepText = true)
    doc.id = Some(name)
    extractFrom(doc)
  }

  def extractFrom(doc: Document): Seq[Mention] = {
    require(doc.id.isDefined, "document must have an id")
    require(doc.text.isDefined, "document should keep original text")
    engine.extractFrom(doc)
  }


}

object ReachSystem {
  val resourcesDir = "/edu/arizona/sista/odin/domains/bigmechanism/dryrun2015"

  def readRules(): String =
    readEntityRules() + "\n\n" + readEventRules()

  def readEntityRules(): String = {
    val files = Seq(
      s"$resourcesDir/custom_entities.yml",
      s"$resourcesDir/model_entities.yml")
    files map readResource mkString "\n\n"
  }

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
}
