package edu.arizona.sista.bionlp

import scala.util.{ Try, Success, Failure }
import org.scalatest._
import edu.arizona.sista.bionlp.mentions._

class TestReachSystem extends FlatSpec with Matchers {
  // instantiate ReachSytem for tests
  val reach = new ReachSystem

  // test data
  val text = "The ubiquitinated Ras protein phosphorylates AKT."
  val docId = "testdoc"
  val chunkId = "1"

  "ReachSystem" should "extract mentions from FriesEntry" in {
    val entry = FriesEntry(docId, chunkId, "example", "example", false, text)
    val result = Try(reach.extractFrom(entry))
    result.isSuccess should be (true)
  }

  it should "extract mentions from text" in {
    val result = Try(reach.extractFrom(text, docId, chunkId))
    result.isSuccess should be (true)
  }

  it should "extract mentions from document" in {
    val doc = reach.processor.annotate(text, keepText = true)
    doc.id = Some(docId)
    val result = Try(reach.extractFrom(doc))
    result.isSuccess should be (true)
  }

  it should "not extract mentions from document without id" in {
    val doc = reach.processor.annotate(text, keepText = true)
    val result = Try(reach.extractFrom(doc))
    result.isSuccess should be (false)
  }

  it should "not extract mentions from document without original text" in {
    val doc = reach.processor.annotate(text, keepText = false)
    doc.id = Some(docId)
    val result = Try(reach.extractFrom(doc))
    result.isSuccess should be (false)
  }

  it should "extract grounded entities only" in {
    val doc = reach.mkDoc(text, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    mentions.forall(_.isGrounded) should be (true)
  }

  // the example text says that Ras is ubiquitinated
  // that should be reflected as a PTM in ras.modifications
  it should "extract Ras with a PTM" in {
    val doc = reach.mkDoc(text, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    // there is only one PTM in the example text
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ubiquitinated" should be (true)
  }

  it should "extract an empty list without entities" in {
    val doc = reach.mkDoc(text, docId, chunkId)
    val mentions = reach.extractEventsFrom(doc, Nil)
    mentions.isEmpty should be (true)
  }

  // there is a phosphorilation event in the example text
  it should "extract a phosphorylation" in {
    val mentions = reach.extractFrom(text, docId, chunkId)
    val phospho = mentions.find(_.label == "Phosphorylation")
    phospho.isDefined should be (true)
    phospho.get.arguments.contains("theme") should be (true)
    // simple events get a single argument
    phospho.get.arguments("theme").size should be (1)
    // simple events shouldn't have causes
    // because they are promoted to Positive_regulations
    phospho.get.arguments.contains("cause") should be (false)
    phospho.get.arguments("theme").head.text.contains("AKT") should be (true)
  }

  // there is an implicit regulation in the example text
  // it is the cause of the phosphorylation
  it should "extract a regulation" in {
    val mentions = reach.extractFrom(text, docId, chunkId)
    val reg = mentions.find(_.label == "Positive_regulation")
    reg.isDefined should be (true)
    reg.get.arguments.contains("controlled") should be (true)
    reg.get.arguments.contains("controller") should be (true)
    reg.get.arguments("controlled").head.label == "Phosphorylation" should be (true)
    reg.get.arguments("controller").head.text.contains("Ras") should be (true)
  }

}
