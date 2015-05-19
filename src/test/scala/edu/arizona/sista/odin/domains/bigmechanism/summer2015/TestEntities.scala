package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import scala.util.Try
import org.scalatest._
import edu.arizona.sista.bionlp._
import TestUtils._

class TestEntities extends FlatSpec with Matchers {

  // test data
  val text = "The ubiquitinated Ras protein phosphorylates AKT."

  "ReachSystem" should "extract mentions from FriesEntry" in {
    val entry = FriesEntry(docId, chunkId, "example", "example", false, text)
    val result = Try(testReach.extractFrom(entry))
    result.isSuccess should be (true)
  }

  it should "extract mentions from text" in {
    val result = Try(testReach.extractFrom(text, docId, chunkId))
    result.isSuccess should be (true)
  }

  it should "extract mentions from document" in {
    val doc = testReach.processor.annotate(text, keepText = true)
    doc.id = Some(docId)
    val result = Try(testReach.extractFrom(doc))
    result.isSuccess should be (true)
  }

  it should "not extract mentions from document without id" in {
    val doc = testReach.processor.annotate(text, keepText = true)
    val result = Try(testReach.extractFrom(doc))
    result.isSuccess should be (false)
  }

  it should "not extract mentions from document without original text" in {
    val doc = testReach.processor.annotate(text, keepText = false)
    doc.id = Some(docId)
    val result = Try(testReach.extractFrom(doc))
    result.isSuccess should be (false)
  }

  it should "extract grounded entities only" in {
    val doc = testReach.mkDoc(text, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    mentions
      // We need ModificationTriggers in the entityEngine because we use them in our rules
      .filter(_.label != "ModificationTrigger")
      .forall(_.isGrounded) should be (true)
  }

  it should "extract an empty list without entities" in {
    val doc = testReach.mkDoc(text, docId, chunkId)
    val mentions = testReach.extractEventsFrom(doc, Nil)
    mentions.isEmpty should be (true)
  }

}
