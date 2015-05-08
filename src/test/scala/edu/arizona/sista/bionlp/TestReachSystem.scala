package edu.arizona.sista.bionlp

import scala.util.{ Try, Success, Failure }
import org.scalatest._

class Test extends FlatSpec with Matchers {
  // instantiate ReachSytem for tests
  val reach = new ReachSystem

  // test data
  val text = "The Ras protein."
  val docId = "testdoc"
  val chunkId = "1"

  "ReachSystem" should "extract mentions from FriesEntry" in {
    val entry = FriesEntry(docId, chunkId, "introduction", "introduction", false, text)
    val result = Try(reach.extractFrom(entry))
    result.isSuccess should be (true)
  }

  it should "extract mentions from text" in {
    val result = Try(reach.extractFrom(text, chunkId, docId))
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
}
