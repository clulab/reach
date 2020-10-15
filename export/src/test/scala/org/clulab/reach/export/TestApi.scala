package org.clulab.reach.export

import org.clulab.reach.FriesEntry
import org.clulab.reach.TestUtils._
import org.clulab.reach.export.apis.Api
import org.scalatest._
import scala.io.Source


class TestApi extends FlatSpec with Matchers {

  // Test Scala API
  val text = "The ubiquitinated Ras protein phosphorylates AKT."
  val text2 = "It has recently been shown that oncogenic RAS can enhance the apoptotic function of p53 via ASPP1 and ASPP2"
  val textNone = "It is a nice day in the neighborhood with no events to be seen."
  def nxmlText = Source.fromURL(getClass.getResource("/inputs/nxml/PMC1240239.nxml")).mkString

  // basic function cases:
  it should "return Reach results from a FriesEntry" in {
    val entry = FriesEntry(docId, chunkId, "example", "example", isTitle = false, text)
    val results = Api.runOnFriesEntry(entry)
    results.isEmpty should be (false)
  }

  it should "return Reach results from text" in {
    val results = Api.runOnText(text)
    results.isEmpty should be (false)
  }

  it should "return Reach results from text given doc id" in {
    val results = Api.runOnText(text, docId)
    results.isEmpty should be (false)
  }

  it should "return Reach results from text given doc id and chunk id" in {
    val results = Api.runOnText(text, docId, chunkId)
    results.isEmpty should be (false)
  }

  it should "return Reach results from NXML text" in {
    val results = Api.runOnNxml(nxmlText)
    results.isEmpty should be (false)
  }

  // no results cases:
  textNone should "not contain any results from FriesEntry" in {
    val entry = FriesEntry(docId, chunkId, "example", "example", false, textNone)
    val results = Api.runOnFriesEntry(entry)
    results.size should be (0)
  }

  textNone should "not contain any results from text" in {
    val results = Api.runOnText(textNone)
    results.size should be (0)
  }

  textNone should "not contain any results from text given doc id" in {
    val results = Api.runOnText(textNone, docId)
    results.size should be (0)
  }

  textNone should "not contain any results from text given doc id and chunk id" in {
    val results = Api.runOnText(textNone, docId, chunkId)
    results.size should be (0)
  }

  // some results cases:
  text2 should "return 1 positive activation result from a FriesEntry" in {
    val entry = FriesEntry(docId, chunkId, "example", "example", isTitle = false, text2)
    val results = Api.runOnFriesEntry(entry)
    results.filter(_.label == "Positive_activation") should have size (1)
  }

  text2 should "return 4 entity results from a FriesEntry" in {
    val entry = FriesEntry(docId, chunkId, "example", "example", isTitle = false, text2)
    val results = Api.runOnFriesEntry(entry)
    hasEntity("RAS", results) should be (true)
    hasEntity("p53", results) should be (true)
    hasEntity("ASPP1", results) should be (true)
    hasEntity("ASPP2", results) should be (true)
  }

  text2 should "return 1 positive activation result from a text call" in {
    val results = Api.runOnText(text2)
    results.filter(_.label == "Positive_activation") should have size (1)
  }

  text2 should "return 4 entity results from a text call" in {
    val results = Api.runOnText(text2)
    hasEntity("RAS", results) should be (true)
    hasEntity("p53", results) should be (true)
    hasEntity("ASPP1", results) should be (true)
    hasEntity("ASPP2", results) should be (true)
  }

  text2 should "return 1 positive activation result from a text call given doc id" in {
    val results = Api.runOnText(text2, docId)
    results.filter(_.label == "Positive_activation") should have size (1)
  }

  text2 should "return 4 entity results from a text call given doc id" in {
    val results = Api.runOnText(text2, docId)
    hasEntity("RAS", results) should be (true)
    hasEntity("p53", results) should be (true)
    hasEntity("ASPP1", results) should be (true)
    hasEntity("ASPP2", results) should be (true)
  }

  text2 should "return 1 positive activation result from a text call given doc id and chunk id" in {
    val results = Api.runOnText(text2, docId, chunkId)
    results.filter(_.label == "Positive_activation") should have size (1)
  }

  text2 should "return 4 entity results from a text call given doc id and chunk id" in {
    val results = Api.runOnText(text2, docId, chunkId)
    hasEntity("RAS", results) should be (true)
    hasEntity("p53", results) should be (true)
    hasEntity("ASPP1", results) should be (true)
    hasEntity("ASPP2", results) should be (true)
  }

  it should "return 7 positive activation and 3 phosphorylation results from NXML test" in {
    val results = Api.runOnNxml(nxmlText)
    // This was previously 9 until mM was ruled out.
    results.filter(_.label == "Positive_activation") should have size (7)
    results.filter(_.label == "Phosphorylation") should have size (3)
  }

}
