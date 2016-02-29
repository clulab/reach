package edu.arizona.sista.reach

import edu.arizona.sista.reach.nxml.FriesEntry

import scala.util.Try
import org.scalatest._
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

  val sent2 = "It has recently been shown that oncogenic RAS can enhance the apoptotic function of p53 via ASPP1 and ASPP2"
  sent2 should "contain 4 entities" in {
    val mentions = getBioMentions(sent2)
    hasEntity("RAS", mentions) should be (true)
    hasEntity("p53", mentions) should be (true)
    hasEntity("ASPP1", mentions) should be (true)
    hasEntity("ASPP2", mentions) should be (true)
  }

  val sent3 = "We hypothesized that MEK inhibition activates AKT by inhibiting ERK activity, which blocks an inhibitory threonine phosphorylation on the JM domains of EGFR and HER2, thereby increasing ERBB3 phosphorylation."
  sent3 should "contain at least 4 entities" in {
    val mentions = getBioMentions(sent3)
    hasEntity("ERK", mentions) should be (true)
    hasEntity("EGFR", mentions) should be (true)
    hasEntity("HER2", mentions) should be (true)
    hasEntity("ERBB3", mentions) should be (true)
  }

  val sent4 = "To test this hypothesis, we transiently transfected CHO-KI cells, which do not express ERBB receptors endogenously, with wildtype ERBB3 with either wild-type EGFR or EGFR T669A."
  sent4 should "contain at least 3 entities" in {
    val mentions = getBioMentions(sent4)
    hasEntity("ERBB receptors", mentions) should be (true)
    hasEntity("ERBB3", mentions) should be (true)
    hasEntity("EGFR", mentions) should be (true)
  }

  val sent5 = "See Figure S31 and Table R15"
  sent5 should "not contain any sites" in {
    val mentions = getBioMentions(sent5)
    mentions.count(_ matches "Site") should be (0)
  }

  val sent6 = "The K-Ras substrate and mTOR substrates shouldn't be found."
  sent6 should "not contain any entities (because of substrate constraint)" in {
    val mentions = getBioMentions(sent6)
    mentions.size should be (0)
  }

  // test recognition of BioProcess entities
  val sent7 = "In some cases, the presence of Ras inhibits autophagy."
  sent7 should "contain 1 BioProcess entity (\"autophagy\")" in {
    val mentions = getBioMentions(sent7)
    mentions.count (_ matches "BioProcess") should be (1) 
  }

  // test lookahead assertions on ner rules for GGP and Family entities
  val mekText = "the MEK family"
  mekText should "contain 1 Family entity for \"MEK\" even if the entity tag is B-Gene_or_gene_product" in {
    val doc = bioproc.annotate(mekText)
    val ggpLabels: Array[String] = Array("O", "B-Gene_or_gene_product", "O")
    // manipulate the document for this test
    doc.id = Some("MEK-test")
    doc.text = Some(mekText)
    doc.sentences.head.entities = Some(ggpLabels)
    val mentions = testReach.extractFrom(doc)
    mentions should have size (1)
    mentions.head matches "Family" should be (true)
  }
}
