package org.clulab.reach.export

import java.util.Date
import org.clulab.reach.FriesEntry
import org.clulab.reach.TestUtils._
import org.clulab.reach.export.fries._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.scalatest.{FlatSpec, Matchers}


/**
  * Test the JSON output by the FRIES output formatter program.
  *   Written by: Tom Hicks. 5/19/2016
  *   Last Modified: Update test for switch to Jackson.
  */
class TestFriesOutput extends FlatSpec with Matchers {

  val paperId = "PMC123456"
  val startTime = new Date()
  val outputter = new FriesOutput()

  val text1 = "AKT1 phosphorylates PTHR2"
  val mentions1 = getBioMentions(text1)
  val entry = FriesEntry("text1", "1", "test", "test", isTitle = false, text1, None)
  val jStr = outputter.toJSON(paperId, mentions1, Seq(entry), startTime, new Date(), paperId)
  val json = parse(jStr)

  text1 should "produce 4 mentions" in {
    // printMentions(Try(mentions1), true)     // DEBUGGING
    mentions1 should have size (4)
  }

  it should "produce valid JSON string" in {
    // println(s"jStr=$jStr")                  // DEBUGGING
    jStr should not be empty
    jStr should include ("\"events\" :")
    jStr should include ("\"entities\" :")
    jStr should include ("\"sentences\" :")
  }

  it should "produce parseable JSON with 3 top-level sections" in {
    ((json \ "events" \ "object-type").values == "frame-collection") should be (true)
    ((json \ "entities" \ "object-type").values == "frame-collection") should be (true)
    ((json \ "sentences" \ "object-type").values == "frame-collection") should be (true)
  }

  it should "output organization and component in meta-data" in {
    val orgList = json \\ "object-meta" \\ "organization" \\ classOf[JString]
    orgList.isEmpty should be (false)
    orgList.size should be (3)
    orgList.forall(_ == "UAZ") should be (true)
    val didList = json \\ "object-meta" \\ "doc-id" \\ classOf[JString]
    didList.isEmpty should be (false)
    didList.size should be (3)
    didList.forall(_ == paperId) should be (true)
  }

  it should "have 1 sentence and one passage frame-type" in {
    val passages = json \ "sentences" \ "frames" \\ "frame-type" \\ classOf[JString]
    passages should have size (2)
    passages should contain ("passage")
    passages should contain ("sentence")
  }

  it should "have the right text" in {
    val texts = json \ "sentences" \ "frames" \\ "text" \\ classOf[JString]
    texts should have size (2)
    all (texts) should be (text1)
  }

  it should "have 2 event mentions: 1 phospho and 1 pos-reg" in {
    val subtypeList = json \ "events" \ "frames" \\ "subtype" \\ classOf[JString]
    subtypeList.isEmpty should be (false)
    subtypeList.size should be (2)
    subtypeList should contain("positive-regulation")
    subtypeList should contain("phosphorylation")
  }

  it should "have the expected arguments argument-types" in {
    val types = json \ "events" \ "frames" \ "arguments" \\ "argument-type" \\ classOf[JString]
    types.size should be (3)
    types.count(_ == "event") should be (1)
    types.count(_ == "entity") should be (2)
  }

  it should "have the expected arguments types" in {
    val types = json \ "events" \ "frames" \ "arguments" \\ "type" \\ classOf[JString]
    types.size should be (3)
    types should contain("controller")
    types should contain("controlled")
    types should contain("theme")
  }

  it should "regulation marked as direct" in {
    val frames = json \ "events" \ "frames" \\ "is-direct" \\ classOf[JBool]
    frames.size should be (1)
    frames should contain(true)
  }

  it should "have phosphorylation trigger" in {
    val triggers = (json \ "events" \ "frames") \\ "trigger" \\ classOf[JString]
    triggers should have size (1)
    all (triggers) should be ("phosphorylates")
  }

  it should "have 2 protein entities" in {
    val ents = (json \ "entities" \ "frames") \\ "type" \\ classOf[JString]
    ents should have size (2)
    all (ents) should be ("protein")
  }

  it should "have the given protein names" in {
    val tvals = json \ "entities" \ "frames" \\ "text" \\ classOf[JString]
    tvals should have size (2)
    tvals should contain("AKT1")
    tvals should contain("PTHR2")
  }

  it should "have xrefs with expected namespace values" in {
    val nsList = json \ "entities" \ "frames" \ "xrefs" \\ "namespace" \\ classOf[JString]
    nsList should have size (2)
    all (nsList) should be ("uniprot")
    (nsList.filter(ns => ns == "uniprot")) should have size (2)
  }

  it should "have xrefs with expected object-type values" in {
    val otList = json \ "entities" \ "frames" \ "xrefs" \\ "object-type" \\ classOf[JString]
    otList should have size (2)
    all (otList) should be ("db-reference")
    (otList.filter(ns => ns == "db-reference")) should have size (2)
  }

  it should "have xrefs with expected ID values" in {
    val idList = json \ "entities" \ "frames" \ "xrefs" \\ "id" \\ classOf[JString]
    idList should contain ("P31749")
    idList should contain ("P49190")
  }


  // Test output for regulation of regulation:
  val text2 = "The phosphorylation of AFT by BEF is inhibited by the ubiquitination of Akt."
  val mentions2 = getBioMentions(text2)
  val entry2 = FriesEntry("text2", "1", "test", "test", isTitle = false, text2, None)
  val jStr2 = outputter.toJSON(paperId, mentions2, Seq(entry2), startTime, new Date(), paperId)
  val json2 = parse(jStr2)

  text2 should "produce valid JSON string" in {
    // println(s"jStr2=$jStr2")                // DEBUGGING
    jStr2 should not be empty
    jStr2 should include ("\"events\" :")
    jStr2 should include ("\"entities\" :")
    jStr2 should include ("\"sentences\" :")
  }

  it should "produce parseable JSON with 3 top-level sections" in {
    ((json2 \ "events" \ "object-type").values == "frame-collection") should be (true)
    ((json2 \ "entities" \ "object-type").values == "frame-collection") should be (true)
    ((json2 \ "sentences" \ "object-type").values == "frame-collection") should be (true)
  }

  it should "have 5 event mentions: 1 phos, 1 ubiq, 1 neg-reg and 2 pos-reg" in {
    val subtypeList = json2 \ "events" \ "frames" \\ "subtype" \\ classOf[JString]
    subtypeList.isEmpty should be (false)
    subtypeList.size should be (4)
    subtypeList.count(_ == "phosphorylation") should be (1)
    subtypeList.count(_ == "ubiquitination") should be (1)
    subtypeList.count(_ == "negative-regulation") should be (1)
    subtypeList.count(_ == "positive-regulation") should be (1)
  }

  // Test output for regulation *by* a regulation:
  val text3 = "The phosphorylation of AFT by BEF inhibits the ubiquitination of Akt."
  val mentions3 = getBioMentions(text3)
  val entry3 = FriesEntry("text3", "1", "test", "test", isTitle = false, text3, None)
  val jStr3 = outputter.toJSON(paperId, mentions2, Seq(entry2), startTime, new Date(), paperId)
  val json3 = parse(jStr3)

  text3 should "produce valid JSON string" in {
    // println(s"jStr3=$jStr3")                // DEBUGGING
    jStr3 should not be empty
    jStr3 should include ("\"events\" :")
    jStr3 should include ("\"entities\" :")
    jStr3 should include ("\"sentences\" :")
  }

  it should "produce parseable JSON with 3 top-level sections" in {
    ((json3 \ "events" \ "object-type").values == "frame-collection") should be (true)
    ((json3 \ "entities" \ "object-type").values == "frame-collection") should be (true)
    ((json3 \ "sentences" \ "object-type").values == "frame-collection") should be (true)
  }

  it should "have 4 event mentions: 1 phos, 1 ubiq, 1 neg-reg and 2 pos-reg" in {
    val subtypeList = json3 \ "events" \ "frames" \\ "subtype" \\ classOf[JString]
    subtypeList.isEmpty should be (false)
    subtypeList.size should be (4)
    subtypeList.count(_ == "phosphorylation") should be (1)
    subtypeList.count(_ == "ubiquitination") should be (1)
    subtypeList.count(_ == "negative-regulation") should be (1)
    subtypeList.count(_ == "positive-regulation") should be (1)
  }

}
