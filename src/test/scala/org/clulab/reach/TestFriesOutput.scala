package org.clulab.reach

import java.util.Date
import org.clulab.reach.extern.export.fries._
import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import org.json4s._
import org.json4s.native.JsonMethods._


/**
  * Test the JSON output by the FRIES output formatter program.
  *   Written by: Tom Hicks. 5/19/2016
  *   Last Modified: Update for fix of issue #236.
  */
class TestFriesOutput extends FlatSpec with Matchers {

  val paperId = "PMC123456"
  val startTime = new Date()
  val outputter = new FriesOutput()

  val text1 = "AKT1 phosphorylates PTHR2"
  val mentions1 = getBioMentions(text1)
  val entry = FriesEntry("text1", "1", "test", "test", isTitle = false, text1)
  val jStr = outputter.toJSON(paperId, mentions1, Seq(entry), startTime, new Date(), paperId)
  val json = parse(jStr)

  text1 should "produce 4 mentions" in {
    // printMentions(Try(mentions1), true)      // DEBUGGING
    mentions1 should have size (4)
  }

  it should "produce valid JSON string" in {
    // println(s"JSON=$jStr")                  // DEBUGGING
    jStr should not be empty
    jStr should include ("\"events\":")
    jStr should include ("\"entities\":")
    jStr should include ("\"sentences\":")
  }

  it should "produce parseable JSON with 3 top-level sections" in {
    ((json \ "events" \ "object-type").values == "frame-collection") should be (true)
    ((json \ "entities" \ "object-type").values == "frame-collection") should be (true)
    ((json \ "sentences" \ "object-type").values == "frame-collection") should be (true)
  }

  it should "output organization and component in meta-data" in {
    val orgList = json \\ "object-meta" \\ "organization" \\ classOf[JString]
    orgList.isEmpty should be (false)
    (orgList.size == 3) should be (true)
    orgList.forall(_ == "UAZ") should be (true)
    val didList = json \\ "object-meta" \\ "doc-id" \\ classOf[JString]
    didList.isEmpty should be (false)
    (didList.size == 3) should be (true)
    didList.forall(_ == paperId) should be (true)
  }

  it should "have 1 sentence and one passage" in {
    val pass = ((json \ "sentences" \ "frames")(0) \ "frame-type").values
    (pass == "passage") should be (true)
    val pText = ((json \ "sentences" \ "frames")(0) \ "text").values
    (pText == text1) should be (true)
    val sent = ((json \ "sentences" \ "frames")(1) \ "frame-type").values
    (sent == "sentence") should be (true)
    val sText = ((json \ "sentences" \ "frames")(1) \ "text").values
    (sText == text1) should be (true)
  }

  it should "have 2 event mentions: 1 phospho and 1 pos-reg" in {
    val subtypeList = json \ "events" \ "frames" \\ "subtype" \\ classOf[JString]
    subtypeList.isEmpty should be (false)
    (subtypeList.size == 2) should be (true)
    (subtypeList(0) == "phosphorylation") should be (true)
    (subtypeList(1) == "positive-regulation") should be (true)
  }

  it should "have the expected regulation arguments" in {
    val args = (json \ "events" \ "frames")(1) \ "arguments" \\ classOf[JObject]
    (args.size == 2) should be (true)
    val args0 = args(0)
    (args0.getOrElse("object-type", "") == "argument") should be (true)
    (args0.getOrElse("argument-type", "") == "event") should be (true)
    (args0.getOrElse("type", "") == "controlled") should be (true)
    val args1 = args(1)
    (args1.getOrElse("object-type", "") == "argument") should be (true)
    (args1.getOrElse("argument-type", "") == "entity") should be (true)
    (args1.getOrElse("type", "") == "controller") should be (true)
  }

  it should "mark regulation as direct" in {
    val isDirect = ((json \ "events" \ "frames")(0) \ "is-direct") \\ classOf[JBool]
    isDirect(0) should be (true)
  }

  it should "have phosphorylation trigger" in {
    val trigger = ((json \ "events" \ "frames")(0) \ "trigger").values
    (trigger == "phosphorylates") should be (true)
  }

  it should "have 2 protein entities" in {
    val ents = (json \ "entities" \ "frames") \\ "type" \\ classOf[JString]
    ents should not be empty
    ents should have size 2
    all (ents) should be ("protein")
  }

  it should "have the given protein names" in {
    val aVal = ((json \ "entities" \ "frames")(0) \ "text").values
    (aVal == "AKT1") should be (true)
    val pVal = ((json \ "entities" \ "frames")(1) \ "text").values
    (pVal == "PTHR2") should be (true)
  }

  it should "have the expected xref properties" in {
    val xrefs = json \ "entities" \ "frames" \ "xrefs" \\ classOf[JObject]
    xrefs should have size 2
    val xrefs0 = xrefs(0)
    xrefs0 should contain ("namespace" -> "uniprot")
    xrefs0 should contain ("object-type" -> "db-reference")
    xrefs0 should contain ("id" -> "P31749")
    val xrefs1 = xrefs(1)
    xrefs1 should contain ("namespace" -> "uniprot")
    xrefs1 should contain ("object-type" -> "db-reference")
    xrefs1 should contain ("id" -> "P49190")
  }


  // Test output for regulation of regulation:
  val text2 = "The phosphorylation of AFT by BEF is inhibited by the ubiquitination of Akt."
  val mentions2 = getBioMentions(text2)
  val entry2 = FriesEntry("text2", "1", "test", "test", isTitle = false, text2)
  val jStr2 = outputter.toJSON(paperId, mentions2, Seq(entry2), startTime, new Date(), paperId)
  val json2 = parse(jStr2)

  "text2" should "produce valid JSON string" in {
    // println(s"JSON=$jStr2")                  // DEBUGGING
    jStr2 should not be empty
    jStr2 should include ("\"events\":")
    jStr2 should include ("\"entities\":")
    jStr2 should include ("\"sentences\":")
  }

  it should "produce parseable JSON with 3 top-level sections" in {
    ((json2 \ "events" \ "object-type").values == "frame-collection") should be (true)
    ((json2 \ "entities" \ "object-type").values == "frame-collection") should be (true)
    ((json2 \ "sentences" \ "object-type").values == "frame-collection") should be (true)
  }

  it should "have 5 event mentions: 1 phos, 1 ubiq, 1 neg-reg and 2 pos-reg" in {
    val subtypeList = json2 \ "events" \ "frames" \\ "subtype" \\ classOf[JString]
    subtypeList.isEmpty should be (false)
    (subtypeList.size == 5) should be (true)
    subtypeList.count(_ == "phosphorylation") should be (1)
    subtypeList.count(_ == "ubiquitination") should be (1)
    subtypeList.count(_ == "negative-regulation") should be (1)
    subtypeList.count(_ == "positive-regulation") should be (2)
  }

  // Test output for regulation *by* a regulation:
  val text3 = "The phosphorylation of AFT by BEF inhibits the ubiquitination of Akt."
  val mentions3 = getBioMentions(text3)
  val entry3 = FriesEntry("text3", "1", "test", "test", isTitle = false, text3)
  val jStr3 = outputter.toJSON(paperId, mentions2, Seq(entry2), startTime, new Date(), paperId)
  val json3 = parse(jStr3)

  text3 should "produce valid JSON string" in {
    // println(s"JSON=$jStr2")                  // DEBUGGING
    jStr3 should not be empty
    jStr3 should include ("\"events\":")
    jStr3 should include ("\"entities\":")
    jStr3 should include ("\"sentences\":")
  }

  it should "produce parseable JSON with 3 top-level sections" in {
    ((json3 \ "events" \ "object-type").values == "frame-collection") should be (true)
    ((json3 \ "entities" \ "object-type").values == "frame-collection") should be (true)
    ((json3 \ "sentences" \ "object-type").values == "frame-collection") should be (true)
  }

  it should "have 4 event mentions: 1 phos, 1 ubiq, 1 neg-reg and 2 pos-reg" in {
    val subtypeList = json3 \ "events" \ "frames" \\ "subtype" \\ classOf[JString]
    // print(subtypeList)
    subtypeList.isEmpty should be (false)
    subtypeList.size should be (5)
    subtypeList.count(_ == "phosphorylation") should be (1)
    subtypeList.count(_ == "ubiquitination") should be (1)
    subtypeList.count(_ == "negative-regulation") should be (1)
    subtypeList.count(_ == "positive-regulation") should be (2)
  }

}
