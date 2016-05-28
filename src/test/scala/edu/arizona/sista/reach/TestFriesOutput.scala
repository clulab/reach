package edu.arizona.sista.reach

import java.util.Date

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.extern.export.fries._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.utils.Serializer

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try
import TestUtils._

import org.json4s._
import org.json4s.native.JsonMethods._

/**
  * Test the JSON output by the FRIES output formatter program.
  *   Written by: Tom Hicks. 5/19/2016
  *   Last Modified: Update for rename of argument label to type.
  */
class TestFriesOutput extends FlatSpec with Matchers {

  val paperId = "PMC123456"
  val startTime = new Date()
  val outputter = new FriesOutput()

  val text1 = "AKT1 phosphorylates PTHR2"
  val mentions1 = getBioMentions(text1)
  val entry = FriesEntry("text1", "1", "test", "test", false, text1)
  val jStr = outputter.toJSON(paperId, mentions1, Seq(entry), startTime, new Date(), s"${paperId}")
  val json = parse(jStr)

  text1 should "produce 4 mentions" in {
    // printMentions(Try(mentions1), true)      // DEBUGGING
    mentions1 should have size (4)
  }

  "text1" should "produce valid JSON string" in {
    // println(s"JSON=$jStr")                  // DEBUGGING
    (jStr.isEmpty) should be (false)
    (jStr.contains("\"events\":")) should be (true)
    (jStr.contains("\"entities\":")) should be (true)
    (jStr.contains("\"sentences\":")) should be (true)
  }

  "text1" should "produce parseable JSON with 3 top-level sections" in {
    ((json \ "events" \ "object-type").values == "frame-collection") should be (true)
    ((json \ "entities" \ "object-type").values == "frame-collection") should be (true)
    ((json \ "sentences" \ "object-type").values == "frame-collection") should be (true)
  }

  "text1" should "output organization and component in meta-data" in {
    val orgList = json \\ "object-meta" \\ "organization" \\ classOf[JString]
    orgList.isEmpty should be (false)
    (orgList.size == 3) should be (true)
    orgList.forall(_ == "UAZ") should be (true)
    val didList = json \\ "object-meta" \\ "doc-id" \\ classOf[JString]
    didList.isEmpty should be (false)
    (didList.size == 3) should be (true)
    didList.forall(_ == paperId) should be (true)
  }

  "text1" should "have 1 sentence and one passage" in {
    val pass = ((json \ "sentences" \ "frames")(0) \ "frame-type").values
    (pass == "passage") should be (true)
    val pText = ((json \ "sentences" \ "frames")(0) \ "text").values
    (pText == text1) should be (true)
    val sent = ((json \ "sentences" \ "frames")(1) \ "frame-type").values
    (sent == "sentence") should be (true)
    val sText = ((json \ "sentences" \ "frames")(1) \ "text").values
    (sText == text1) should be (true)
  }

  "text1" should "have 2 event mentions: 1 phospho and 1 pos-reg" in {
    val subtypeList = json \ "events" \ "frames" \\ "subtype" \\ classOf[JString]
    subtypeList.isEmpty should be (false)
    (subtypeList.size == 2) should be (true)
    (subtypeList(0) == "phosphorylation") should be (true)
    (subtypeList(1) == "positive-regulation") should be (true)
  }

  "text1 regulation" should "have the expected arguments" in {
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

  "text1" should "mark regulation as direct" in {
    val isDirect = ((json \ "events" \ "frames")(0) \ "is-direct") \\ classOf[JBool]
    (isDirect(0)) should be (true)
  }

  "text1" should "have phosphorylation trigger" in {
    val trigger = ((json \ "events" \ "frames")(0) \ "trigger").values
    (trigger == "phosphorylates") should be (true)
  }

  "text1" should "have 2 protein entities" in {
    val ents = (json \ "entities" \ "frames") \\ "type" \\ classOf[JString]
    ents.isEmpty should be (false)
    (ents.size == 2) should be (true)
    ents.forall(_ == "protein") should be (true)
  }

  "text1 entities" should "have the given protein names" in {
    val aVal = ((json \ "entities" \ "frames")(0) \ "text").values
    (aVal == "AKT1") should be (true)
    val pVal = ((json \ "entities" \ "frames")(1) \ "text").values
    (pVal == "PTHR2") should be (true)
  }

  "text1 xrefs" should "have the expected properties" in {
    val xrefs = json \ "entities" \ "frames" \ "xrefs" \\ classOf[JObject]
    (xrefs.size == 2) should be (true)
    val xrefs0 = xrefs(0)
    (xrefs0.getOrElse("namespace", "") == "uniprot") should be (true)
    (xrefs0.getOrElse("object-type", "") == "db-reference") should be (true)
    (xrefs0.getOrElse("id", "") == "P31749") should be (true)
    val xrefs1 = xrefs(1)
    (xrefs1.getOrElse("namespace", "") == "uniprot") should be (true)
    (xrefs1.getOrElse("object-type", "") == "db-reference") should be (true)
    (xrefs1.getOrElse("id", "") == "P49190") should be (true)
  }

}
