package org.clulab.reach.mentions.serialization

import org.scalatest._
import scala.util.Try                       // do not remove: needed for debugging
import org.clulab.reach.TestUtils._
import org.clulab.reach.mentions.{MentionOps => MOps}
import org.clulab.reach.mentions.serialization.json._
import org.json4s._
import org.json4s.jackson.JsonMethods._


class TestJSONSerializer extends FlatSpec with Matchers {

  val text = "Phosphorylated MEK activates K-RAS."
  val mentions = getMentionsFromText(text)
  val corefmentions = mentions.map(_.toCorefMention)
  val biomentions = mentions.map(_.toBioMention)
  val mentionsJSON = readFileContent("inputs/json/serializedDocument.json")
  val mentionsAST = parse(mentionsJSON)

  // printMentions(Try(biomentions), true)        // DEBUGGING

  "JSONSerializer" should "serialize a Seq[BioMention] to json correctly" in {
    val mentions2 = JSONSerializer.toBioMentions(corefmentions.jsonAST)
    mentions2 should have size (corefmentions.size)
    mentions2.map(_.label) should equal (corefmentions.map(_.label))
    mentions2.map(_.document.equivalenceHash) should equal (corefmentions.map(_.document.equivalenceHash))
  }

  it should "deserialize a Seq[BioMention] from json correctly" in {
    val mentions2 = JSONSerializer.toBioMentions(mentionsAST)
    mentions2 should have size (biomentions.size)
    mentions2.map(_.label) should equal (biomentions.map(_.label))
    mentions2.map(_.document.equivalenceHash) should equal (biomentions.map(_.document.equivalenceHash))
  }

  it should "serialize/deserialize a BioMention to/from json correctly " in {
    val Seq(m) = biomentions.filter(m => (m matches "Entity") && (m.text == "MEK"))
    val mns = JSONSerializer.toBioMentions(m.completeAST)
    mns should have size (1)
    val Seq(deserializedm) = mns
    m.document.equivalenceHash should equal (deserializedm.document.equivalenceHash)
//    println(s"M (original) is ${org.clulab.reach.display.summarizeMention(m)}")
//    println(s"M (deserialized) is ${org.clulab.reach.display.summarizeMention(deserializedm)}")
    m.tokenInterval should equal (m.tokenInterval)
    m.grounding() should equal (m.grounding())
    // test deserialization
    val mentions2 = JSONSerializer.toBioMentions(mentionsAST)
    mentions2 should have size (mentions.size)
    val Seq(deserializedbm) = mentions2.filter(m => (m matches "Entity") && (m.text == "MEK"))
    deserializedbm.label should equal (m.label)
    deserializedbm.tokenInterval should equal (m.tokenInterval)
    deserializedbm.grounding() should equal (m.grounding())
  }

  // coref mention tests

  it should "serialize a Seq[CorefMention] to json correctly" in {
    val mentions2 = JSONSerializer.toCorefMentions(corefmentions.jsonAST)
    mentions2 should have size corefmentions.size
    mentions2.map(_.label) should equal (corefmentions.map(_.label))
    mentions2.map(_.document.equivalenceHash) should equal (corefmentions.map(_.document.equivalenceHash))
  }

  it should "deserialize a Seq[CorefMention] from json correctly" in {
    val mentions2 = JSONSerializer.toCorefMentions(mentionsAST)
    mentions2 should have size corefmentions.size
    mentions2.map(_.label) should equal (corefmentions.map(_.label))
    mentions2.map(_.document.equivalenceHash) should equal (corefmentions.map(_.document.equivalenceHash))
  }

  it should "serialize/deserialize a CorefMention to/from json correctly " in {
    val Seq(cm) = corefmentions.filter(m => (m matches "Entity") && (m.text == "MEK"))
    val mns = JSONSerializer.toCorefMentions(cm.completeAST)
    mns should have size 1
    val Seq(m) = mns
    m.document.equivalenceHash should equal (cm.document.equivalenceHash)
    m.tokenInterval should equal (cm.tokenInterval)
    m.grounding() should equal (cm.grounding())
  }

  it should "produce identical json for a Seq[CorefMention] before and after serialization/deserialization" in {
    corefmentions.json(true) == JSONSerializer.toCorefMentions(corefmentions.jsonAST).json(true)
  }

  s"json for '$text'" should "be identical before and after serialization/deserialzation" in {
    val mekmns = corefmentions.filter(_.text == "MEK")
    mekmns should have size 1
    val mek = mekmns.head
    mek.json(true) should equal (JSONSerializer.toCorefMentions(Seq(mek).jsonAST).head.json(true))
  }

  it should "contain a modification with \"modification-type\" PTM" in {
    val mekmns = corefmentions.filter(_.text == "MEK")
    mekmns should have size 1
    val mek = mekmns.head
    (mek.jsonAST \ "modifications" \\ "modification-type").extract[String] should equal ("PTM")
  }

  it should "still contain a PTM after serialization/deserialization" in {
    val deserializedCorefMentions = JSONSerializer.toCorefMentions(corefmentions.jsonAST)
    val mekmns = deserializedCorefMentions.filter(_.text == "MEK")
    mekmns should have size 1
    val mek = mekmns.head
    (mek.jsonAST \ "modifications" \\ "modification-type").extract[String] should equal ("PTM")
  }

  val text2 = "MEK activates K-RAS."
  s"json for '$text2'" should "NOT contain a \"modifications\" field" in {
    val mekmns = getCorefmentionsFromText(text2).filter(_.text == "MEK")
    mekmns should have size 1
    val mek = mekmns.head
    (mek.jsonAST \ "modifications") should equal(JNothing)
  }
}
