package org.clulab.reach.serialization

import org.scalatest._
import org.clulab.reach.TestUtils._
import org.clulab.reach.serialization.json._
import org.json4s._


class TestJSONSerializer extends FlatSpec with Matchers {

  val text = "Phosphorylated MEK activates K-RAS."
  val mentions = getCorefmentionsFromText(text)

  "JSONSerializer" should "serialize/deserialize a Seq[CorefMention] to/from json correctly" in {
    val mentions2 = JSONSerializer.toCorefMentions(mentions.jsonAST)
    mentions2 should have size mentions.size
    mentions2.map(_.label) should equal (mentions.map(_.label))
    mentions2.map(_.document.equivalenceHash) should equal (mentions.map(_.document.equivalenceHash))
  }

  it should "serialize/deserialize a CorefMention to/from json correctly " in {
    val mns = JSONSerializer.toCorefMentions(mentions.head.completeAST)
    mns should have size 1
    val m = mns.head
    m.document.equivalenceHash should equal (mentions.head.document.equivalenceHash)
    m.tokenInterval should equal (mentions.head.tokenInterval)
  }

  it should "produce identical json for a Seq[CorefMention] before and after serialization/deserialization" in {
    mentions.json(true) == JSONSerializer.toCorefMentions(mentions.jsonAST).json(true)
  }

  s"json for '$text'" should "be identical before and after serialization/deserialzation" in {
    val mekmns = mentions.filter(_.text == "MEK")
    mekmns should have size 1
    val mek = mekmns.head
    mek.json(true) should equal (JSONSerializer.toCorefMentions(Seq(mek).jsonAST).head.json(true))
  }

  it should "contain a modification with \"modification-type\" PTM" in {
    val mekmns = mentions.filter(_.text == "MEK")
    mekmns should have size 1
    val mek = mekmns.head
    (mek.jsonAST \ "modifications" \ "modification-type").extract[String] should equal ("PTM")
  }

  it should "still contain a PTM after serialization/deserialization" in {
    val deserializedCorefMentions = JSONSerializer.toCorefMentions(mentions.jsonAST)
    val mekmns = deserializedCorefMentions.filter(_.text == "MEK")
    mekmns should have size 1
    val mek = mekmns.head
    (mek.jsonAST \ "modifications" \ "modification-type").extract[String] should equal ("PTM")
  }

  val text2 = "MEK activates K-RAS."
  s"json for '$text2'" should "NOT contain a \"modifications\" field" in {
    val mekmns = getCorefmentionsFromText(text2).filter(_.text == "MEK")
    mekmns should have size 1
    val mek = mekmns.head
    (mek.jsonAST \ "modifications") should equal(JNothing)
  }
}