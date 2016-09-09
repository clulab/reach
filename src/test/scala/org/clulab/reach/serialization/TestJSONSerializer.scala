package org.clulab.reach.serialization

import org.scalatest._
import org.clulab.reach.TestUtils._
import org.clulab.reach.serialization.json._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native._


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
    //println(s"${m.json(true)}")
    m.document.equivalenceHash should equal (mentions.head.document.equivalenceHash)
    m.tokenInterval should equal (mentions.head.tokenInterval)
  }

  s"json for '$text'" should "contain a modifications field" in {
    (mentions.jsonAST \\ "modifications") should not equal JNothing
  }

  val text2 = "MEK activates K-RAS."
  s"json for '$text2'" should "NOT contain a modifications field" in {
    val mns = getCorefmentionsFromText(text2)
    (mns.jsonAST \\ "modifications") should equal(JObject(List()))
  }
}