package org.clulab.reach.export.apis.open

import scala.io.Source
import scala.util.Try

import org.scalatest._

import org.clulab.odin.Mention
import org.clulab.reach.TestUtils._
import org.clulab.reach.utils.MentionManager

/**
  * Tests of the Open System class.
  *   Written by: Tom Hicks. 7/14/2017.
  *   Last Modified: Initial creation.
  */
class TestOpenSystem extends FlatSpec with Matchers {
  val mentionManager = new MentionManager()
  def printMentions(result:Try[Seq[Mention]], verbose:Boolean = true):Seq[Mention] = {
    val mentions = result.get
    if (verbose) {
      println("Mentions:")
      for (m <- mentions) {
        mentionManager.mentionToStrings(m).foreach(println(_))
        println()
      }
    }
    mentions
  }

  val emptyText = ""

  val leoco = Source.fromURL(getClass.getResource("/inputs/inputOpenTexts/Leo-coref.txt")).mkString
//  val sally = Source.fromURL(getClass.getResource("/inputs/inputOpenTexts/Sally.txt")).mkString
  val timmy = Source.fromURL(getClass.getResource("/inputs/inputOpenTexts/Timmy.txt")).mkString

  val nerRules = Source.fromURL(getClass.getResource("/inputs/inputRulesets/small-ner.yml")).mkString
  val xpRules = Source.fromURL(getClass.getResource("/inputs/inputRulesets/xp-surface.yml")).mkString

  // instance of class under test
  val openSys = new OpenSystem

  "Open System" should "make a document from empty text" in {
    val doc = openSys.mkDoc(emptyText)
    (doc) should not be (null)
    (doc.sentences.size) should equal(0)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "make a document from the given event text" in {
    val doc = openSys.mkDoc(timmy)
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "not extract events from empty text using XP rules" in {
    val doc = openSys.mkDoc(emptyText)
    val extract = openSys.extractFrom(xpRules, doc)
    (extract.isSuccess) should be (true)
    (extract.get) should be (empty)
  }

  it should "not extract events from empty text using NER rules" in {
    val doc = openSys.mkDoc(emptyText)
    val extract = openSys.extractFrom(nerRules, doc)
    (extract.isSuccess) should be (true)
    (extract.get) should be (empty)
  }

  it should "extract events from event text using XP rules" in {
    val doc = openSys.mkDoc(timmy)
    val extract = openSys.extractFrom(xpRules, doc)
    (extract.isSuccess) should be (true)
    // printMentions(extract)                  // DEBUGGING
    val mentions = extract.get
    (mentions) should not be (empty)
  }

  it should "extract events from event text using NER rules" in {
    val doc = openSys.mkDoc(timmy)
    val extract = openSys.extractFrom(nerRules, doc)
    (extract.isSuccess) should be (true)
    // printMentions(extract)                  // DEBUGGING
    val mentions = extract.get
    (mentions) should not be (empty)
  }

  it should "extract events from coref event text using XP rules" in {
    val doc = openSys.mkDoc(leoco)
    val extract = openSys.extractFrom(xpRules, doc)
    (extract.isSuccess) should be (true)
    // printMentions(extract)                  // DEBUGGING
    val mentions = extract.get
    (mentions) should not be (empty)
  }

  it should "extract events from coref event text using NER rules" in {
    val doc = openSys.mkDoc(leoco)
    val extract = openSys.extractFrom(nerRules, doc)
    (extract.isSuccess) should be (true)
    // printMentions(extract)                  // DEBUGGING
    val mentions = extract.get
    (mentions) should not be (empty)
  }

}
