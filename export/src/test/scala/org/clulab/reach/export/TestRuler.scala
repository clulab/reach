package org.clulab.reach.export.apis

import scala.io.Source
import scala.util.Try

import org.scalatest._

import org.clulab.reach.TestUtils._
// import org.clulab.reach.export.Ruler

/**
  * Tests of the open domain Ruler class.
  *   Written by: Tom Hicks. 7/14/2017.
  *   Last Modified: Initial creation.
  */
class TestRuler extends FlatSpec with Matchers {

  val emptyText = ""
  val leoco = Source.fromURL(getClass.getResource("/inputs/inputOpenTexts/Leo-coref.txt")).mkString
  val sally = Source.fromURL(getClass.getResource("/inputs/inputOpenTexts/Sally.txt")).mkString
  val timmy = Source.fromURL(getClass.getResource("/inputs/inputOpenTexts/Timmy.txt")).mkString
  val bioabc = Source.fromURL(getClass.getResource("/inputs/inputBioTexts/BIO-A-B-C.txt")).mkString

  val emptyRules = ""
  val badRule = """
- namer: "subj"
  labels: [Subject, Entity]
  priority: 1
  type: token
  unit: "tag"
  pattern: |
    /^N/* [incoming=/^nsubj/] /^N/*
"""                                         // bad names for name and label fields
  val nerRules = Source.fromURL(getClass.getResource("/inputs/inputRulesets/small-ner.yml")).mkString
  val xpRules = Source.fromURL(getClass.getResource("/inputs/inputRulesets/xp-surface.yml")).mkString

  // test runOpen
  "open Ruler.runOpen" should "return error results for empty text / empty rules" in {
    val rr = Ruler.runOpen(emptyText, emptyRules)
    (rr) should not be (null)
    (rr.text) should equal (emptyText)
    (rr.rules) should equal (emptyRules)
    (rr.error) should not be (null)
    (rr.error(1)) should not be (empty)
    (rr.error(1)) should equal ("rulesStr is empty") // error message present
  }

  it should "not fail for empty text / good rules" in {
    val rr = Ruler.runOpen(emptyText, xpRules)
    (rr) should not be (null)
    (rr.error) should be (null)
    (rr.text) should equal (emptyText)
    (rr.rules) should equal (xpRules)
    (rr.eventAnnotations) should be (empty)
    (rr.syntaxAnnotations) should be (empty)
    (rr.syntaxTokens) should be (empty)
    (rr.syntaxTrees) should be (empty)
    (rr.ruleMap) should not be (empty)
  }

  it should "succeed for good text / good XP rules" in {
    val rr = Ruler.runOpen(sally, xpRules)
    (rr) should not be (null)
    (rr.error) should be (null)
    (rr.text) should equal (sally)
    (rr.rules) should equal (xpRules)
    (rr.eventAnnotations) should not be (empty)
    (rr.syntaxAnnotations) should not be (empty)
    (rr.syntaxTokens) should not be (empty)
    (rr.syntaxTrees) should not be (empty)
    (rr.ruleMap) should not be (empty)
  }

  it should "succeed for good coref text / good NER rules" in {
    val rr = Ruler.runOpen(leoco, nerRules)
    (rr) should not be (null)
    (rr.error) should be (null)
    (rr.text) should equal (leoco)
    (rr.rules) should equal (nerRules)
    (rr.eventAnnotations) should not be (empty)
    (rr.syntaxAnnotations) should not be (empty)
    (rr.syntaxTokens) should not be (empty)
    (rr.syntaxTrees) should not be (empty)
    (rr.ruleMap) should not be (empty)
  }

  it should "fail for good text / bad rules" in {
    val rr = Ruler.runOpen(timmy, badRule)
    (rr) should not be (null)
    (rr.text) should equal (timmy)
    (rr.rules) should equal (badRule)
    (rr.eventAnnotations) should be (null)
    (rr.syntaxAnnotations) should be (null)
    (rr.syntaxTokens) should not be (empty)
    (rr.syntaxTrees) should not be (empty)
    (rr.ruleMap) should be (empty)
    (rr.error) should not be (null)
    (rr.error(1)) should not be (empty)     // error message present
  }

  // test runReach
  "open Ruler.runReach" should "not fail for empty text" in {
    val rr = Ruler.runReach(emptyText)
    (rr) should not be (null)
    (rr.error) should be (null)
    (rr.text) should equal (emptyText)
    (rr.rules) should not be (empty)
    (rr.eventAnnotations) should be (empty)
    (rr.syntaxAnnotations) should be (empty)
    (rr.syntaxTokens) should be (empty)
    (rr.syntaxTrees) should be (empty)
    (rr.ruleMap) should not be (empty)
  }

  it should "succeed for good non-bio text" in {
    val rr = Ruler.runReach(sally)
    (rr) should not be (null)
    (rr.error) should be (null)
    (rr.text) should equal (sally)
    (rr.rules) should not be (empty)
    (rr.eventAnnotations) should be (empty) // no bio events
    (rr.syntaxAnnotations) should not be (empty)
    (rr.syntaxTokens) should not be (empty)
    (rr.syntaxTrees) should not be (empty)
    (rr.ruleMap) should not be (empty)
  }

  it should "succeed for good bio text" in {
    val rr = Ruler.runReach(bioabc)
    (rr) should not be (null)
    (rr.error) should be (null)
    (rr.text) should equal (bioabc)
    (rr.rules) should not be (empty)
    (rr.eventAnnotations) should not be (empty)
    (rr.syntaxAnnotations) should not be (empty)
    (rr.syntaxTokens) should not be (empty)
    (rr.syntaxTrees) should not be (empty)
    (rr.ruleMap) should not be (empty)
  }

}
