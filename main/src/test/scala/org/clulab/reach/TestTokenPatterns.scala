package org.clulab.reach

import org.scalatest._
import org.clulab.struct.Interval
import org.clulab.odin.impl.TokenPattern
import org.clulab.odin._
import TestUtils._

class TestTokenPatterns extends FlatSpec with Matchers {
  // use shared processor instance from TestUtils
  val proc = bioproc
  val text1 = "TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3."
  val text2 = "TGFBR2 phosphorylates peri-kappa B and peri-kappa C and inhibits the ubiquitination of SMAD3."

  text1 should "contain one match" in {
    val doc = proc annotate text1
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(new TextBoundMention("Phosphorylation", Interval(0, 4), 0, doc, true, "<MANUAL>"))
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    assert(results.size == 1)
  }

  text2 should "contain one match" in {
    val doc = proc annotate text2
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(new TextBoundMention("Phosphorylation", Interval(0, 4), 0, doc, true, "<MANUAL>"),
                       new TextBoundMention("Phosphorylation", Interval(0, 7), 0, doc, true, "<MANUAL>"))
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    assert(results.size == 1)
  }

  val text4 = "a b c d e f g h i c"
  val doc = proc annotate text4

  text4 should "match with a lazy plus" in {
    val p = TokenPattern.compile("a /./+? c")
    val results = p.findAllIn(0, doc)
    assert(results.size == 1)
    assert(results.head.interval.start == 0)
    assert(results.head.interval.end == 3)
  }

  it should "match with a greedy plus" in {
    val p = TokenPattern.compile("a /./+ c")
    val results = p.findAllIn(0, doc)
    assert(results.size == 1)
    assert(results.head.interval.start == 0)
    assert(results.head.interval.end == 10)
  }

  it should "match with a lazy star" in {
    val p = TokenPattern.compile("a /./*? c")
    val results = p.findAllIn(0, doc)
    assert(results.size == 1)
    assert(results.head.interval.start == 0)
    assert(results.head.interval.end == 3)
  }

  it should "match with a greedy star" in {
    val p = TokenPattern.compile("a /./* c")
    val results = p.findAllIn(0, doc)
    assert(results.size == 1)
    assert(results.head.interval.start == 0)
    assert(results.head.interval.end == 10)
  }
}
