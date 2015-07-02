package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp._
import edu.arizona.sista.bionlp.reach.brat.Brat
import edu.arizona.sista.processors.Document

object Ruler {
  val reach = new ReachSystem

  def runOpen(text: String): RulerResults = runOpen(text, "")

  def runOpen(text: String, rulesStr: String): RulerResults = {
    val doc = reach.mkDoc(text, "visualizer")
    val mentions = reach.extractFrom(doc)
    val rules = reach.allRules              // TODO: IMPLEMENT RULE SUBMISSION LATER
    val eventAnnotations = Brat.dumpStandoff(mentions, doc)
    val syntaxAnnotations = Brat.syntaxStandoff(doc)
    new RulerResults(text, rules, eventAnnotations, syntaxAnnotations, tokens(doc), synTrees(doc))
  }


  def runReach(text: String): RulerResults = {
    val doc = reach.mkDoc(text, "visualizer")
    val mentions = reach.extractFrom(doc)
    val rules = reach.allRules
    val eventAnnotations = Brat.dumpStandoff(mentions, doc)
    val syntaxAnnotations = Brat.syntaxStandoff(doc)
    new RulerResults(text, rules, eventAnnotations, syntaxAnnotations, tokens(doc), synTrees(doc))
  }


  def tokens(doc: Document): Array[Token] = {
    val allTokens = doc.sentences flatMap { s =>
      0 until s.size map { i =>
        new Token(s.words(i),
                  s.lemmas.get(i),
                  s.tags.get(i),
                  s.entities.get(i),
                  s.startOffsets(i),
                  s.endOffsets(i))
      }
    }
    allTokens.toArray
  }


  def synTrees(doc: Document): Array[String] = {
    val allTrees = doc.sentences map { s =>
      s.syntacticTree.map(_.toString).getOrElse("()")
    }
    allTrees.toArray
  }
}


class RulerResults(val text: String,
                   val rules: String,
                   val eventAnnotations: String,
                   val syntaxAnnotations: String,
                   val syntaxTokens: Array[Token],
                   val syntaxTrees: Array[String])

class Token(val word: String,
            val lemma: String,
            val tag: String,
            val entity: String,
            val start: Int,
            val end: Int)
