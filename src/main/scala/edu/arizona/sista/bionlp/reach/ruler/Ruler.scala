package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp.reach.brat.Brat
import edu.arizona.sista.matcher.ExtractorEngine
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

object Ruler {
  val proc = new BioNLPProcessor

  def doItAll(text: String): RulerResults = doItAll(text, "")

  def doItAll(text: String, rulesStr: String): RulerResults = {
    val actions = new DarpaActions

    // read default rules if needed
    val rules = if (rulesStr.trim.isEmpty) BasicRuler.readRules else rulesStr

    val doc = proc.annotate(text)
    val basicRuler = new BasicRuler(rules, actions)
    val mentions = basicRuler.extractFrom(doc)

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
      s.syntacticTree.getOrElse("()").toString()
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
