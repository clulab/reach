package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp._
import edu.arizona.sista.bionlp.reach.brat.Brat
import edu.arizona.sista.open.OpenSystem
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import scala.util.{Success, Failure}

object Ruler {
  val reach = new ReachSystem
  val odProc = new CoreNLPProcessor(withDiscourse = false)
  val od = new OpenSystem(Some(odProc))

  // For parsing the error (name) and (error)
  val ruleErrorPattern = """Error parsing rule '(.+)': (.+)""".r

  def runOpen(text: String): RulerResults = runOpen(text, "")

  def runOpen(text: String, rulesStr: String): RulerResults = {
    val doc = od.mkDoc(text)

    val rules = rulesStr // TODO: IMPLEMENT RULE SUBMISSION LATER
    val ruleMap = mkRuleMap(rules)

    val result = od.extractFrom(rulesStr, doc)
    result match {

      // The engine succeeded
      case Success(mentions) =>
        val eventAnnotations = Brat.dumpStandoff(mentions, doc)
        val syntaxAnnotations = Brat.syntaxStandoff(doc)
        new RulerResults(text, rules, eventAnnotations, syntaxAnnotations, tokens(doc), synTrees(doc), ruleMap)
      // There must have been a problem compiling the rules

      case Failure(e) =>
        val (ruleName, syntaxError) = {
          val m = ruleErrorPattern.findFirstMatchIn(e.getMessage).get
          // strip any quotes from the rule name
          (m.group(1).replaceAll( """["']""", ""), m.group(2))
        }
        // No standoff in this case...
        new RulerResults(text, rules, "", "", tokens(doc), synTrees(doc), ruleMap, Option(Array(ruleName, syntaxError)))

    }
  }

  def runReach(text: String): RulerResults = {
    val doc = reach.mkDoc(text, "visualizer")
    val mentions = reach.extractFrom(doc)
    val rules = reach.allRules
    val eventAnnotations = Brat.dumpStandoff(mentions, doc)
    val syntaxAnnotations = Brat.syntaxStandoff(doc)
    val ruleMap = mkRuleMap(rules)
    new RulerResults(text, rules, eventAnnotations, syntaxAnnotations, tokens(doc), synTrees(doc), ruleMap)
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

  /** Create a Map from rule name -> rule **/
  def mkRuleMap(rules: String): Map[String, String] = {
    // to find the rule name
    val namePattern = """^- name: (.+)""".r

    val ruleMap: Map[String, String] =
      rules
        .split("(?=- name:)")
        .map(_.trim)
        // remove empty chunks from the split
        .filter(_.nonEmpty)
        // find the rule name
        .groupBy { rule =>
        // remove any surrounding quotes from rule name
        val ruleName =
          namePattern.findFirstMatchIn(rule)
            .get.group(1)
            .replaceAll( """["']""", "")
        ruleName
      }.mapValues(_.head)

    ruleMap
  }
}

class RulerResults(val text: String,
                   val rules: String,
                   val eventAnnotations: String,
                   val syntaxAnnotations: String,
                   val syntaxTokens: Array[Token],
                   val syntaxTrees: Array[String],
                   val ruleMap: Map[String, String],
                   // Always size of 2 (name, message) whenever present
                   val error: Option[Array[String]] = None)

class Token(val word: String,
            val lemma: String,
            val tag: String,
            val entity: String,
            val start: Int,
            val end: Int)

