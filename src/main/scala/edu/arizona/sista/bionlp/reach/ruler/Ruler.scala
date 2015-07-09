package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp._
import edu.arizona.sista.bionlp.reach.brat.Brat
import edu.arizona.sista.open.OpenSystem
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import scala.util.{Try, Success, Failure}

object Ruler {

  // read rules for ReachSystem from resource
  val reachRules = RuleReader.mkRules()
  // pass in reachRules
  val reach = new ReachSystem(Some(reachRules))
  // setup open domain system
  val odProc = new CoreNLPProcessor(withDiscourse = false)
  val od = new OpenSystem(Some(odProc))

  // For parsing the error (name) and (error)
  val ruleErrorPattern = """Error parsing rule '(.+)': (.+)""".r

  /** Run the Open Domain system with the given text and rules. */
  def runOpen(text: String, rules: String): RulerResults = {
    val doc = od.mkDoc(text)

    // Were any rules submitted?
    if (rules.trim.isEmpty)
      return new RulerResults(text, rules, null, null, tokens(doc), synTrees(doc), Map.empty, Array("", "rulesStr is empty"))

    // For displaying rules (ruleName -> rule)
    val ruleMap = Try(mkRuleMap(rules)) getOrElse Map.empty
    val result = od.extractFrom(rules, doc)

    result match {
      // either the engine succeeded
      case Success(mentions) =>
        val eventAnnotations = Brat.dumpStandoff(mentions, doc)
        val syntaxAnnotations = Brat.syntaxStandoff(doc)
        new RulerResults(text, rules, eventAnnotations, syntaxAnnotations, tokens(doc),
          synTrees(doc), ruleMap)

      // OR there must have been a problem compiling the rules
      case Failure(e) if e.getMessage.startsWith("Error parsing rule '") =>
        // In the case of a rule compilation error, we will return an Array of size 2
        // Where the first element is the rule name and the second is the error message
        val (ruleName, syntaxError) = {
          val m = ruleErrorPattern.findFirstMatchIn(e.getMessage).get
          // strip any quotes from the rule name
          (m.group(1).replaceAll( """["']""", ""), m.group(2))
        }
        // No standoff in this case...
        new RulerResults(text, rules, null, null, tokens(doc), synTrees(doc), ruleMap,
          Array(ruleName, syntaxError))
    }
  }


  /** Run the bio nlp system with the given text and REACH rules. */
  def runReach(text: String): RulerResults = {
    val doc = reach.mkDoc(text, "visualizer")
    val mentions = reach.extractFrom(doc)
    val rules = reach.allRules
    val eventAnnotations = Brat.dumpStandoff(mentions, doc)
    val syntaxAnnotations = Brat.syntaxStandoff(doc)
    new RulerResults(text, rules, eventAnnotations, syntaxAnnotations,
                     tokens(doc), synTrees(doc), mkRuleMap(rules))
  }


  private def tokens(doc: Document): Array[Token] = {
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

  private def synTrees(doc: Document): Array[String] = {
    val allTrees = doc.sentences map { s =>
      s.syntacticTree.map(_.toString).getOrElse("()")
    }
    allTrees.toArray
  }

  /** removes commented lines */
  def clean(input: String): String = input.replaceAll("""(?m)^\s*#.*$""", "").trim()

  /** Create a Map from rule name -> rule. **/
  private def mkRuleMap(rules: String): Map[String, String] = {
    val namePattern = """^- name: (.+)""".r  // to find the rule name

    val ruleMap: Map[String, String] =
      clean(rules)
        .split("(?=- name:)")
        .map(_.trim)
        .filter(_.nonEmpty)                 // remove empty chunks from the split
        .groupBy { rule =>                  // find the rule name
          // remove any surrounding quotes from rule name
          namePattern.findFirstMatchIn(rule)
          .get.group(1)
          .replaceAll( """["']""", "")
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
                   // Error is always size of 2 (name, message) whenever present
                   val error: Array[String] = null)


class Token(val word: String,
            val lemma: String,
            val tag: String,
            val entity: String,
            val start: Int,
            val end: Int)
