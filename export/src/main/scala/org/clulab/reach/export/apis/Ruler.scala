package org.clulab.reach.export.apis

import org.clulab.odin.impl.{OdinCompileException, OdinNamedCompileException}
import org.clulab.processors.Document
import org.clulab.reach._
import org.clulab.reach.brat.Brat
import org.clulab.reach.export.apis.open.OpenSystem
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

/**
  * Singleton to run the Open Domain system with the given text and rules.
  *   Last Modified: Redo to use processor core client.
  */
object Ruler {

  val reachRules = RuleReader.mkRules()         // read rules for ReachSystem from resource
  val reach = new ReachSystem(Some(reachRules)) // pass in reachRules
  val od = new OpenSystem()                     // setup open domain system

  /** Run the Open Domain system with the given text and rules. */
  def runOpen(text: String, rules: String): RulerResults = {
    val doc = od.mkDoc(text)

    // Were any rules submitted?
    if (rules.trim.isEmpty)
      return new RulerResults(text, rules, null, null, tokens(doc), synTrees(doc), null, Array(null, "rulesStr is empty"))

    // For displaying rules (ruleName -> rule)
    val ruleMap = Try(mkRuleMap(rules)).getOrElse(null).asJava
    val result = od.extractFrom(rules, doc)

    // Reset cachedRules on failure
    if (result.isFailure) od.cachedRules = ""

    result match {
      // either the engine succeeded
      case Success(mentions) =>
        val eventAnnotations = Brat.dumpStandoff(mentions, doc)
        val syntaxAnnotations = Brat.syntaxStandoff(doc)
        new RulerResults(text, rules, eventAnnotations, syntaxAnnotations, tokens(doc),
          synTrees(doc), ruleMap)

      // there may have been a problem compiling the rules
      case Failure(OdinNamedCompileException(e, name)) =>
        // No standoff in this case...
        new RulerResults(text, rules, null, null, tokens(doc), synTrees(doc), ruleMap,
          Array(name, e))

      // An error without a name
      case Failure(OdinCompileException(other)) =>
        new RulerResults(text, rules, null, null, tokens(doc), synTrees(doc), ruleMap,
          Array(null, other))

      // Catch-all for anything else
      case Failure(e) =>
        new RulerResults(text, rules, null, null, tokens(doc), synTrees(doc), ruleMap,
          Array(null, e.getMessage))
    }
  }


  /** Run the bio nlp system with the given text and REACH rules. */
  def runReach(text: String): RulerResults = {
    val doc = reach.mkDoc(text, "visualizer")
    val mentions = reach.extractFrom(doc)
      // Remove ModificationTriggers from output
      .filterNot(_ matches "ModificationTrigger")
    val rules = reach.allRules
    val eventAnnotations = Brat.dumpStandoff(mentions, doc)
    val syntaxAnnotations = Brat.syntaxStandoff(doc)
    val ruleMap = Try(mkRuleMap(rules)).getOrElse(null).asJava
    new RulerResults(text, rules, eventAnnotations, syntaxAnnotations,
                     tokens(doc), synTrees(doc), ruleMap)
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
    allTokens
  }

  private def synTrees(doc: Document): Array[String] = {
    val allTrees = doc.sentences map { s =>
      s.syntacticTree.map(_.toString).getOrElse("()")
    }
    allTrees
  }

  /** removes commented lines */
  private def clean(input: String): String = input.replaceAll("""(?m)^\s*#.*$""", "").trim()

  /** Create a Map from rule name -> rule. **/
  private def mkRuleMap(rules: String): Map[String, String] = {

    // to find the rule name (even if it is quoted)
    val namePattern = """^- name:\s+("[^\\"]*(?:\\.[^\\"]*)*"|[^\s#]+)""".r

    val ruleMap: Map[String, String] =
      clean(rules)
        .split("(?=- name:)")
        .map(_.trim)
        .filter(_.nonEmpty)                 // remove empty chunks from the split
        .flatMap { rule =>                  // find the rule name
          namePattern.findFirstMatchIn(rule).map { m =>
            val name = m.group(1)
            val key =
              // if the string is quoted, remove the quotes
              if (name.startsWith("\"") && name.endsWith("\"")) name.drop(1).dropRight(1)
              else name // it isn't quoted, just return it
            (key, rule)
          }
        }.toMap

    ruleMap
  }
}


class RulerResults(val text: String,
                   val rules: String,
                   val eventAnnotations: String,
                   val syntaxAnnotations: String,
                   val syntaxTokens: Array[Token],
                   val syntaxTrees: Array[String],
                   val ruleMap: java.util.Map[String, String],
                   // Error is always size of 2 (name, message) whenever present
                   val error: Array[String] = null)


class Token(val word: String,
            val lemma: String,
            val tag: String,
            val entity: String,
            val start: Int,
            val end: Int)
