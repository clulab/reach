package edu.arizona.sista.bionlp.reach.ruler

import scala.util.control.Breaks._
import edu.arizona.sista.matcher.{ExtractorEngine, Mention, TextBoundMention, EventMention}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

object RuleShell extends App {
  require(args.size % 2 == 0, "wrong command line args")

  val prompt = ">>> "

  val entityRules = Ruler.readEntityRules

  val ruleArgIndex = args.indexOf("--rules")
  val eventRules = if (ruleArgIndex == -1) Ruler.readEventRules else Ruler.readFile(args(ruleArgIndex + 1))

  val rules = entityRules + "\n\n" + eventRules

  val actions = new DarpaActions

  val proc = new BioNLPProcessor
  val extractor = new ExtractorEngine(rules, actions)

  breakable {
    while (true) {
      val text = readLine(prompt)
      if (text == null) break
      val doc = proc.annotate(text)
      val mentions = extractor.extractFrom(doc)
      displayMentions(mentions, doc)
    }
  }

  println("\nbye")

  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.words.mkString(" "))
      println
      mentionsBySentence(i) foreach displayMention
      println("=" * 50)
    }
  }

  def displayMention(mention: Mention) {
    println(s"rule: ${mention.foundBy}")
    mention match {
      case m: TextBoundMention =>
        println(s"${m.label} (TextBoundMention)")
        println(m.text)
        println
      case m: EventMention =>
        println(s"${m.label} (EventMention)")
        println(s"trigger = ${m.trigger.text}")
        m.arguments foreach {
          case (k, vs) => for (v <- vs) println(s"$k = ${v.text}")
        }
        println
      case _ => ()
    }
  }
}
