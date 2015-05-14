package edu.arizona.sista.bionlp

import scala.collection.mutable.MutableList
import edu.arizona.sista.odin._
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.processors.{ Document, Sentence }

package object display {

  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText())
      printSyntacticDependencies(s)
      println

      val sortedMentions = mentionsBySentence(i).sortBy(_.label)
      println("entities:")
      sortedMentions foreach (m => if(m.isInstanceOf[TextBoundMention]) displayMention(m))

      println
      println("events:")
      sortedMentions foreach (m => if(! m.isInstanceOf[TextBoundMention]) displayMention(m))
      println("=" * 50)
    }
  }

  def printSyntacticDependencies(s:Sentence): Unit = {
    if(s.dependencies.isDefined) {
      println(s.dependencies.get.toString)
    }
  }

  def displayMention(mention: Mention) {
    val boundary = s"\t${"-" * 30}"
    println(mention.labels)
    println(boundary)
    println(s"\tRule => ${mention.foundBy}")
    val mentionType = mention.getClass.toString.split("""\.""").last
    println(s"\tType => $mentionType")
    println(boundary)
    mention match {
      case m: TextBoundMention =>
        println(s"\t${m.asInstanceOf[Display].displayLabel}|${m.labels} => ${m.text}")
        val bm = m.toBioMention
        if (bm.isGrounded)
          println(s"\txref: ${bm.xref.get}")
      case m: EventMention =>
        println(s"\ttrigger => ${m.trigger.text}")
        m.arguments foreach {
          case (k, vs) => for (v <- vs) println(s"\t$k (${v.labels}) => ${v.text}")
        }
      case m: RelationMention =>
        m.arguments foreach {
          case (k, vs) => for (v <- vs) println(s"\t$k (${v.labels}) => ${v.text}")
        }
      case _ => ()
    }
    println(s"$boundary\n")
  }

  /** Generates a representation of the given mention as a list of strings. */
   def mentionToStrings (mention:Mention): List[String] = {
     return mentionToStrings(mention, 0)
   }

  /** Return a list of strings representing the given mention at the given indentation level. */
  private def mentionToStrings (mention:Mention, level:Integer): List[String] = {
    val mStrings:MutableList[String] = MutableList[String]()
    val indent = ("  " * level)
    mention match {
      case mention: TextBoundMention =>
        mStrings += s"${indent}TextBoundMention: [S${mention.sentence}]: ${mention.label}"
        mStrings += s"${indent}text: ${mention.text}"
        //if (mention.isGrounded)
        //  mStrings += s"${indent}xref: ${mention.xref.get}"
        if (level == 0) mStrings += ("=" * 80)
      case mention: EventMention =>
        mStrings += s"${indent}EventMention: [S${mention.sentence}]: ${mention.label}"
        mStrings += s"${indent}text: ${mention.text}"
        mStrings += s"${indent}trigger:"
        mStrings ++= mentionToStrings(mention.trigger, level+1)
        mention.arguments foreach {
          case (k,vs) => {
            mStrings += s"${indent}${k} (${vs.length}):"
            for (v <- vs) {
              mStrings ++= mentionToStrings(v, level+1)
            }
          }
        }
        if (level == 0) mStrings += ("=" * 80)
      case mention: RelationMention =>
        mStrings += s"${indent}RelationMention: [S${mention.sentence}]: ${mention.label}"
        mStrings += s"${indent}text: ${mention.text}"
        mention.arguments foreach {
          case (k,vs) => {
            mStrings += s"${indent}${k} (${vs.length}):"
            for (v <- vs) {
              mStrings ++= mentionToStrings(v, level+1)
            }
          }
        }
        if (level == 0) mStrings += ("=" * 80)
      case _ => ()
    }
    return mStrings.toList
  }

}
