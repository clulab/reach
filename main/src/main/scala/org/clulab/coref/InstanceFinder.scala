package org.clulab.coref

import scala.collection.mutable.ArrayBuffer
import org.clulab.struct.Interval
import org.clulab.struct.HashTrie
import org.clulab.odin.TextBoundMention
import org.clulab.processors.Document

class InstanceFinder(val multiLabel: Seq[String], val foundBy: String = "InstanceFinder") {

  import InstanceFinder.readIOBLabels

  val trie = new HashTrie

  val outsideLabel = "O"

  def add(words: Seq[String]): Unit = trie.add(words.toArray)

  def findAllIn(doc: Document): Seq[TextBoundMention] = {
    for {
      i <- doc.sentences.indices
      words = doc.sentences(i).words
      labels = trie.find(words, multiLabel.head, outsideLabel)
      mention <- mkMentions(doc, i, labels)
    } yield mention
  }

  def mkMentions(doc: Document, sent: Int, labels: Array[String]): Seq[TextBoundMention] = {
    readIOBLabels(labels) map { case (lbl, start, end) =>
      new TextBoundMention(multiLabel, Interval(start, end), sent, doc, true, foundBy)
    }
  }

}

object InstanceFinder {

  // reads IOB style labels
  def readIOBLabels(labels: Seq[String]): Seq[(String, Int, Int)] = {
    val mentions = new ArrayBuffer[(String, Int, Int)]

    var name: Option[String] = None
    var start: Option[Int] = None
    var end: Option[Int] = None

    for ((label, idx) <- labels.zipWithIndex if label != "O") {
      if (label startsWith "B-") {
        if (name.isDefined) {
          // a mention just ended
          val m = (name.get, start.get, end.get + 1)
          mentions += m
        }
        // start new mention
        name = Some(label.drop(2))
        start = Some(idx)
        end = Some(idx)
      } else if (label startsWith "I-") {
        // the mention keeps growing
        end = Some(idx)
      } else {
        sys.error("tag not in IOB format")
      }
    }

    // include last mention
    if (name.isDefined) {
      val m = (name.get, start.get, end.get + 1)
      mentions += m
    }

    mentions
  }

}
