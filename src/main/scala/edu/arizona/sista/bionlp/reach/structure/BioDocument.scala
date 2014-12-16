package edu.arizona.sista.bionlp.reach.structure

import edu.arizona.sista.discourse.rstparser.DiscourseTree
import edu.arizona.sista.processors.{Document, CorefChains, Sentence}

/**
 *
 * User: mihais
 * Date: 9/16/14
 */
class BioDocument( /** Ideally this is the pubmed id, or some other unique id */
                   val paperId:String,
                   /** Indicates which section this is, e.g., abstract or body */
                   val section:String,
                   sentences:Array[Sentence],
                   coref:Option[CorefChains],
                   dtree:Option[DiscourseTree]) extends Document(sentences, coref, dtree) with Serializable {

  def this(paperId:String, section:String, sentences:Array[Sentence]) =
    this(paperId, section, sentences, None, None)

  override def toString:String = {
    val b = new StringBuilder
    b.append(s"Document $paperId/$section\n")
    for(i <- 0 until sentences.size) {
      b.append(s"Sentence #$i:\n")
      b.append(s"${toString(sentences(i))}\n")
    }
    b.toString()
  }

  private def toString(s:Sentence):String = {
    val b = new StringBuilder
    b.append(s"Words: ${s.words.mkString(" ")}\n")
    s.lemmas.foreach(i => b.append(s"Lemmas: ${i.mkString(" ")}\n"))
    s.tags.foreach(i => b.append(s"Tags: ${s.words.zip(i).mkString(" ")}\n"))
    s.chunks.foreach(i => b.append(s"Chunks: ${s.words.zip(i).mkString(" ")}\n"))
    s.entities.foreach(i => b.append(s"Entities: ${s.words.zip(i).mkString(" ")}\n"))
    s.syntacticTree.foreach(i => b.append(s"Syntactic tree:\n$i\n"))
    s.dependencies.foreach(i => b.append(s"Dependencies with CC processed:\n$i\n"))
    b.toString()
  }
}
