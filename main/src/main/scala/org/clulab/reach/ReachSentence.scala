package org.clulab.reach

import org.clulab.processors.{Document, Sentence}

// It would be really nice to have a copy constructor for Sentence!
class ReachSentence(sentence: Sentence, val sections: Option[Array[String]] = None) extends Sentence(
  sentence.raw,
  sentence.startOffsets,
  sentence.endOffsets,
  sentence.words
) {
  this.tags = sentence.tags
  this.lemmas = sentence.lemmas
  this.entities = sentence.entities
  this.norms = sentence.norms
  this.chunks = sentence.chunks
  this.syntacticTree = sentence.syntacticTree
  this.graphs = sentence.graphs
  this.relations = sentence.relations
}

object ReachSentence {

  implicit class Converter(sentence: Sentence) {
    
    def sections: Option[Array[String]] = sentence match {
      case sentence: ReachSentence => sentence.sections
      case _ => None
    }
  }
}
