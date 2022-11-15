package org.clulab.reach

import org.clulab.processors.Sentence

// It would be really nice to have a copy constructor for Sentence!
class ReachSentence(sentence: Sentence, var sections: Option[Array[String]] = None) extends Sentence(
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
