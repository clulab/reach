package org.clulab.reach

import org.clulab.processors.{Document, Sentence}

class ReachDocument(doc: Document, val reachSentences: Array[ReachSentence])
    extends Document(reachSentences.asInstanceOf[Array[Sentence]]) {
  this.id = doc.id
  this.coreferenceChains = doc.coreferenceChains
  this.text = doc.text

  doc.getAttachmentKeys.foreach { key =>
    this.addAttachment(key, doc.getAttachment(key).get)
  }
  doc.getDCT.foreach(this.setDCT)

  def serialize(): Unit = ???
}

object ReachDocument {

  def apply(doc: Document): ReachDocument = {
    val reachSentences = doc.sentences.map(new ReachSentence(_))

    new ReachDocument(doc, reachSentences)
  }
}
