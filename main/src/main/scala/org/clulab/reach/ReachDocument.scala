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
}

object ReachDocument {

  def apply(doc: Document): ReachDocument = {
    val arrayOfSectionsOpt = doc.sentences.map(_ => None: Option[Array[String]])

    apply(doc, arrayOfSectionsOpt)
  }
  def apply(doc: Document, arrayOfSectionsOpt: Array[Option[Array[String]]]): ReachDocument = {
    val reachSentences = doc.sentences.zip(arrayOfSectionsOpt).map { case (sentence, sectionsOpt) =>
      new ReachSentence(sentence, sectionsOpt)
    }

    new ReachDocument(doc, reachSentences)
  }
}
