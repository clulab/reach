package org.clulab.reach.mentions.serialization.json

import org.clulab.processors.Document

import java.util

object EquivalenceHashes {
  protected val equivalenceHashes: util.IdentityHashMap[Document, Int] = new util.IdentityHashMap[Document, Int]()

  def get(document: Document): Int = synchronized {
    if (equivalenceHashes.containsKey(document))
      equivalenceHashes.get(document)
    else {
      val equivalencyHash = document.equivalenceHash

      equivalenceHashes.put(document, equivalencyHash)
      equivalencyHash
    }
  }

  def remove(document: Document): Option[Int] = synchronized {
    Option(equivalenceHashes.remove(document))
  }
}
