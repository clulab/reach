package org.clulab.reach.mentions.serialization.json

import org.clulab.processors.Document

import java.util

object EquivalencyHashes {
  protected val equivalencyHashes: util.IdentityHashMap[Document, Int] = new util.IdentityHashMap[Document, Int]()

  def get(document: Document): Int = {
    if (equivalencyHashes.containsKey(document))
      equivalencyHashes.get(document)
    else {
      val equivalencyHash = document.equivalenceHash

      equivalencyHashes.put(document, equivalencyHash)
      equivalencyHash
    }
  }

  def remove(document: Document): Option[Int] = {
    Option(equivalencyHashes.remove(document))
  }

  def clear(): Unit = {
    equivalencyHashes.clear()
  }
}
