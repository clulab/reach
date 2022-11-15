package org.clulab.reach

import org.clulab.processors.Document
import org.clulab.serialization.DocumentSerializer

import java.io.{BufferedReader, PrintWriter, StringWriter}

class ReachDocumentSerializer {
  val documentSerializer = new DocumentSerializer()

  def load(bufferedReader: BufferedReader): ReachDocument = {
    documentSerializer.load(bufferedReader)
    // Read the rest
    ???
  }

  def save(document: Document, printWriter: PrintWriter): Unit = {
    documentSerializer.save(document, printWriter)
    // Write the rest
    ???
  }
}

object ReachDocumentSerializer {
  val END_OF_SENTENCE = "EOS"
  val START_SECTION = "Z"
  val END_OF_REACH = "EOR"
}
