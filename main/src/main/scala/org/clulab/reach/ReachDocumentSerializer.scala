package org.clulab.reach

import org.clulab.processors.Document
import org.clulab.reach.ReachSentence.Converter
import org.clulab.serialization.DocumentSerializer

import java.io.{BufferedReader, PrintWriter}

class ReachDocumentSerializer extends DocumentSerializer {

  // This is private in the superclass, unfortunately.
  protected def read(bufferedReader: BufferedReader, howManyTokens: Int = 0): Array[String] = {
    val line = bufferedReader.readLine()

    if (line.isEmpty) Array.empty[String]
    else line.split(ReachDocumentSerializer.SEP, howManyTokens)
  }

  override def load(bufferedReader: BufferedReader): ReachDocument = {
    val document = super.load(bufferedReader)
    val arrayOfSectionsOpt = loadSections(bufferedReader, document.sentences.length)

    val bits = read(bufferedReader)
    assert(bits(0) == ReachDocumentSerializer.END_OF_REACH, s"END_OF_DOCUMENT expected, found ${bits(0)}")

    ReachDocument(document, arrayOfSectionsOpt)
  }

  def loadSections(bufferedReader: BufferedReader, sentenceCount: Int): Array[Option[Array[String]]] = {
    val sections = Range(0, sentenceCount).map { _ =>
      val numSubSections = {
        val bits = read(bufferedReader)
        assert(bits(0) == ReachDocumentSerializer.START_SECTION)
        bits(1).toInt
      }
      val subSectionsOpt = {
        val bits = read(bufferedReader)

        if (numSubSections == 0) None
        else Some(bits.take(numSubSections))
      }

      subSectionsOpt
    }

    sections.toArray
  }

  override def save(doc: Document, printWriter: PrintWriter, keepText: Boolean): Unit = {
    super.save(doc, printWriter, keepText)
    doc.sentences.foreach { sentence =>
      val sections = sentence.sections.getOrElse(Array.empty[String])

      printWriter.println(ReachDocumentSerializer.START_SECTION + ReachDocumentSerializer.SEP + sections.length)
      saveSections(sections, printWriter)
    }
    printWriter.println(ReachDocumentSerializer.END_OF_REACH)
  }

  def saveSections(sections: Array[String], printWriter: PrintWriter): Unit = {
    sections.foreach { section =>
      // The sections names from the root to the most concrete
      printWriter.print(section + ReachDocumentSerializer.SEP)
    }
    printWriter.println()
  }
}

object ReachDocumentSerializer {
  val SEP = "\t"
  val START_SECTION = "Z"
  val END_OF_REACH = "EOR"
}
