package org.clulab.reach

import org.clulab.processors.Document
import org.clulab.reach.ReachSentence.Converter
import org.clulab.serialization.DocumentSerializer

import java.io.{BufferedReader, ByteArrayInputStream, ByteArrayOutputStream, InputStreamReader, PrintWriter}

class ReachDocumentSerializer {
  val documentSerializer = new DocumentSerializer()

  // These first two are for compatibility with the processors version.
  protected def read(bufferedReader: BufferedReader, howManyTokens: Int = 0): Array[String] = {
    val line = bufferedReader.readLine()

    if (line.isEmpty) Array.empty[String]
    else line.split(ReachDocumentSerializer.SEP, howManyTokens)
  }

  def load(string: String, encoding: String = "UTF-8"): ReachDocument = {
    val byteArrayInputStream = new ByteArrayInputStream(string.getBytes(encoding))
    val bufferedReader = new BufferedReader(new InputStreamReader(byteArrayInputStream))
    val doc = load(bufferedReader)

    bufferedReader.close()
    doc
  }

  def load(bufferedReader: BufferedReader): ReachDocument = loadDocument(bufferedReader)

  def loadDocument(bufferedReader: BufferedReader): ReachDocument = {
    val document = documentSerializer.load(bufferedReader)
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

  def save(doc: Document, encoding: String = "UTF-8", keepText: Boolean = false): String = {
    val byteArrayOutputStream = new ByteArrayOutputStream
    val printWriter = new PrintWriter(byteArrayOutputStream)

    save(doc, printWriter, keepText)
    printWriter.flush()
    printWriter.close()
    byteArrayOutputStream.toString(encoding)
  }

  def save(doc: Document, printWriter: PrintWriter): Unit = save(doc, printWriter, keepText = false)

  def save(doc: Document, printWriter: PrintWriter, keepText: Boolean): Unit = saveDocument(doc, printWriter, keepText)

  def saveDocument(doc: Document, printWriter: PrintWriter, keepText: Boolean): Unit = {
    documentSerializer.save(doc, printWriter, keepText)
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
