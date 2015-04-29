package edu.arizona.sista.bionlp

import java.io.File
import scala.sys.process._
import scala.collection.JavaConverters._
import org.apache.commons.io.{ FileUtils, FilenameUtils }

class Nxml2Fries(val executable: String, val removeCitations: Boolean, val encoding: String) {
  import Nxml2Fries._

  def extractEntries(input: File): Seq[Entry] = {
    val command =
      if (removeCitations) Seq(executable, "--no-citations", input.getCanonicalPath)
      else Seq(executable, input.getCanonicalPath)

    // execute command
    val status = command.!

    if (status == 0) {
      // get document name
      val name = FilenameUtils.removeExtension(input.getName)
      val tsvFile = new File(FilenameUtils.removeExtension(input.getCanonicalPath) + ".tsv")
      for (line <- FileUtils.readLines(tsvFile, encoding).asScala) yield {
        val fields = line.split('\t')
        Entry(name, fields(0), fields(1), fields(2), fields(3).toInt == 1, fields(4))
      }
    } else sys.error("something went wrong when running nxml2fries")
  }
}

object Nxml2Fries {
  case class Entry(
    name: String,
    chunkId: String,
    sectionId: String,
    sectionName: String,
    isTitle: Boolean,
    text: String)
}
