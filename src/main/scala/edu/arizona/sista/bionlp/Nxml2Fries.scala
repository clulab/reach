package edu.arizona.sista.bionlp

import java.io.File
import scala.sys.process._
import scala.collection.JavaConverters._
import org.apache.commons.io.{ FileUtils, FilenameUtils }

class Nxml2Fries(
    val executable: String,
    val removeCitations: Boolean,
    val ignoreSections: Set[String],
    val encoding: String
) {
  def extractEntries(input: File): Seq[FriesEntry] = {
    val command =
      if (removeCitations) Seq(executable, "--no-citations", input.getCanonicalPath)
      else Seq(executable, input.getCanonicalPath)

    // execute command
    val status = command.!

    if (status == 0) {
      // get document name
      val name = FilenameUtils.removeExtension(input.getName)
      val tsvFile = new File(FilenameUtils.removeExtension(input.getCanonicalPath) + ".tsv")
      FileUtils.readLines(tsvFile, encoding).asScala.flatMap { line =>
        val fields = line.split('\t')
        val entry = FriesEntry(name, fields(0), fields(1), fields(2), fields(3).toInt == 1, fields(4))
        if (ignoreSections contains entry.sectionName) None else Some(entry)
      }
    } else sys.error("something went wrong when running nxml2fries")
  }
}

case class FriesEntry(
  name: String,
  chunkId: String,
  sectionId: String,
  sectionName: String,
  isTitle: Boolean,
  text: String)
