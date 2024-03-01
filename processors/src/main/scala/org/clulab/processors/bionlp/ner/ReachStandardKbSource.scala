package org.clulab.processors.bionlp.ner

import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.processors.bionlp.ner.KBGenerator.containsValidSpecies
import org.clulab.processors.bionlp.ner.KBGenerator.logger
import org.clulab.processors.bionlp.ner.KBGenerator.tokenizeResourceLine
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.sequences.StandardKbSource
import org.clulab.utils.Files
import org.clulab.utils.Serializer

import java.io.File
import java.util.function.Consumer
import scala.language.reflectiveCalls
import scala.util.{Try, Using}

abstract class ReachStandardKbSource(caseInsensitiveMatching: Boolean) extends StandardKbSource(caseInsensitiveMatching)

class ReachSingleStandardKbSource(kbEntry: KBEntry, caseInsensitiveMatching: Boolean) extends ReachStandardKbSource(caseInsensitiveMatching) {

  override def getLabel: String = kbEntry.neLabel

  override def getCaseInsensitiveMatching: Boolean = caseInsensitiveMatching

  override def withTokens(f: Array[String] => Unit): Unit = {
    logger.info(s"Loading ${kbEntry.kbName}...")
    val tokenizer: Tokenizer = new BioNLPProcessor().tokenizer
    val consumer = new Consumer[String]() {
      var lineCount = 0
      def accept(line: String): Unit = {
        val trimmedLine = line.trim
        if (trimmedLine.nonEmpty && !trimmedLine.startsWith("#")) { // skip comments
          val kbTokens = trimmedLine.split("\t")
          // This condition is also distinctive
          if (containsValidSpecies(kbEntry, kbTokens)) { // this is a protein from a species we want
            lineCount += 1
            val ne = kbTokens(0) // we enforce that the first token is the actual NE to be considered
            val tokens = tokenizeResourceLine(ne, tokenizer) // tokenize using BioNLPProcessor

            f(tokens)
          }
        }
      }
    }
    val inputPath = kbEntry.path //inputDir + File.separator + entry.kbName + ".tsv"
    // This is different from the processors version in that a local file may override the resource.
    // If the original inputPath happens to end with .gz, then behavior has changed slightly.
    val bufferedReader =
      Try(Files.loadFile(inputPath)).getOrElse(
        Try(Files.loadFile(inputPath + ".gz")).getOrElse(
          Try(Files.loadStreamFromClasspath(inputPath)).getOrElse(
            Files.loadStreamFromClasspath(inputPath + ".gz")
          )
        )
      )

    Using.resource(bufferedReader) { bufferedReader =>
      bufferedReader.lines.forEach(consumer)
    }
    logger.info(s"Done. Read ${consumer.lineCount} lines from ${new File(kbEntry.path).getName}")
  }
}

class ReachMultiStandardKbSource(kbEntries: Seq[KBEntry], caseInsensitiveMatching: Boolean) extends ReachStandardKbSource(caseInsensitiveMatching) {
  protected val label: String = {
    require(kbEntries.nonEmpty)
    val label = kbEntries.head.neLabel
    require(kbEntries.forall(_.neLabel == label))
    label
  }

  override def getLabel: String = label

  override def getCaseInsensitiveMatching: Boolean = caseInsensitiveMatching

  override def withTokens(f: Array[String] => Unit): Unit =
      kbEntries.foreach { kbEntry =>
        new ReachSingleStandardKbSource(kbEntry, caseInsensitiveMatching).withTokens(f)
      }
}
