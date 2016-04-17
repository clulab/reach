package edu.arizona.sista.assembly.relations

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.reach.nxml.indexer.NxmlSearcher
import edu.arizona.sista.embeddings.word2vec.Word2Vec
import edu.arizona.sista.reach.PaperReader
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.zip.GZIPOutputStream
import org.apache.commons.io.FileUtils
import org.apache.commons.compress.compressors.gzip._
import java.io.{FileOutputStream, Writer, OutputStreamWriter, File}


/**
 * Takes a path to a lucene index, <br>
 * tokenizes the text of each doc using BioNLPProcessor, <br>
 * sanitizes each word (prep for for w2v), <br>
 * and writes each doc to a .gz file where each line is a tokenized sentence of sanitized tokens
 */
object DumpIndex extends App {

  val config = ConfigFactory.load()
  val indexDir = config.getString("assembly.indexDir")
  val searcher = new NxmlSearcher(indexDir)
  val threadLimit = config.getInt("threadLimit")
  val bioproc = PaperReader.rs.processor
  val outDir = config.getString("assembly.indexDump")

  def writeToCompressedFile(text: String, outFile: String): Unit = {
    try {
      val output: FileOutputStream = new FileOutputStream(outFile)
      val writer: Writer = new OutputStreamWriter(new GZIPOutputStream(output), "UTF-8")
      writer.write(text)
      writer.close()
      output.close()
    } catch {
      case _ => println(s"Couldn't write $outFile")
    }
  }

  def dumpFilesFromIndex(searcher: NxmlSearcher, nThreads: Int): Unit = {
    // parallelize processing of indexed docs
    val docs = (0 to searcher.reader.maxDoc).par
    // limit threads
    docs.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(nThreads))

    for {
    // is there an easier way to iterate
    // over the indexed documents?
      i <- docs
      entry = searcher.reader.document(i)
      // get text and id
      txt = entry.getField("text").stringValue
      pmid = entry.getField("id").stringValue
      // tokenize
      doc = bioproc.annotate(txt)
      outFile = new File(outDir, s"$pmid.txt")
    // iterate over each sentence
    } {
      val sanitizedLines: Seq[String] = doc.sentences.map{ s =>
        // sanitize each word
        s.words.map(w => Word2Vec.sanitizeWord(w)).mkString(" ")
      }
      // write to disk...
      val gzipOutFile = GzipUtils.getCompressedFilename(outFile.getAbsolutePath)
      println(s"writing $gzipOutFile ...")
      writeToCompressedFile(sanitizedLines.mkString("\n"), gzipOutFile)
      //GzipCompressorOutputStream
      //FileUtils.writeLines(outFile, sanitizedLines)
    }
  }

  // prepare indexed papers for generation of embeddings
  dumpFilesFromIndex(searcher, threadLimit)
}