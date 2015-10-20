package edu.arizona.sista.reach.nxml.indexer

import java.io.{FileWriter, PrintWriter, File}
import java.nio.file.Paths

import edu.arizona.sista.utils.StringUtils
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{TopScoreDocCollector, IndexSearcher}
import org.apache.lucene.store.FSDirectory
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

import NxmlSearcher._

/**
 * Searches the NXML index created by NXML indexer
 * User: mihais
 * Date: 10/19/15
 */
class NxmlSearcher(val indexDir:String) {
  def search(query:String, totalHits:Int):Seq[Document] = {
    val analyzer = new StandardAnalyzer
    val q = new QueryParser("text", analyzer).parse(query)
    val index = FSDirectory.open(Paths.get(indexDir))
    val reader = DirectoryReader.open(index)
    val searcher = new IndexSearcher(reader)
    val collector = TopScoreDocCollector.create(totalHits)
    searcher.search(q, collector)
    val hits = collector.topDocs().scoreDocs
    val results = new ListBuffer[Document]
    for(hit <- hits) {
      val docId = hit.doc
      val d = searcher.doc(docId)
      results += d
    }
    reader.close()
    logger.debug(s"""Found ${results.size} results for query "$query"""")
    results.toList
  }

  def useCase(resultDir:String, totalHits:Int): Unit = {
    val docs = search("penetrances", totalHits)
    for(doc <- docs) {
      val id = doc.get("id")
      val nxml = doc.get("nxml")
      val os = new PrintWriter(new FileWriter(resultDir + File.separator + id + ".nxml"))
      os.print(nxml)
      os.close()
    }
  }
}

object NxmlSearcher {
  val logger = LoggerFactory.getLogger(classOf[NxmlSearcher])

  def main(args:Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)
    val indexDir = props.getProperty("index")
    val resultDir = props.getProperty("output")

    val searcher = new NxmlSearcher(indexDir)
    searcher.useCase(resultDir, 10000)
  }
}
