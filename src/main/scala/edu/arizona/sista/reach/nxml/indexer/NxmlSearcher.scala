package edu.arizona.sista.reach.nxml.indexer

import java.io.{FileWriter, PrintWriter, File}
import java.nio.file.Paths

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.utils.StringUtils
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{TopScoreDocCollector, IndexSearcher}
import org.apache.lucene.store.FSDirectory
import org.slf4j.LoggerFactory

import scala.collection.mutable

import NxmlSearcher._

import scala.collection.mutable.ArrayBuffer

/**
 * Searches the NXML index created by NXML indexer
 * User: mihais
 * Date: 10/19/15
 */
class NxmlSearcher(val indexDir:String) {
  val reader = DirectoryReader.open(FSDirectory.open(Paths.get(indexDir)))
  val searcher = new IndexSearcher(reader)
  val proc = new BioNLPProcessor()

  def close() = reader.close()

  def docs(ids:Set[Int]):Set[Document] = {
    val ds = new mutable.HashSet[Document]()
    for(id <- ids) ds += searcher.doc(id)
    ds.toSet
  }

  def saveIds(docs:Set[Document]): Unit = {
    val os = new PrintWriter(new FileWriter("ids.txt"))
    for(doc <- docs) {
      val id = doc.get("id")
      os.println(id)
    }
    os.close()
  }

  def saveNxml(resultDir:String, docs:Set[Document]): Unit = {
    for(doc <- docs) {
      val id = doc.get("id")
      val nxml = doc.get("nxml")
      val os = new PrintWriter(new FileWriter(resultDir + File.separator + id + ".nxml"))
      os.print(nxml)
      os.close()
    }
  }

  def search(query:String, totalHits:Int = TOTAL_HITS):Set[Int] = {
    searchByField(query, "text", new StandardAnalyzer(), totalHits)
  }

  def searchByField(query:String,
                    field:String,
                    analyzer:Analyzer,
                    totalHits:Int = TOTAL_HITS,
                    verbose:Boolean = true):Set[Int] = {
    val q = new QueryParser(field, analyzer).parse(query)
    val collector = TopScoreDocCollector.create(totalHits)
    searcher.search(q, collector)
    val hits = collector.topDocs().scoreDocs
    val results = new mutable.HashSet[Int]
    for(hit <- hits) {
      val docId = hit.doc
      results += docId
    }
    if(verbose) logger.debug(s"""Found ${results.size} results for query "$query"""")
    results.toSet
  }

  def intersection(s1:Set[Int], s2:Set[Int]):Set[Int] = {
    val result = new mutable.HashSet[Int]()
    for(s <- s1) if(s2.contains(s)) result += s
    result.toSet
  }

  def union(s1:Set[Int], s2:Set[Int]):Set[Int] = {
    val result = new mutable.HashSet[Int]()
    s1.foreach(result += _)
    s2.foreach(result += _)
    result.toSet
  }

  def countDocsContaining(eventDocs:Set[Int], token:String):Int = {
    val query = "Ras AND " + token
    val result = intersection(eventDocs, search(query))
    result.size
  }

  def useCase(resultDir:String): Unit = {
    val eventDocs = search("phosphorylation phosphorylates ubiquitination ubiquitinates hydroxylation hydroxylates sumoylation sumoylates glycosylation glycosylates acetylation acetylates farnesylation farnesylates ribosylation ribosylates methylation methylates binding binds")
    val result = intersection(eventDocs, search("Ras AND (ROS OR MAPK OR Raf/Mek/Erk OR Akt OR NfkB OR TGFb OR TGFbeta OR TGFb1 OR TGFbeta1 OR EGFR OR apoptosis OR autophagy OR proliferation OR p53 OR RB OR glycolysis OR exosomes OR RAGE OR HMGB1)"))
    logger.debug(s"The result contains ${result.size} documents.")
    val resultDocs = docs(result)
    saveNxml(resultDir, resultDocs)
    saveIds(resultDocs)

    //
    // histogram of term distribution in docs
    //
    logger.debug("Generating topic histogram...")
    val histoPoints = Array("ROS", "MAPK", "Raf/Mek/Erk", "Akt", "NfkB", "TGFb", "TGFbeta", "TGFb1", "TGFbeta1", "EGFR", "apoptosis", "autophagy", "proliferation", "p53", "RB", "glycolysis", "exosomes", "RAGE", "HMGB1")
    val histoValues = new ArrayBuffer[(String, Int)]()
    for(point <- histoPoints) {
      histoValues += new Tuple2(point, countDocsContaining(eventDocs, point))
    }
    val histoFile = new PrintWriter(new FileWriter(resultDir + File.separator + "histo.txt"))
    for(i <- histoValues.sortBy(0 - _._2)) {
      histoFile.println(s"${i._1}\t${i._2}")
    }
    histoFile.close()
    logger.debug("Done.")
  }

  def searchByIds(ids:Array[String], resultDir:String): Unit = {
    val result = new mutable.HashSet[Int]()
    for(id <- ids) {
      val docs = searchByField(id, "id", new WhitespaceAnalyzer, verbose = false)
      if(docs.isEmpty) {
        logger.info(s"Found 0 results for id $id!")
      } else if(docs.size > 1) {
        logger.info(s"Found ${docs.size} for id $id, which should not happen!")
      }
      result ++= docs
    }
    logger.debug(s"Found ${result.size} documents for ${ids.length} ids.")
    val resultDocs = docs(result.toSet)

    saveNxml(resultDir, resultDocs)
  }
}

object NxmlSearcher {
  val logger = LoggerFactory.getLogger(classOf[NxmlSearcher])
  val TOTAL_HITS = 200000

  def main(args:Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)
    val indexDir = props.getProperty("index")
    val resultDir = props.getProperty("output")
    val searcher = new NxmlSearcher(indexDir)

    if(props.containsKey("ids")) {
      val ids = readIds(props.getProperty("ids"))
      searcher.searchByIds(ids, resultDir)
    } else {
      searcher.useCase(resultDir)
    }

    searcher.close()
  }

  def readIds(fn:String):Array[String] = {
    val ids = new ArrayBuffer[String]()
    for(line <- io.Source.fromFile(fn).getLines()) {
      var l = line.trim
      if (! l.startsWith("PMC"))
        l = "PMC" + l
      ids += l
    }
    ids.toArray
  }
}
