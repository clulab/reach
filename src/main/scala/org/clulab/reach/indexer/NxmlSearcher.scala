package org.clulab.reach.indexer

import java.io.{FileWriter, PrintWriter, File}
import java.nio.file.Paths
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.utils.StringUtils
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

  def docs(ids:Set[(Int, Float)]):Set[(Document, Float)] = {
    val ds = new mutable.HashSet[(Document, Float)]()
    for(id <- ids) {
      ds += new Tuple2(searcher.doc(id._1), id._2)
    }
    ds.toSet
  }

  def saveIds(docs:Set[(Document, Float)]): Unit = {
    val os = new PrintWriter(new FileWriter("ids.txt"))
    for(doc <- docs) {
      val id = doc._1.get("id")
      os.println(id)
    }
    os.close()
  }

  def saveNxml(resultDir:String, docs:Set[(Document, Float)], howManyToSave:Int = 0): Unit = {
    val docSeq = if (howManyToSave > 0) {
      docs.toSeq.sortBy(-_._2).take(howManyToSave)
    } else {
      docs.toSeq.sortBy(-_._2)
    }
    val sos = new PrintWriter(new FileWriter(resultDir + File.separator + "scores.tsv"))
    for(doc <- docSeq) {
      val id = doc._1.get("id")
      val nxml = doc._1.get("nxml")
      val os = new PrintWriter(new FileWriter(resultDir + File.separator + id + ".nxml"))
      os.print(nxml)
      os.close()
      sos.println(s"$id\t${doc._2}")
    }
    sos.close()
  }

  def saveDocs(resultDir:String, docIds:Set[(Int, Float)]): Unit = {
    val sos = new PrintWriter(new FileWriter(resultDir + File.separator + "scores.tsv"))
    var count = 0
    for(docId <- docIds) {
      val doc = searcher.doc(docId._1)
      val id = doc.get("id")
      val nxml = doc.get("nxml")
      val os = new PrintWriter(new FileWriter(resultDir + File.separator + id + ".nxml"))
      os.print(nxml)
      os.close()
      sos.println(s"$id\t${docId._2}")
      count += 1
    }
    sos.close()
    logger.info(s"Saved $count documents.")
  }

  def search(query:String, totalHits:Int = TOTAL_HITS):Set[(Int, Float)] = {
    searchByField(query, "text", new StandardAnalyzer(), totalHits)
  }

  def searchByField(query:String,
                    field:String,
                    analyzer:Analyzer,
                    totalHits:Int = TOTAL_HITS,
                    verbose:Boolean = true):Set[(Int, Float)] = {
    val q = new QueryParser(field, analyzer).parse(query)
    val collector = TopScoreDocCollector.create(totalHits)
    searcher.search(q, collector)
    val hits = collector.topDocs().scoreDocs
    val results = new mutable.HashSet[(Int, Float)]
    for(hit <- hits) {
      val docId = hit.doc
      val score = hit.score
      results += new Tuple2(docId, score)
    }
    if(verbose) logger.debug(s"""Found ${results.size} results for query "$query"""")
    results.toSet
  }

  def intersection(s1:Set[(Int, Float)], s2:Set[(Int, Float)]):Set[(Int, Float)] = {
    val result = new mutable.HashSet[(Int, Float)]()
    for(s <- s1) {
      var found = false
      var otherScore = 0.0.toFloat
      for(o <- s2 if ! found) {
        if(s._1 == o._1) {
          found = true
          otherScore = o._2
        }
      }
      if(found) {
        result += new Tuple2(s._1, s._2 + otherScore)
      }
    }
    result.toSet
  }

  def union(s1:Set[Int], s2:Set[Int]):Set[Int] = {
    val result = new mutable.HashSet[Int]()
    s1.foreach(result += _)
    s2.foreach(result += _)
    result.toSet
  }

  def countDocsContaining(eventDocs:Set[(Int, Float)], token:String):Int = {
    // token could be a phrase; make sure quotes are used
    val query = s"""Ras AND "$token""""
    val result = intersection(eventDocs, search(query))
    result.size
  }

  def useCase(resultDir:String): Unit = {
    val eventDocs = search("phosphorylation phosphorylates ubiquitination ubiquitinates hydroxylation hydroxylates sumoylation sumoylates glycosylation glycosylates acetylation acetylates farnesylation farnesylates ribosylation ribosylates methylation methylates binding binds")
    val result = intersection(eventDocs, search("""Ras AND (ROS OR "antioxidant response element" OR Warburg OR MAPK OR "Raf/Mek/Erk" OR Akt OR NfkB OR TGFb OR TGFbeta OR TGFb1 OR TGFbeta1 OR integrins OR ADAM OR EGF OR EGFR OR RTK OR apoptosis OR autophagy OR proliferation OR "transcription factors" OR ATM OR p53 OR RB OR "tumor suppressors" OR glycolysis OR "pentose phosphate pathway" OR OXPHOS OR mitochondria OR "cell cycle" OR "energy balance" OR exosomes OR RAGE OR HMGB1)"""))
    logger.debug(s"The result contains ${result.size} documents.")
    val resultDocs = docs(result)
    saveNxml(resultDir, resultDocs, 0)
    saveIds(resultDocs)

    //
    // histogram of term distribution in docs
    //

    logger.debug("Generating topic histogram...")
    val histoPoints = Array(
      "ROS",
      "antioxidant response element",
      "Warburg",
      "MAPK",
      "Raf/Mek/Erk",
      "Akt",
      "NfkB",
      "TGFb",
      "TGFbeta",
      "TGFb1",
      "TGFbeta1",
      "integrins",
      "ADAM",
      "EGF",
      "EGFR",
      "EGFR",
      "RTK",
      "apoptosis",
      "autophagy",
      "proliferation",
      "transcription factors",
      "ATM",
      "p53",
      "RB",
      "tumor suppressors",
      "glycolysis",
      "pentose phosphate pathway",
      "exosomes",
      "OXPHOS",
      "mitochondria",
      "cell cycle",
      "energy balance",
      "RAGE",
      "HMGB1")

    val histoValues = new ArrayBuffer[(String, Int)]()
    for(point <- histoPoints) {
      histoValues += new Tuple2(point, countDocsContaining(result, point))
    }
    val histoFile = new PrintWriter(new FileWriter(resultDir + File.separator + "histo.txt"))
    for(i <- histoValues.sortBy(0 - _._2)) {
      histoFile.println(s"${i._1}\t${i._2}")
    }
    histoFile.close()


    logger.debug("Done.")
  }

  /** Finds all NXML that contain at least one biochemical interaction */
  def useCase2(resultDir:String): Unit = {
    val eventDocs = search("phosphorylation phosphorylates ubiquitination ubiquitinates hydroxylation hydroxylates sumoylation sumoylates glycosylation glycosylates acetylation acetylates farnesylation farnesylates ribosylation ribosylates methylation methylates binding binds")
    logger.debug(s"The result contains ${eventDocs.size} documents.")
    saveDocs(resultDir, eventDocs)
    logger.debug("Done.")
  }

  def useCase3(resultDir:String): Unit = {
    val eventDocs = search("children AND ((TNFAlpha AND nutrition) OR (inflammation AND stunting) OR (kcal AND inflammation) OR (protein AND inflammation) OR (nutrition AND inflammation))")
    logger.debug(s"The result contains ${eventDocs.size} documents.")
    saveDocs(resultDir, eventDocs)
    logger.debug("Done.")
  }

  def useCaseTB(resultDir:String): Unit = {
    val eventDocs = search(""" "chronic inflammation" AND ("tissue damage" OR "tissue repair" OR "wound healing" OR "angiogenesis" OR "fibrosis" OR "resolvin" OR "eicosanoid" OR "tumor-infiltrating lymphocyte" OR "lymphoid aggregate" OR "granuloma" OR "microbiome" OR "short-chain fatty acid") """)
    logger.info(s"The result contains ${eventDocs.size} documents.")
    saveDocs(resultDir, eventDocs)
    logger.info("Done.")
  }

  // Natasa's use case, first query
  def useCase3a(resultDir:String): Unit = {
    val eventDocs = search("""(TGFbeta1 OR "Transforming Growth Factor beta 1") AND (BMP OR "Bone Morphogenetic Protein")""")
    logger.debug(s"The result contains ${eventDocs.size} documents.")
    saveDocs(resultDir, eventDocs)
    logger.debug("Done.")
  }

  def searchByIds(ids:Array[String], resultDir:String): Unit = {
    val result = new mutable.HashSet[(Int, Float)]()
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
    saveIds(resultDocs)
  }
}

object NxmlSearcher {
  val logger = LoggerFactory.getLogger(classOf[NxmlSearcher])
  val TOTAL_HITS = 500000

  def main(args:Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)
    val indexDir = props.getProperty("index")
    val resultDir = props.getProperty("output")
    val searcher = new NxmlSearcher(indexDir)

    if(props.containsKey("ids")) {
      val ids = readIds(props.getProperty("ids"))
      searcher.searchByIds(ids, resultDir)
    } else {
      searcher.useCase3a(resultDir)
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
