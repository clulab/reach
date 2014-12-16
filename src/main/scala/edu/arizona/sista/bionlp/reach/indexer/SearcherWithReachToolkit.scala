package edu.arizona.sista.bionlp.reach.indexer

import java.io.File

import edu.arizona.sista.bionlp.reach.structure.BioDocument
import edu.arizona.sista.utils.StringUtils._
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{Query, TopScoreDocCollector, IndexSearcher}
import org.apache.lucene.store.SimpleFSDirectory

import scala.collection.mutable.ListBuffer

/**
 * Searches the index constructed by the IndexerWithReachToolkit class in the same package
 * User: mihais
 * Date: 9/15/14
 */
class SearcherWithReachToolkit (val indexDir:String) extends Searcher {
  val indexReader = DirectoryReader.open(new SimpleFSDirectory(new File(indexDir)))
  val indexSearcher = new IndexSearcher(indexReader)

  /** Call this before disposing of this Searcher object */
  def close() {
    indexReader.close()
  }

  /** Search the index for a paper with a specific pubmedid; this searches the Indexer.PAPERID field */
  def searchById(id:String): Array[BioDocument] = {
    val q = mkQuery(id, IndexerWithReachToolkit.PAPERID)
    // note that we index sections as separate documents, so we may get multiple docs as a result of this search
    search(q, SearcherWithReachToolkit.MAX_SECTIONS_PER_PAPER)
  }

  /**
   * Makes a Lucene query object
   * Note: the query is tokenized using white spaces only!
   * If any bio-specific tokenization is needed, it MUST be done before calling this method!
   */
  def mkQuery(queryStr:String, field:String):Query = {
    new QueryParser(IndexerWithReachToolkit.VERSION, field, new WhitespaceAnalyzer(IndexerWithReachToolkit.VERSION)).parse(queryStr)
  }

  /** Search the index using a regular text query; this searches the Indexer.TOK field */
  def searchByQuery(query:String, hitsPerPage:Int = 1000): Array[BioDocument] = {
    val q = mkQuery(query, IndexerWithReachToolkit.TOK)
    search(q, hitsPerPage)
  }

  def search(query:Query, hitsPerPage:Int): Array[BioDocument] = {
    // the actual search
    val collector = TopScoreDocCollector.create(hitsPerPage, true)
    indexSearcher.search(query, collector)
    val hits = collector.topDocs().scoreDocs
    val reader = new ReachToolkitReader

    // traverse the Lucene results and construct the BioDocument objects
    val bioDocs = new ListBuffer[BioDocument]
    for(i <- 0 until hits.length) {
      val docId = hits(i).doc
      val d = indexSearcher.doc(docId)

      val sentences = reader.readSentences(
        d.get(IndexerWithReachToolkit.TAGS),
        d.get(IndexerWithReachToolkit.PARSE),
        d.get(IndexerWithReachToolkit.SDEPCC))

      bioDocs += new BioDocument(
        d.get(IndexerWithReachToolkit.PAPERID),
        d.get(IndexerWithReachToolkit.SECTION),
        sentences
      )
    }

    bioDocs.toArray
  }
}

object SearcherWithReachToolkit {
  val MAX_SECTIONS_PER_PAPER = 20

  def main(args:Array[String]) {
    val props = argsToProperties(args)
    val searcher = new SearcherWithReachToolkit(props.getProperty("index.dir"))
    searcher.shell()
    searcher.close()
  }
}
