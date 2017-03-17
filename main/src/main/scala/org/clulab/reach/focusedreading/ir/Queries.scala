package org.clulab.reach.focusedreading.ir

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.queryparser.classic.QueryParserBase
import org.clulab.reach.focusedreading.Participant
import org.clulab.reach.grounding.ReachKBUtils
import org.clulab.reach.indexer.NxmlSearcher
import org.clulab.utils.Serializer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object QueryStrategy extends Enumeration{
  type Strategy = Value
  val Singleton, Disjunction, Conjunction, Spatial, Cascade = Value
}

case class Query(val strategy:QueryStrategy.Strategy, val A:Participant, val B:Option[Participant])


/**
  * Created by enrique on 18/02/17.
  */
object LuceneQueries extends LazyLogging{

  logger.info("Loading KBs...")
  var lines = ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("uniprot-proteins.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("GO-subcellular-locations.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("ProteinFamilies.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PubChem.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PFAM-families.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("bio_process.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("hgnc.tsv.gz")).getLines.toSeq

  val dict = lines.map{ l => val t = l.split("\t"); (t(1), t(0)) }.groupBy(t=> t._1).mapValues(l => l.map(_._2).distinct)

  val indexDir = "/data/nlp/corpora/pmc_openaccess/pmc_aug2016_index"
  val nxmlSearcher:NxmlSearcher = new NxmlSearcher(indexDir)
  val nxmlDir = "/work/enoriega/fillblanks/nxml"

  logger.info("Loading lucene record...")
  // Load the serialized record if exists, otherwise create a new one
  val ldcFile = new File(nxmlDir, "luceneDocRecord.ser")
  val luceneDocRecord = if(ldcFile.exists()){
    Serializer.load[mutable.HashMap[Int, String]](ldcFile.getAbsolutePath)
  }
  else{
    mutable.HashMap[Int, String]()
  }

  /***
    * Gets the synonyms from the KB files
    * @param term Grounding ID without namespace to look for
    * @return String with the disjunction of synonyms ready to be queried by lucene
    */
  def resolveParticipant(term:String) = {

    dict.lift(term) match {
      case Some(l) => "(" + l.map( x => "\"" + x + "\"").mkString(" OR ") + ")"
      case None =>
        logger.debug(s"Warning: missing term in the KB: $term")
        ""
    }
  }

  /***
    * Retrieves documents from lucene. If they have already been retrieved don't do it agaib
    * @param hits Set of documents coming from NxmlSearcher
    * @return list with the ids of documents already fetched from the index
    */
  def fetchHitsWithCache(hits: Set[(Int, Float)]): List[String] = {
    // Hits are tuples with (docId, score), fetch the documents from the ids if they haven't been fetched before
    val existing = new ListBuffer[String]
    val toFetch = new ListBuffer[(Int, Float)]

    for (record <- hits) {
      if (luceneDocRecord contains record._1) {
        // Get the IDs from the record
        existing += luceneDocRecord(record._1)
      }
      else {
        // Mark them for retrieval
        toFetch += record
      }
    }

    val tfs = toFetch.toSet
    // Fetch the Document objects
    val docs = nxmlSearcher.docs(tfs)
    val newPapers = docs.toSeq.sortBy(-_._2).map(d => d._1.get("id"))

    // Save them to disk
    nxmlSearcher.saveNxml(nxmlDir, docs)

    // Add them to the record
    for ((t, d) <- toFetch.sortBy(-_._2) zip newPapers) {
      luceneDocRecord += (t._1 -> d)
    }

    // Reserialize the record
    // Serializer.save[mutable.HashMap[Int, String]](luceneDocRecord, ldcFile.getAbsolutePath)

    existing.toList ++ newPapers
  }


//  val totalHits = 200 // Max # of hits per query


  /***
    * Expands the frontier with a focus on finding info that may create a path between participants
    * @param a Participant A
    * @param b Participant B
    * @return
    */
  def binarySpatialQuery(a:Participant, b:Participant, k:Int, totalHits:Int):Iterable[String] = {


    // Build a query for lucene
    val aSynonyms = resolveParticipant(a.id)
    val bSynonyms = resolveParticipant(b.id)

    if(aSynonyms.isEmpty || bSynonyms.isEmpty){
      return Set()
    }

    var luceneQuery = QueryParserBase.escape("(" + aSynonyms + " AND " + bSynonyms + ")~"+k)
    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    fetchHitsWithCache(hits)
  }

  def binaryConjunctionQuery(a:Participant, b:Participant, totalHits:Int):Iterable[String] = {
    // Build a query for lucene
    val aSynonyms = resolveParticipant(a.id)
    val bSynonyms = resolveParticipant(b.id)

    if(aSynonyms.isEmpty || bSynonyms.isEmpty){
      return Set()
    }

    var luceneQuery = QueryParserBase.escape("(" + aSynonyms + " AND " + bSynonyms + ")")
    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    fetchHitsWithCache(hits)
  }

  def binaryDisonjunctionQuery(a:Participant, b:Participant, totalHits:Int):Iterable[String] = {
    // Build a query for lucene
    val aSynonyms = resolveParticipant(a.id)
    val bSynonyms = resolveParticipant(b.id)

    if(aSynonyms.isEmpty || bSynonyms.isEmpty){
      return Set()
    }

    var luceneQuery = QueryParserBase.escape("(" + aSynonyms + " OR " + bSynonyms + ")")
    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    fetchHitsWithCache(hits)
  }

  def singletonQuery(p:Participant, totalHits:Int):Iterable[String] = {
    val synonyms = resolveParticipant(p.id)

    if(synonyms.isEmpty)
      return Set()

    var luceneQuery = QueryParserBase.escape("(" + synonyms + ")")
    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    fetchHitsWithCache(hits)
  }

}
