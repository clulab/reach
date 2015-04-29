package edu.arizona.sista.bionlp.reach.indexer

import edu.arizona.sista.bionlp.reach.structure.BioDocument

/**
 * Searches the index of pre-processed pubmed articles
 * User: mihais
 * Date: 11/5/14
 */
trait Searcher {
  /** Search the index for a paper with a specific pubmedid */
  def searchById(id:String): Array[BioDocument]

  /** Search the index using a regular text query */
  def searchByQuery(query:String, hitsPerPage:Int = 1000): Array[BioDocument]

  /** Call this before disposing of this Searcher object */
  def close()

  def shell() {
    while(true) {
      print("> ")
      val query = io.StdIn.readLine()
      val docs = searchByQuery(query)
      println(s"Found ${docs.size} documents.")

      for(d <- docs) {
        println(d)
      }
    }
  }
}
