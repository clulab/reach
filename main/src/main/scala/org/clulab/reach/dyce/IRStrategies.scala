package org.clulab.reach.dyce

import org.clulab.reach.dyce.sqlite.SQLiteQueries
import org.clulab.reach.dyce.QueryStrategy._

/**
  * Created by enrique on 20/02/17.
  */
trait IRStrategy {
  def informationRetrival(query: Query):Iterable[String]
}


trait LuceneIRStrategy extends IRStrategy{
  val maxHits = 200

  override def informationRetrival(query: Query) = {
    val pmcids:Iterable[String] = query.strategy match {
      case Singleton => LuceneQueries.singletonQuery(query.A, maxHits)
      case Disjunction => LuceneQueries.binaryDisonjunctionQuery(query.A, query.B.get, maxHits)
      case Conjunction => LuceneQueries.binaryConjunctionQuery(query.A, query.B.get, maxHits)
      case Spatial => LuceneQueries.binarySpatialQuery(query.A, query.B.get, 20, maxHits)
      case Cascade => {
        var results = LuceneQueries.binarySpatialQuery(query.A, query.B.get, 20, maxHits)
        if(results.isEmpty){
          results = LuceneQueries.binaryConjunctionQuery(query.A, query.B.get, maxHits)
          if(results.isEmpty)
            results = LuceneQueries.binaryDisonjunctionQuery(query.A, query.B.get, maxHits)
        }
        results
      }

    }


    pmcids
  }
}

trait SQLIRStrategy extends IRStrategy{

  val daIR = new SQLiteQueries("/Users/enrique/Desktop/lucene_queries.sqlite")

  override def informationRetrival(query: Query) = {
    val pmcids: Iterable[String] = query.strategy match {
      case Singleton => daIR.singletonQuery(query.A)
      case Disjunction => daIR.binaryDisonjunctionQuery(query.A, query.B.get)
      case Conjunction => daIR.binaryConjunctionQuery(query.A, query.B.get)
      case Spatial => daIR.binarySpatialQuery(query.A, query.B.get)
      case Cascade => {
        var results = daIR.binarySpatialQuery(query.A, query.B.get)
        if (results.isEmpty) {
          results = daIR.binaryConjunctionQuery(query.A, query.B.get)
          if (results.isEmpty)
            results = daIR.binaryDisonjunctionQuery(query.A, query.B.get)
        }
        results
      }
    }

    pmcids
  }
}
