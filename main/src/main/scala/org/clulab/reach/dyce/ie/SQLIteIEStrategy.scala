package org.clulab.reach.dyce.ie

import org.clulab.reach.dyce.sqlite.SQLiteQueries
import org.clulab.reach.dyce.{Connection, Participant}

/**
  * Created by enrique on 12/03/17.
  */
trait SQLIteIEStrategy extends IEStrategy{

  val daIE = new SQLiteQueries("/Users/enrique/Desktop/dyce/code/interactions.sqlite")

  override def informationExtraction(pmcids: Iterable[String]):Iterable[Connection] = {

    // Query the DB
    //val info = pmcids.take(100).grouped(40) flatMap daIE.ieQuery
    val info = pmcids.grouped(40) flatMap daIE.ieQuery

    // Group the info by interaction
    val groups = info.toSeq.groupBy(i => (i._1, i._2, i._3))

    // Filter out the interactions that appear only once
    val interactions = groups.mapValues{
      l =>
        val first = l.head
        val freq =  l.map(_._4).sum
        val evidence = l.flatMap(_._5)
        (first._1, first._2, first._3, freq, evidence)
    }.values.filter(_._4 > 1)

    // Instantiate interaction objects
    val connections = interactions map {
      c =>
        new Connection(Participant("", c._1), Participant("", c._2), c._3, c._5)
    }

    connections
  }


  override def getEvidence(connection: Connection) = daIE.fetchEvidence(connection)
}
