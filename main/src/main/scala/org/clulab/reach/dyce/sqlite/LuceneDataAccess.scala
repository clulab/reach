package org.clulab.reach.dyce.sqlite

import java.sql._

import com.typesafe.scalalogging.LazyLogging
import jdk.internal.org.objectweb.asm.util.Printer
import org.clulab.reach.dyce.{Connection => _, _}

/**
  * Class that encapsulates the SQLite access related to Lucene queries
  * Created by enrique on 21/02/17.
  */


class LuceneDataAccess(val path:String) extends LazyLogging with LuceneIRStrategy{

  // Load the JDBC driver
  Class.forName("org.sqlite.JDBC")

  // DDL Commands
  val queryTable =
    """ CREATE TABLE Queries (
      |   id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      |   pa TEXT NOT NULL,
      |   pb TEXT,
      |   type TEXT NOT NULL,
      |   UNIQUE (pa, pb, type) ON CONFLICT IGNORE
    | );""".stripMargin

  val queryResultTable =
    """ CREATE TABLE QueryResults (
      |   queryid INTEGER NOT NULL,
      |   pmcid TEXT NOT NULL,
      |   FOREIGN KEY(queryid) REFERENCES Queries(id),
      |   PRIMARY KEY(queryid, pmcid)
    | );""".stripMargin

  val ddlStatements = Seq(queryTable, queryResultTable)
  ////////////////


  private def getConnection = DriverManager.getConnection(s"jdbc:sqlite:$path");

  def createDatabase(): Unit ={

    logger.info(s"Creating database in $path")
    val connection:Connection = getConnection

    // Create all the tables
    for(command <- ddlStatements){
      val statement:Statement = connection.createStatement()
      statement.executeUpdate(command)
      statement.close
    }

    connection.close
    logger.info("Finished creating database")
  }

  val insertQueryCommand = "INSERT INTO Queries(pa, pb, type) VALUES(?, ?, ?);"
  val insertQueryResults = "INSERT INTO QueryResults(queryid, pmcid) VALUES(?, ?);"

  def insert(q:Query): Unit ={

    // Execute the lucene query to get the results
    val results = this.informationRetrival(q)

    val conn = getConnection

    val statement = conn.prepareStatement(insertQueryCommand, Statement.RETURN_GENERATED_KEYS)
    statement.setString(1, q.A.id)

    q.B match {
      case Some(b) =>
        statement.setString(2, b.id)
      case None =>
        statement.setString(2, null)
    }

    statement.setString(3, q.strategy.toString)
    val qid = statement.executeUpdate

    val statement2 = conn.prepareStatement(insertQueryResults)
    for(r <- results){
      statement2.setInt(1, qid)
      statement2.setString(2, r)
      statement2.addBatch
    }

    conn.setAutoCommit(false)
    statement2.executeBatch
    conn.setAutoCommit(true)


    conn.close
  }

}

object DeleteMe extends App {
  val q = Query(QueryStrategy.Spatial,  Participant("uniprot", "Q13315"), None)
  val la = new LuceneDataAccess("eraseme.sqlite")
  la.insert(q)
}
