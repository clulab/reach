package org.clulab.reach.dyce.sqlite

import java.sql.DriverManager
import collection.mutable

import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.dyce.Participant

/**
  * Created by enrique on 26/02/17.
  */
class SQLiteQueries(path:String) extends LazyLogging{

  def removeNamespace(id:String):String = {
    val tokens = id.split(":")
    tokens.size match {
      case i:Int if i > 1 => tokens.drop(1).mkString(":")
      case 1 => id
    }
  }

  // Load the JDBC driver
  Class.forName("org.sqlite.JDBC")

  private val getConnection = DriverManager.getConnection(s"jdbc:sqlite:$path");


  /***
    * Expands the frontier with a focus on finding info that may create a path between participants
    * @param a Participant A
    * @param b Participant B
    * @return
    */
  def binarySpatialQuery(a:Participant, b:Participant):Iterable[String] = {
    val command =
      """ SELECT pmcid FROM Queries INNER JOIN QueryResults
        | ON id = queryid WHERE type = 'Spatial' AND pa = ? AND pb = ?
      """.stripMargin

    val conn = getConnection

    val cmd = conn.prepareStatement(command)

    cmd.setString(1, removeNamespace(a.id))
    cmd.setString(2, removeNamespace(b.id))

    val resultSet = cmd.executeQuery()

    val pmcids = new mutable.ArrayBuffer[String]()

    while(resultSet.next)
      pmcids += resultSet.getString("pmcid")

    cmd.close
    //conn.close

    // Do a set to remove duplicate entries, the back to a seq
    pmcids.toSeq
  }

  def binaryConjunctionQuery(a:Participant, b:Participant):Iterable[String] = {
    val command =
      """ SELECT pmcid FROM Queries INNER JOIN QueryResults
        | ON id = queryid WHERE type = 'Conjunction' AND pa = ? AND pb = ?
      """.stripMargin

    val conn = getConnection

    val cmd = conn.prepareStatement(command)

    cmd.setString(1, removeNamespace(a.id))
    cmd.setString(2, removeNamespace(b.id))

    val resultSet = cmd.executeQuery()

    val pmcids = new mutable.ArrayBuffer[String]()

    while(resultSet.next)
      pmcids += resultSet.getString("pmcid")

    cmd.close
    //conn.close

    // Do a set to remove duplicate entries, the back to a seq
    pmcids.toSeq
  }

  def binaryDisonjunctionQuery(a:Participant, b:Participant):Iterable[String] = {
    val command =
      """ SELECT pmcid FROM Queries INNER JOIN QueryResults
        | ON id = queryid WHERE type = 'Singleton' AND (pa = ? OR pa = ?)
      """.stripMargin

    val conn = getConnection

    val cmd = conn.prepareStatement(command)

    cmd.setString(1, removeNamespace(a.id))
    cmd.setString(2, removeNamespace(b.id))

    val resultSet = cmd.executeQuery()

    val pmcids = new mutable.ArrayBuffer[String]()

    while(resultSet.next)
      pmcids += resultSet.getString("pmcid")

    cmd.close
    //conn.close

    // Do a set to remove duplicate entries, the back to a seq
    pmcids.toSet.toSeq
  }

  def singletonQuery(p:Participant):Iterable[String] = {
    val command =
      """ SELECT pmcid FROM Queries INNER JOIN QueryResults
        | ON id = queryid WHERE type = 'Singleton' AND pa = ?
      """.stripMargin

    val conn = getConnection

    val cmd = conn.prepareStatement(command)

    cmd.setString(1, p.id)

    val resultSet = cmd.executeQuery()

    val pmcids = new mutable.ArrayBuffer[String]()

    while(resultSet.next)
      pmcids += resultSet.getString("pmcid")

    cmd.close
    //conn.close

    pmcids.toSeq
  }

  def ieQuery(pmcids:Iterable[String]):Seq[(String, String, Boolean, Int, Iterable[String])] = {

    // Fetch the interactions for the paper
    val commandInteractions =
      s""" SELECT i.*, pi.frequency
        |  FROM Paper_Interaction AS pi
        |  INNER JOIN Interactions AS i
        |  ON interaction = id
        | WHERE pmcid in (${pmcids.map(s => s"'$s'").mkString(",")})
      """.stripMargin

    // Fetch the evidence for the interactions
    val commandEvidence =
      """ SELECT pmcid, evidence
        |  FROM Evidence
        |  WHERE interaction = ?
      """.stripMargin

    val conn = getConnection

    var cmd = conn.prepareStatement(commandInteractions)

    //cmd.setString(1, pmcid)

    var resultSet = cmd.executeQuery

    val interactions = new mutable.ArrayBuffer[Tuple5[Int, String, String, Boolean, Int]] // Interaction ID, Controller, Controlled, Sign, Frequency

    while(resultSet.next){
      val id = resultSet.getInt("id")
      val controller = resultSet.getString("controller")
      val controlled = resultSet.getString("controlled")
      val sign = if(resultSet.getInt("direction") == 1)
        true
      else
        false
      val freq = resultSet.getInt("frequency")

      interactions += Tuple5(id, controller, controlled, sign, freq)
    }

    resultSet.close()
    cmd.close()

    val returnVal = new mutable.ArrayBuffer[(String, String, Boolean, Int, Iterable[String])]

//    cmd = conn.prepareStatement(commandEvidence)
//
//    for(interaction <- interactions){
//      val id = interaction._1
//      cmd.setInt(1, id)
//
//      resultSet = cmd.executeQuery
//      val ev = new mutable.ArrayBuffer[String]
//
//      while(resultSet.next)
//        ev += s"${resultSet.getString("pmcid")}: ${resultSet.getString("evidence")}"
//
//      returnVal += Tuple5(interaction._2, interaction._3, interaction._4, interaction._5, ev.toSeq)
//
//      resultSet.close()
//
//    }
//
//    cmd.close()

    for(interaction <- interactions) {
      returnVal += Tuple5(interaction._2, interaction._3, interaction._4, interaction._5, Nil)
    }

    //if(!conn.isClosed)
      //conn.close()

    returnVal.toSeq
  }
}
