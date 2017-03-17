package org.clulab.reach.focusedreading.tracing

import java.text.SimpleDateFormat

import org.clulab.reach.focusedreading.{Connection, Participant}

import scalax.collection.mutable.Graph
import scalax.collection.edge.LDiEdge
import org.clulab.reach.focusedreading.ir.QueryStrategy.Strategy
import org.clulab.utils.Serializer
import java.util.Date
import java.nio.file.Path

/**
  * Created by enrique on 17/03/17.
  * Stores all the steps taken by a focused reader
  */

case class IterativeStep(val number:Int, val graphBefore:Option[Graph[Participant, LDiEdge]],
                         val graphAfter:Option[Graph[Participant, LDiEdge]], val chosenParticipants:(Participant, Participant),
                         val queryStrategy:Strategy,
                         val irResults:Iterable[(String, Double)], // PMCID + IR Score
                         val ieResults:Iterable[Connection])

case class AgentRunTrace(val source:Participant, val destination:Participant,
                         val steps:Seq[IterativeStep], val recoveredPath:Option[Seq[Connection]],
                         val groundTruth:Option[Seq[String]])


object AgentRunTrace{
  def save(trace:AgentRunTrace, path:Path):Unit = {

    val dir = path.toFile.getParentFile
    if(!dir.exists)
      dir.mkdirs

    val fileName = path.toString
    save(trace, fileName)
  }

  def save(trace:AgentRunTrace, path:String):Unit = Serializer.save(trace, path)
  def load(path:String):AgentRunTrace = Serializer.load[AgentRunTrace](path)

  def getFileName:String = {
    val now = new Date()
    val fmtString = "yyyy-MM-dd_HH:mm:ss"
    val formatter = new SimpleDateFormat(fmtString)
    val dateString = formatter.format(now)
    s"trace_$dateString.ser"
  }

  def getFileName(groundTruth:Seq[String]) = {
    val now = new Date()
    val fmtString = "yyyy-MM-dd_HH:mm:ss"
    val formatter = new SimpleDateFormat(fmtString)
    val dateString = formatter.format(now)
    val gtString = groundTruth.mkString("-")
    s"trace_${gtString}_$dateString.ser"
  }
}
