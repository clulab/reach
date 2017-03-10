package org.clulab.reach.dyce.models

import gremlin.scala._
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph
import org.clulab.reach.dyce.{Connection, Participant}

/**
  * Created by enrique on 09/03/17.
  */
class TinkerPopModel extends SearchModel{


  // The graph backend
  val graph = TinkerGraph.open.asScala

  // TinkerPop type-safe attributes
  val ParticipantValue = Key[Participant]("participant")
  val Sign = Key[Boolean]("sign")

  // Auxiliary constructor
  def this(source:Participant, destination:Participant) {
    this()
    addNode(source)
    addNode(destination)
  }

  private def find(p:Participant):Option[Vertex] = {
    val v = graph.V.has(ParticipantValue, p)
    if(v.exists)
      Some(v.head)
    else
      None
  }

  override def addNode(p: Participant): Unit = {
    if(graph.V.has(ParticipantValue, p).notExists)
      graph + ("node", ParticipantValue -> p)
  }

  override def addEdge(e: Connection): Unit = {

    // Locate the source or create it
    val src = find(e.controller) match {
      case Some(v) => v
      case None => graph + ("node", ParticipantValue -> e.controller)
    }

    // Locate the destination or create it
    val dst = find(e.controlled) match {
      case Some(v) => v
      case None => graph + ("node", ParticipantValue -> e.controlled)
    }

    // If the edge doesn't exist yet
    if(graph.V(src).outE.filter( ed=> ed.value[Boolean]("sign") == e.sign).inV.has(ParticipantValue -> e.controlled).notExists()){
      val edge = src --- ("activates", Sign -> e.sign) --> dst
    }

  }

  override def addEdges(es: Iterable[Connection]): Unit = es foreach addEdge

  override def nodes: Iterable[Participant] = graph.V.toList.map(v => v.value[Participant]("participant"))

  override def edges: Iterable[Connection] = graph.E.toList.map{
    e =>
      val controller = e.outVertex.value[Participant]("participant")
      val controlled = e.inVertex.value[Participant]("participant")

      val sign = e.property[Boolean]("sign").value

      Connection(controller, controlled, sign, Seq())
  }

  override def numNodes: Int = graph.V.count.head.toInt

  override def numEdges: Int = graph.E.count.head.toInt

  override def connectedComponents(): Iterable[Set[Participant]] = ???

  override def getConnectedComponentOf(node: Participant): Option[Iterable[Participant]] = {
    val components = connectedComponents filter (cc => cc contains node)

    // There should only be one
    assert(components.size <= 1, s"TinkerPop found more than one component for node $node")

    if(components.size == 1)
      Some(components.head)
    else
      None
  }

  override def shortestPath(source: Participant, destination: Participant): Option[Seq[Connection]] = ???

  override def allPaths(source: Participant, destination: Participant): Iterable[Seq[Connection]] = ???

  override def degree(node: Participant): Int = find(node) match {
    case Some(v) => v.bothE.count.head.toInt
    case None => 0
  }
}
