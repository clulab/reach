package org.clulab.reach.dyce.models

import org.clulab.reach.dyce.{Connection, Participant}

import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._

/**
  * Created by enrique on 09/03/17.
  */
class GFSModel extends SearchModel{

  def this(source:Participant, destination:Participant) {
    this()
    addNode(source)
    addNode(destination)
  }


  private def connectionToEdge(c:Connection):LDiEdge[Participant] = (c.controller ~+> c.controlled)(c.sign)
  private def edgeToConnection(e:G.EdgeT):Connection = Connection(e.source, e.target, e.label.value.asInstanceOf[Boolean], Seq())

  val G = Graph[Participant, LDiEdge]()

  override def addNode(node: Participant) = G add node

  override def addEdge(e: Connection) = G add connectionToEdge(e)

  override def addEdges(es: Iterable[Connection]) = G ++= es.map(connectionToEdge)

  override def nodes = G.nodes map (_.value)

  override def numNodes = G.nodes.size

  override def numEdges = G.edges.size

  override def edges = G.edges map edgeToConnection

  override def degree(node: Participant) = G.find(node) match {
    case Some(n) => n.degree
    case None => 0
  }

  override def shortestPath(source: Participant, destination: Participant) = {
    (G.find(source), G.find(destination)) match {
      case (Some(pa), Some(pb)) => {
        pa shortestPathTo pb match{
          case Some(path) => Some{
            path.edges.map{
              e => Connection(e.source, e.target, e.label.value.asInstanceOf[Boolean], Seq(""))
            }.toSeq
          }
          case None => None
        }
      }
      case _ => None
    }
  }

  override def allPaths(source: Participant, destination: Participant) = {
    shortestPath(source, destination) match {
      case Some(path) => Seq(path)
      case None => Seq()
    }
  }

  override def connectedComponents() = {
    G.componentTraverser().map{
      c => c.nodes.map(_.value)
    }.toSeq
  }

  override def getConnectedComponentOf(node: Participant) = {
    val components = G.componentTraverser().filter(c => c.nodes.map(_.value).contains(node))

    components.size match {
      case 1 =>
        val component = components.head
        Some(component.nodes.map(_.value))
      case 0 => None
      case _ => throw new RuntimeException(s"Participant $node in more than one component!!")
    }
  }

}
