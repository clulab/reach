package org.clulab.reach.dyce.models

import org.clulab.reach.dyce.{Connection, Participant}

import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import collection.mutable
import scala.annotation.tailrec

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

    // Do breath first search
    val colors = new mutable.HashMap[Participant, String]
    val parents = new mutable.HashMap[Participant, mutable.ArrayBuffer[(Participant, Boolean)]]
    val frontier = new mutable.Queue[Participant]

    // Initialize
    for(vertex <- this.G.nodes){
      val p = vertex.value
      colors += (p -> "White")
      parents += (p -> new mutable.ArrayBuffer[(Participant, Boolean)])
    }

    colors(source) = "Grey"
    frontier += source

    // Do the graph walk iteratively
    while(frontier.nonEmpty){
      // Get the visited element and its internal node
      val p = frontier.dequeue()
      val s = G.find(p).get

      // Iterate through it's outward edges
      for((d, sign) <- s.outgoing.filter(e => e.target.value != e.source.value).map(e => (e.target.value, e.label.value.asInstanceOf[Boolean]))){
        val color = colors(d)
        if(color == "White"){
          colors(d) = "Grey"
          parents(d) += Tuple2(p, sign)
          frontier += d
        }
        else if(color == "Grey"){
          parents(d) += Tuple2(p, sign)
        }
        else if(color == "Black"){
          parents(d) += Tuple2(p, sign)
        }
      }

      colors(p) = "Black"

    }

    // Locate the destination and start building the paths
    val nDst = G.find(destination).get.value
    val ret = reconstructPaths(nDst, parents, 1)
    ret map (_.reverse) filter (_.head.controller == source)
  }

  private def reconstructPaths(node: Participant,
                               parentsCache: mutable.HashMap[Participant
                                 , mutable.ArrayBuffer[(Participant, Boolean)]], depth:Int):
  Iterable[List[Connection]] = {
    if(depth <= 6){
      val parents = parentsCache(node).toList

      parents flatMap {
        case (p, sign) =>
          val connection = Connection(p, node, sign, Seq())
          val subPaths = reconstructPaths(p, parentsCache, depth+1)

          if(subPaths != Nil){
            subPaths map {
              s =>
                connection::s
            }
          }
          else
            List(connection::Nil)

      }
    }
    else
      Seq(Nil)
      //Nil

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
