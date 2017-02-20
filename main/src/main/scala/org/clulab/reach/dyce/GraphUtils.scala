package org.clulab.reach.dyce

import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph // shortcuts

import SearchAgent.Model

/**
  * Created by enrique on 20/02/17.
  */
object GraphUtils {

  /***
    * Gets the sets of nodes in the connected components
    * @return Iterable with a set of nodes for each connected component
    */
  def getConnectedComponents(G:Model):Iterable[Set[Participant]] = {
    G.componentTraverser().map{
      c => c.nodes.map(_.value)
    }.toSeq
  }

  def getComponentOf(p:Participant, m:Model):Option[Set[m.NodeT]] = {

    val components = m.componentTraverser().filter(c => c.nodes.map(_.value).contains(p))

    components.size match {
      case 1 =>
        val component = components.head
        Some(component.nodes)
      case 0 => None
      case _ => throw new RuntimeException(s"Participant $p in more than one component!!")
    }
  }

//  def sortByDegree(nodes:Set[Model.NodeT])
}
