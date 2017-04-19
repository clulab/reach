package org.clulab.reach.focusedreading.reinforcement_learning.policies

import org.clulab.reach.focusedreading.reinforcement_learning.actions.{Action, DummyAction}
import org.clulab.reach.focusedreading.reinforcement_learning.states.{DummyState, State}
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.language.implicitConversions

/**
  * Created by enrique on 26/03/17.
  */
abstract class Policy {

  def selectAction(ss:Seq[State], possibleActions:Seq[Action]):(State, Action)

  def selectAction(s:State, possibleActions:Seq[Action]):(State, Action) = {
    val ss = Seq.fill(possibleActions.size)(s)
    selectAction(ss, possibleActions.toSeq)
  }

  // Save the policy as json
  def save(path:String)
}

object Policy {
  implicit lazy val formats = DefaultFormats

  def loadPolicy(ast:JObject):Policy = {
    (ast \ "type").extract[String] match {
      case "ep_greedy" =>
        val epsilon = (ast \ "epsilon").extract[Double]
        val values = Values.loadValues((ast \ "values").extract[JObject])
        //val actions = values.asInstanceOf[LinearApproximationValues].coefficients.keySet
        new EpGreedyPolicy(epsilon, values)
      case _ =>
        throw new NotImplementedError("Not yet implemented")
    }
  }

  def loadPolicy(path:String):Policy = {
    val text = io.Source.fromFile(path).getLines.mkString("\n")
    val json = parse(text).asInstanceOf[JObject]
    loadPolicy(json)
  }
}

case class DummyPolicy() extends Policy{
  override def selectAction(s:Seq[State], possibleActions: Seq[Action]): (State, Action) = (DummyState(), DummyAction())

  override def save(path: String): Unit = {}
}