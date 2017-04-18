package org.clulab.reach.focusedreading.reinforcement_learning.policies

import org.clulab.reach.focusedreading.reinforcement_learning.actions.Action
import org.clulab.reach.focusedreading.reinforcement_learning.states.State
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.language.implicitConversions

/**
  * Created by enrique on 26/03/17.
  */
abstract class Policy(val actionSet:Set[Action]) {
  def selectAction(s:State):Action

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
        val actions = values.asInstanceOf[LinearApproximationValues].coefficients.keySet
        new EpGreedyPolicy(epsilon, values, actions)
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