package org.clulab.reach.focusedreading.reinforcement_learning.policies

import org.clulab.reach.focusedreading.reinforcement_learning.Actions
import org.clulab.reach.focusedreading.reinforcement_learning.states.State
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import scala.language.implicitConversions

/**
  * Created by enrique on 26/03/17.
  */
trait Policy {
  def selectAction(s:State):Actions.Value

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
        new EpGreedyPolicy(epsilon, new LinearApproximationValues)
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
