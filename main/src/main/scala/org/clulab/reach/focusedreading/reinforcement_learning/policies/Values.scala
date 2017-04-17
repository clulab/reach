package org.clulab.reach.focusedreading.reinforcement_learning.policies

import breeze.linalg.DenseVector

import collection.mutable
import org.clulab.reach.focusedreading.reinforcement_learning.Actions
import org.clulab.reach.focusedreading.reinforcement_learning.states.State
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import scala.language.implicitConversions

/**
  * Created by enrique on 26/03/17.
  */


abstract class Values(val tolerance:Double = 1e-4){

  def apply(key:(State, Actions.Value)):Double
  def tdUpdate(current:(State, Actions.Value), next:(State, Actions.Value), reward:Double, rate:Double, decay:Double):Boolean
  def toJson:JObject
}

object Values{
  implicit lazy val formats = DefaultFormats

  def loadValues(ast:JObject):Values = {
    (ast \ "type") match {
      case JString("linear") =>
        val valsExplore = (ast \ "coefficientsExplore").asInstanceOf[JObject].obj
        val valsExploit = (ast \ "coefficientsExploit").asInstanceOf[JObject].obj

        // Make a map out of the coefficients
        val coefficientsExplore = new mutable.HashMap[String, Double]
        for((k, v) <- valsExplore){
          coefficientsExplore += (k -> v.extract[Double])
        }

        val coefficientsExploit = new mutable.HashMap[String, Double]
        for((k, v) <- valsExploit){
          coefficientsExploit += (k -> v.extract[Double])
        }
        new LinearApproximationValues(coefficientsExplore, coefficientsExploit)
      case _ =>
        throw new NotImplementedError("Not yet implemented")
    }
  }
}

class TabularValues(default:Double) extends Values {
  val backEnd = new mutable.HashMap[(State, Actions.Value), Double]

  override def apply(key:(State, Actions.Value)): Double = {
    if(backEnd.contains(key))
      backEnd(key)
    else{
      backEnd += (key -> default)
      default
    }
  }

  override def tdUpdate(current:(State, Actions.Value), next:(State, Actions.Value), reward:Double, rate:Double, decay:Double) = {
    val value:Double = this(current)
    val newValue:Double = value + (rate*(reward + decay*this(next) - value))

    backEnd(current) =  newValue

    // Return whether the current changed above the requested tolerance
    if(Math.abs(newValue - value) > tolerance)
      true
    else
      false
  }

  override def toJson = {
    ("coming" -> "soon")
  }

}


class LinearApproximationValues(val coefficientsExplore:mutable.HashMap[String, Double] = new mutable.HashMap[String, Double], val coefficientsExploit:mutable.HashMap[String, Double] = new mutable.HashMap[String, Double]) extends Values {

  //val coefficients = new mutable.HashMap[String, Double]

  val coefficientMemoryExplore = new mutable.ArrayBuffer[DenseVector[Double]]
  val coefficientMemoryExploit = new mutable.ArrayBuffer[DenseVector[Double]]

  override def apply(key:(State, Actions.Value)): Double = {

    val action = key._2
    val coefficients = action match {
      case Actions.Conjunction => coefficientsExploit
      case Actions.Disjunction => coefficientsExplore
    }
    // Encode the state vector into features
    val features = Map("bias" -> 1.0) ++ key._1.toFeatures //++ Actions.toFeatures(key._2)

    // Do the dot product with the coefficients
    val products = features map {
      case (k, v) =>
        val coefficient = coefficients.lift(k).getOrElse(0.0)
        coefficient*v
    }

    // Return the summation
    products.sum
  }

  override def tdUpdate(current:(State, Actions.Value), next:(State, Actions.Value), reward: Double, rate: Double, decay: Double): Boolean = {

    val action = current._2
    val coefficients = action match {
      case Actions.Conjunction => coefficientsExploit
      case Actions.Disjunction => coefficientsExplore
    }

    // The gradient are the feature values because this is a linear function optimizing MSE
    val gradient = Map("bias" -> 1.0) ++ current._1.toFeatures //++ Actions.toFeatures(current._2)

    val currentVal = this(current)
    val nextVal =this(next)

    val ret = reward + decay*nextVal
    val delta =rate*(ret-currentVal)

    var change = false

    // Now perform the update
    for(k <- gradient.keys){
      val value = coefficients.lift(k).getOrElse(0.0)
      val newValue = value + delta*gradient(k)
      coefficients(k) = newValue

      // Return whether the current changed above the requested tolerance
      if(Math.abs(newValue - value) > tolerance)
        change = true

    }

    storeCurrentCoefficients()

    change
  }

  override def toJson = {
    ("type" -> "linear") ~
      ("coefficientsExplore" -> coefficientsExplore.toMap) ~
      ("coefficientsExploit" -> coefficientsExploit.toMap)
  }

  private def storeCurrentCoefficients(): Unit ={


    val keysExplore = coefficientsExplore.keySet.toSeq.sorted
    val valsExplore = keysExplore map coefficientsExplore
    coefficientMemoryExplore += DenseVector(valsExplore.toArray)

    val keysExploit = coefficientsExploit.keySet.toSeq.sorted
    val valsExploit = keysExploit map coefficientsExploit
    coefficientMemoryExploit += DenseVector(valsExploit.toArray)
  }
}
