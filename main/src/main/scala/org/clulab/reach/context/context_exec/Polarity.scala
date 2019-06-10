package org.clulab.reach.context.context_exec
import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.util.parsing.json.JSON
object Polarity extends App {
  val config = ConfigFactory.load()

  /*val activationJSONPath = config.getString("contextEngine.params.activationJSON")
  val activationJSONString = Source.fromFile(activationJSONPath).getLines().mkString
  val activationContents = JSON.parseFull(activationJSONString) match {
    case Some(t) => t.asInstanceOf[List[Map[String,Any]]]
    case None =>
  }*/

  val inhibitionJSONPath = config.getString("polarityContext.inhibitionJSON")
  val inhibitionJSONString = Source.fromFile(inhibitionJSONPath).getLines().mkString
  val inhibitionContents = JSON.parseFull(inhibitionJSONString).asInstanceOf[Some[List[Map[String, Any]]]]
  val mapList = inhibitionContents.getOrElse(List(Map()).asInstanceOf[List[Map[String, Any]]])

  println(mapList(0)("evidence"))



}
