package org.clulab.reach.context
import scala.util.parsing.json._
import com.typesafe.config.ConfigFactory
import scala.io.Source
import java.io.File
import org.clulab.reach.ReachSystem
import org.clulab.reach.context.ContextEngineFactory.Engine
object Polarity extends App {
  val config = ConfigFactory.load()
  val inhibitionJSONPath = config.getString("contextEngine.params.inhibitionJSON")
  val activationJSONPath = config.getString("contextEngine.params.activationJSON")
  val activationJSONString = Source.fromFile(activationJSONPath).getLines().mkString
  val activationContents = JSON.parseFull(activationJSONString) match {
    case Some(t) => t
    case None => null
  }
  println(activationContents)


}
