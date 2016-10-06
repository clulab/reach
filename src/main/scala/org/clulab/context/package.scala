package org.clulab

import com.typesafe.config.ConfigObject
import org.clulab.reach.mentions._
import scala.collection.JavaConverters._
import ai.lum.nxmlreader.standoff._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import ai.lum.common.Interval
import scala.language.implicitConversions

package object context {

  /** Type alias for the context map which maps context types to a sequence of values. */
  type ContextMap = Map[String, Seq[String]]

  /** Tell whether the given mention has a context map containing species info or not. */
  def hasSpeciesContext (mention:BioMention): Boolean =
    mention.context.exists(_.contains("Species"))

  /** Utility for returning context engine parameters from a configuration */
  def createContextEngineParams(contextConfig: ConfigObject): Map[String, String] = {
    contextConfig.keySet.asScala.map {
      key => key -> contextConfig.asScala.apply(key).unwrapped.toString
    }.toMap
  }

  // Companion object to keep the json deserialization code
  object RichTree {
    implicit lazy val formats = DefaultFormats

    def parseJson(txt:String):Tree = {

      def parseHelper(elem:JObject):Tree ={
        // Figure out if this isn't a terminal object
        if(elem.obj.exists(f => f._1 == "children")){
          // This is an internal node in the AST
          new NonTerminal(
            (elem \ "label").extract[String],
            (elem \ "children").extract[List[JObject]].map(parseHelper),
            (elem \ "attributes").extract[Map[String, String]]
          )
        }
        else{
          // This is a terminal in the AST
          new Terminal(
            (elem \ "label").extract[String],
            (elem \ "text").extract[String],
            Interval.open((elem \ "start").extract[Int], (elem \ "end").extract[Int])
          )
        }
      }

      val json = parse(txt).asInstanceOf[JObject]
      parseHelper(json)
    }

    def readJson(path:String):Tree = {
      val json = io.Source.fromFile(path).getLines.mkString("\n")
      parseJson(json)
    }

  }

  trait RichTree {
    def toJson:JObject
    def printJson: String = pretty(render(this.toJson))
  }

  case class RichTerminal(t:Terminal) extends Terminal(t.label, t.text, t.interval) with RichTree{

    def toJson: JObject = {
      // Create a dictionary to be rendered as JSON
      ("label" -> this.label) ~
      ("text" -> this.text) ~
      ("start" -> this.interval.start) ~
      ("end" -> this.interval.end)
    }
  }

  case class  RichNonTerminal(t:NonTerminal) extends NonTerminal(t.label, t.children, t.attributes) with RichTree{

    def toJson: JObject = {
      // Create a dictionary to be rendered as JSON
      ("label" -> this.label) ~
      ("text" -> this.text) ~
      ("start" -> this.interval.start) ~
      ("end" -> this.interval.end)
    }
  }

  implicit def tree2RichTree(tree :Tree):RichTree = tree match {
    case t:Terminal => RichTerminal(t)
    case nt:NonTerminal => RichNonTerminal(nt)
  }
}
