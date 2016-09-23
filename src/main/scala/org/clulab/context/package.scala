package org.clulab

import com.typesafe.config.ConfigObject
import org.clulab.reach.mentions._
import scala.collection.JavaConverters._

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
}
