package org.clulab.reach

import com.typesafe.config.{ Config, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import org.clulab.processors.ProcessorAnnotator
import org.clulab.processors.client.ProcessorClient
import org.clulab.processors.clu.{ BioCluProcessor, CluProcessor }
import org.clulab.processors.bionlp.BioNLPProcessor

/**
  * Factory object which selects and returns a ProcessorAnnotator, configured from
  * a given or the default configuration file.
  *   Author: Tom Hicks. 11/14/2017.
  *   Last Modified: Initial creation.
  */
object ProcessorAnnotatorFactory extends LazyLogging {

  /** Return a processor annotator using the Reach default configuration. */
  def apply (): ProcessorAnnotator = apply(ConfigFactory.load())

  /** Return a processor annotator using the given configuration. */
  def apply (config: Config): ProcessorAnnotator = {
    if (!config.hasPath("processorAnnotator.type"))
      throw new RuntimeException(
        "Reach configuration must specify the ProcessorAnnotator type to use.")
    else
      makeProcessorAnnotator(config)
  }

  /** Instantiate and return a processor annotator using the given configuration. */
  private def makeProcessorAnnotator (config: Config): ProcessorAnnotator = {
    config.getString("processorAnnotator.type") match {
      case "bionlp" => new BioNLPProcessor()
      case "clu"    => new CluProcessor()
      case "clubio" => new BioCluProcessor()
      case "server" => ProcessorClient.instance
      case other    =>
        throw new RuntimeException(
          s"Invalid ProcessorAnnotator type specified in configuration: '${other}'.")
    }
  }

}
