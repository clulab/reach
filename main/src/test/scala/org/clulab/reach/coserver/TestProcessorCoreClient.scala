package org.clulab.reach.coserver

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import org.scalatest.{ Matchers, FlatSpec }

/**
  * Tests of the ProcessorCoreClient.
  *   Written by: Tom Hicks. 6/20/2017.
  *   Last Modified: Initial creation.
  */
class TestProcessorCoreClient extends FlatSpec with Matchers with LazyLogging {

  // load application configuration from the configuration file
  val config = ConfigFactory.load().getConfig("ProcessorCoreClient")
  logger.debug(s"(TestProcessorCoreClient): config=${config}")

  // create a processor core server instance
  val client = new ProcessorCoreClient
  logger.debug(s"(TestProcessorCoreClient): client=${client}")

  "ProcessorCoreServer" should "return path to the processor server" in {
    val path = client.serverPath.toString
    logger.error(s"PATH=${path}")           // TODO LATER: set to debug
    (path) should not be (null)
    (path.toString) should equal("akka://proc-core-server/user/proc-actor-pool")
  }

}
