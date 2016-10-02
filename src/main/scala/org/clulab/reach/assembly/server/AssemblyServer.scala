package org.clulab.reach.assembly.server

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.{ ConfigValueFactory, ConfigFactory }


object AssemblyServer extends App with Service {

  val argMap = buildArgMap(ServerConfig.defaults, args.toList)

  val p: Int = argMap(ServerConfig.port).toInt
  val h: String = argMap(ServerConfig.host)

  // Update config with values from command line
  val config = ServerConfig.defaultConfig
    .withValue(ServerConfig.defaultHostName, ConfigValueFactory.fromAnyRef(h))
    .withValue(ServerConfig.defaultPort, ConfigValueFactory.fromAnyRef(p))

  override implicit val system: ActorSystem = ActorSystem("reach-assembly", config)
  override implicit val executionContext = system.dispatcher
  override implicit val materializer = ActorMaterializer()
  override val logger = Logging(system, getClass)

  val bindingFuture =  Http().bindAndHandle(handler = routes, interface = h, port = p)

  logger.info(s"Server online at http://$h:$p")
}

/**
  * Server configuration
  */
object ServerConfig {

  val defaultConfig = ConfigFactory.load()
  val port = "port"
  val host = "host"
  val defaultPort = defaultConfig.getString("akka.http.server.port")
  val defaultHostName = defaultConfig.getString("akka.http.server.host")
  val defaults = Map(
    port -> defaultPort,
    host -> defaultHostName
  )
}