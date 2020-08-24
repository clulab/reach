package org.clulab.reach.assembly.server

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }

/**
  * Class to load configuration information and setup Akka server defaults.
  */
class AkkaServerConfig (
  /** Map of command line argument name strings to value strings. */
  val argMap: Map[String,String] = Map[String,String](),

  /** Optional key for server-specific portion of the configuration file. */
  val serverConfigKey: Option[String] = None
)
{
  val loadedConfig = ConfigFactory.load()
  val serverConfig = if (serverConfigKey.isDefined && loadedConfig.hasPath(serverConfigKey.get))
    loadedConfig.getConfig(serverConfigKey.get) else loadedConfig

  val configHost = serverConfig.getString("akka.http.server.host")
  val configPort = serverConfig.getString("akka.http.server.port")

  val host: String = argMap.getOrElse("host", configHost)
  val port: Int = argMap.getOrElse("port", configPort).toInt

  val config = serverConfig
    .withValue("host", ConfigValueFactory.fromAnyRef(host))
    .withValue("port", ConfigValueFactory.fromAnyRef(port))
}
