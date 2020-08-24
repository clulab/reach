package org.clulab.reach.assembly.server

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.FiniteDuration

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization

import akka.actor.ActorSystem
import akka.event.{ Logging, LoggingAdapter }
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.{ ActorMaterializer, Materializer }


trait Service {
  implicit val serialization = Serialization
  implicit val formats = DefaultFormats
  implicit val system: ActorSystem
  implicit def executionContext: ExecutionContextExecutor
  implicit val materializer: Materializer

  val logger: LoggingAdapter

  val static = "org/clulab/reach/assembly/server/static"
  val routes = {
    logRequestResult("reach-assembly") {    // wrapper to log all results
      path("") {                            // index page
        getFromResource(s"$static/index.html")
      } ~
      path("annotate") {                    // annotation UI
        getFromResource(s"$static/annotate.html")
      } ~
      path("model") {                       // model UI
        getFromResource(s"$static/model.html")
      }
    }
  }
}


object AssemblyServer extends App with Service {

  val argMap = buildServerArgMap(args.toList)
  val akkaServerConfig = new AkkaServerConfig(argMap, Some("ReachAssemblyServer"))
  val config = akkaServerConfig.config
  val host = akkaServerConfig.host
  val port = akkaServerConfig.port

  override implicit val system: ActorSystem = ActorSystem("reach-assembly", config)
  override implicit val executionContext = system.dispatcher
  override implicit val materializer = ActorMaterializer()
  override val logger = Logging(system, getClass)

  val bindingFuture =  Http().bindAndHandle(handler= routes, interface = host, port = port)

  logger.info(s"Server online at http://${host}:${port}")
}
