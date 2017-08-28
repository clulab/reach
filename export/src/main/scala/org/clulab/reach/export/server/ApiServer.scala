package org.clulab.reach.export.server

import java.io.File

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

import com.typesafe.config.Config

import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.{ Formats, DefaultFormats, jackson, native }

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import org.clulab.reach.export.apis.ApiRuler
import org.clulab.reach.export.apis.ApiRuler._

/**
  * Server to implement RESTful Reach API via Akka HTTP service.
  *   Written by: Tom Hicks. 8/17/2017.
  *   Last Modified: Implement GET text call as prototype.
  */
object ApiServer extends App {
  val argMap = buildServerArgMap(args.toList)
  val serverConfig = new AkkaServerConfig(argMap, Some("ApiServer"))
  val apiService = new ApiService(serverConfig)
}


/**
  * Glue-code connecting API server to existing annotation functionality and
  * hiding implementation details of the returned response map.
  */
trait ApiImpl {

  // A couple of fallback error messages
  private val defaultErrorMsg = "Unexpected internal server error"
  private val noResultMsg = "Unable to retrieve result"

  /** Return the results from processing the given NXML text with the REACH rulesets. */
  def doNxml (nxmlText: String, outputFormat:String = "fries"): Response =
    ApiRuler.annotateNxml(nxmlText, outputFormat)

  /** Return the results from processing the given text with the REACH rulesets. */
  def doText (text: String, outputFormat: String = "fries"): Response =
    ApiRuler.annotateText(text, outputFormat)

  /** Tell whether the given result response has an error or not. */
  def hasError (response: Response): Boolean =
    response.getOrDefault("hasError", "false").asInstanceOf[Boolean]

  /** Get the error message from given result response or return a default error message. */
  def getErrorMessage (response: Response): String =
    response.getOrDefault("errorMsg", defaultErrorMsg).asInstanceOf[String]

  /** Get the result JSON string from given result response or return an error message. */
  def getResult (response: Response): String =
    response.getOrDefault("result", noResultMsg).asInstanceOf[String]

  /** Return the media type for the given output format string. */
  def contentTypeFor (outputFormat: String): ContentType = outputFormat match {
    case "fries"       => ContentTypes.`application/json`
    case "indexcard"   => ContentTypes.`application/json`
    case "serial-json" => ContentTypes.`application/json`
    case "arizona"     => ContentTypes.`text/csv(UTF-8)`
    case "tsv"         => ContentTypes.`text/csv(UTF-8)`
    case _             => ContentTypes.`text/plain(UTF-8)`
  }

}


/**
  * Provide a Reach API service via Akka HTTP using Json4s support for marshalling.
  */
class ApiService (

  /** Application configuration overridden with command line arguments. */
  appConfig: AkkaServerConfig

) extends ApiImpl with Json4sSupport {

  val serverConfig = appConfig.config         // final args-merged configuration

  implicit val serialization = jackson.Serialization // or native.Serialization
  implicit val formats = DefaultFormats

  // setup Akka system
  implicit val system: ActorSystem = ActorSystem("apiserver", serverConfig)
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val logger = Logging(system, getClass)

  /** Convenience method to execute a task after a fixed duration of time. */
  def in [U] (duration: FiniteDuration)(body: => U): Unit =
    system.scheduler.scheduleOnce(duration)(body)

  /** Create and return the route for this app, using the given configuration, if needed. */
  def makeRoute (config: Config): Route = {
    val appVersion = config.getString("version")

    val static = "org/clulab/reach/export/server/static"
    val routesGet = {
      logRequestResult("apiserver") {       // wrap contained paths in logger
        get {                               // GETS
          pathPrefix("api") {
            path("text") {
              parameters("text", 'output ? "fries") { (text, output) =>
                logger.info(s"GET api/text -> ${text}, ${output}")
                val response = doText(text, output)
                if (hasError(response))
                  complete(StatusCodes.InternalServerError, getErrorMessage(response))
                else
                  complete(HttpResponse(StatusCodes.OK,
                                        List(headers.`Content-Type`(contentTypeFor(output))),
                                        entity = getResult(response) ))
              }
            } ~
            path("nxml") {
              parameters("nxml", 'output ? "fries") { (nxml, output) =>
                logger.info(s"GET api/nxml -> NXML(${nxml.size}), ${output}")
                val response = doNxml(nxml, output)
                if (hasError(response))
                  complete(StatusCodes.InternalServerError, getErrorMessage(response))
                else
                  complete(HttpResponse(StatusCodes.OK,
                                        List(headers.`Content-Type`(contentTypeFor(output))),
                                        entity = getResult(response) ))
              }
            }
          } ~
          path("") {                                // index page
            getFromResource(s"${static}/api.html")
          } ~
          path("index.html") {                      // index page
            getFromResource(s"${static}/api.html")
          } ~
          pathPrefix("static") {                    // SHOULD WORK BUT DOES NOT
            getFromResourceDirectory(s"/${static}")
          } ~
          path("application.css") {                 // application stylesheet
            getFromResource(s"${static}/application.css")
          } ~
          path("CLU-notext-trans_68x68.png") {      // image
            getFromResource(s"${static}/images/CLU-notext-trans_68x68.png")
          } ~
          path("version") {                         // show version
            logger.info(s"GET version")
            complete( ("version" -> appVersion) )
          }
        }
      }
    }  // end routesGet

    val routesPost = {
      logRequestResult("apiserver") {       // wrap contained paths in logger
        post {                              // POSTS
          pathPrefix("api") {
            path("text") {
              parameters("text", 'output ? "fries") { (text, output) =>
                logger.info(s"POST api/text -> ${text}, ${output}")
                val response = doText(text, output)
                if (hasError(response))
                  complete(500, getErrorMessage(response))
                else
                  complete(getResult(response))
              }
            } ~
            path("nxml") {
              parameters("nxml", 'output ? "fries") { (nxml, output) =>
                logger.info(s"POST api/nxml -> NXML(${nxml.size}), ${output}")
                val response = doNxml(nxml, output)
                if (hasError(response))
                  complete(500, getErrorMessage(response))
                else
                  complete(getResult(response))
              }
            }
          } ~
          path("version") {                         // show version
            logger.info(s"POST version")
            complete( ("version" -> appVersion) )
          } ~
          path("shutdown") {                        // shut down the server
            logger.info(s"POST shutdown")
            // complete request and then shut down the server in 1 second
            complete {
              in (1.second) {
                system.terminate()
              }
              "Stopping apiserver..."
            }
          }
        }  // post
      }
    }  // end routesPost

    routesGet ~ routesPost                  // return concatenated routes
  }

  val route = makeRoute(serverConfig)
  val host = appConfig.host
  val port = appConfig.port
  val bindingFuture =  Http().bindAndHandle(handler = route, interface = host, port = port)
  logger.info(s"Server online at http://$host:$port")
}


/** Trait implemented by all model classes which are used as messages. */
trait Message

/** A single text string message. */
case class TextMessage (val text: String) extends Message

