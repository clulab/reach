package org.clulab.reach.export.server

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.util.{Failure, Success}

import com.typesafe.config.Config

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink

import org.clulab.reach.export.apis.ApiRuler
import org.clulab.reach.export.apis.ApiRuler._

/**
  * Server to implement RESTful Reach API via Akka HTTP service.
  *   Written by: Tom Hicks. 8/17/2017.
  *   Last Modified: Add route to process text in body of a POST.
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
  def contentTypeFor (outputFormat: String): ContentType.NonBinary = outputFormat match {
    case "fries"       => ContentTypes.`application/json`
    case "indexcard"   => ContentTypes.`application/json`
    case "serial-json" => ContentTypes.`application/json`
    case _             => ContentTypes.`text/plain(UTF-8)`
  }

  /** Return the file extension string for the given output format string. */
  def fileExtensionFor (outputFormat: String): String = outputFormat match {
    case "fries"       => "json"
    case "indexcard"   => "json"
    case "serial-json" => "json"
    case _             => "json"
  }

}


/**
  * Provide a Reach API service via Akka HTTP using Json4s support for marshalling.
  */
class ApiService (

  /** Application configuration overridden with command line arguments. */
  appConfig: AkkaServerConfig

) extends ApiImpl {

  val serverConfig = appConfig.config         // final args-merged configuration

  // setup Akka system
  implicit val system: ActorSystem = ActorSystem("apiserver", serverConfig)
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val logger = Logging(system, getClass)

  /** Convenience method to execute a task after a fixed duration of time. */
  def in [U] (duration: FiniteDuration)(body: => U): Unit =
    system.scheduler.scheduleOnce(duration)(body)

  /** Return a timestamped filename from the given filename string. */
  private def makeTimestampedFilename (filename:String, extension:String=".json"): String = {
    def iso8601 = new SimpleDateFormat("yyMMdd-HHmmss")
    def now = iso8601.format(new Date())
    def cleaned = filename.replaceAll("\\W", "_")
    s"${cleaned}_${now}.${extension}"
  }

  /** Parse the response from the internal call and complete the routing. */
  private def makeResponseRoute (
    response: Response,
    outputFormat: String,
    downFilename: Option[String] = None): Route =
  {
    if (hasError(response))
      complete(StatusCodes.InternalServerError, getErrorMessage(response))
    else {
      val contentType = contentTypeFor(outputFormat)
      if (downFilename.isEmpty) {
        complete(HttpResponse(StatusCodes.OK).withEntity(contentType, getResult(response)))
      }
      else {
        val contentDisposition = RawHeader("Content-Disposition",
                                           s"attachment; filename=${downFilename.get};")
        complete(HttpResponse(StatusCodes.OK)
          .withEntity(contentType, getResult(response))
          .withHeaders(contentDisposition))
      }
    }
  }

  /** Upload textual data from the file associated with the 'file' parameter. */
  private def uploadTextFile (outputFormat:String, download:Boolean=false): Route =
    fileUpload("file") {
      case (metaData, fileStream) =>
        val content = fileStream.map(_.utf8String).runWith(Sink.head)
        onSuccess(content) { text =>
          val filename = metaData.getFileName
          val fileExt = filename.substring(filename.lastIndexOf(".") + 1)
          val response = fileExt match {
            case "nxml" | "xml" => doNxml(text, outputFormat)
            case _ => doText(text, outputFormat)
          }
          val downExt = fileExtensionFor(outputFormat)
          val downFilename = if (download) Some(makeTimestampedFilename(filename, downExt)) else None
          makeResponseRoute(response, outputFormat, downFilename)
        }
    }

  /** Create and return the route for this app, using the given configuration, if needed. */
  def makeRoute (config: Config): Route = {
    val appVersion = config.getString("version")
    val static = "org/clulab/reach/export/server/static"
    val uploadTimeout = Duration(config.getString("upload-timeout")).asInstanceOf[FiniteDuration]

    /** Create and return the route for this server. */
    val routesGet = {
      logRequestResult("apiserver") {       // wrap contained paths in logger
        get {                               // GETS
          pathPrefix("api") {
            path("text") {
              parameters("text", 'output ? "fries") { (text, outputFormat) =>
                logger.info(s"GET api/text -> ${text}, ${outputFormat}")
                makeResponseRoute(doText(text, outputFormat), outputFormat)
              }
            }
          } ~
          pathPrefix("images") {                    // images from static subdir
            getFromResourceDirectory(s"${static}/images/")
          } ~
          pathPrefix("js") {                        // javascript from static subdir
            getFromResourceDirectory(s"${static}/javascripts/")
          } ~
          path("") {                                // index page
            getFromResource(s"${static}/api.html")
          } ~
          path("index.html") {                      // index page
            getFromResource(s"${static}/api.html")
          } ~
          path("uploader") {                        // file upload page
            getFromResource(s"${static}/fileUpload.html")
          } ~
          path("application.css") {                 // application stylesheet
            getFromResource(s"${static}/application.css")
          } ~
          // path("images" / Segment) { name =>        // another way to serve images
          //   getFromResource(s"${static}/images/${name}")
          // } ~
          path("version") {                         // show version
            logger.info(s"GET version")
            complete( (s"${appVersion}") )
          }
        }
      }
    }  // end routesGet

    val routesPost = {
      logRequestResult("apiserver") {       // wrap contained paths in logger
        post {                              // POSTS
          pathPrefix("api") {
            path("text") {
              parameters("text", "output" ? "fries") { (text, outputFormat) =>
                logger.info(s"POST api/text -> ${text}, ${outputFormat}")
                makeResponseRoute(doText(text, outputFormat), outputFormat)
              }
            } ~
            path("textBody") {
              formFields("text", "output" ? "fries") { (text, outputFormat) =>
                logger.info(s"POST api/bodyText -> ${text}, ${outputFormat}")
                makeResponseRoute(doText(text, outputFormat), outputFormat)
              }
            } ~
            path("uploadFile") {
              toStrictEntity(uploadTimeout) {
                entity(as[Multipart.FormData]) { formData =>
                  formFields("output" ? "fries", "download" ? "off") { (outputFormat, download) =>
                    logger.info(s"POST api/uploadFile: output=${outputFormat}, download=${download}")
                    uploadTextFile(outputFormat, download == "on")
                  }
                }
              }
            }
          } ~
          path("version") {                         // show version
            logger.info(s"POST version")
            complete( (s"${appVersion}") )
          } ~
          path("shutdown") {                        // shut down the server
            logger.info(s"POST shutdown")
            // complete request and then shut down the server
            complete {
              in (2.seconds) {
                system.terminate()
              }
              "Stopping API server..."
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

  val timeout = serverConfig.getString("akka.http.server.request-timeout")
  val uploadTimeout = serverConfig.getString("upload-timeout")
  logger.info(s"Server timeout: ${timeout}, Upload timeout: ${uploadTimeout}")
  logger.info(s"Server online at http://$host:$port")
}
