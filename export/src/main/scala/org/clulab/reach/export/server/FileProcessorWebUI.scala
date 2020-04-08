package org.clulab.reach.export.server

import java.io.{File, FileOutputStream}
import java.nio.file.Path
import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

import com.typesafe.config.{Config, ConfigValueFactory}
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Multipart
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.scaladsl._
import akka.util.ByteString

import org.clulab.reach.mentions._
import org.clulab.reach.mentions.serialization.json._
import org.clulab.reach.PaperReader


trait FileUpload {
  def processFile(tempFile: File, outputType: String): String

  implicit val serialization = Serialization
  implicit val formats = DefaultFormats
  implicit val system: ActorSystem
  implicit def executionContext: ExecutionContextExecutor
  implicit val materializer: Materializer

  val logger: LoggingAdapter
}


object FileProcessorWebUI extends App with FileUpload {
  val JSON = "json"

  // form elements
  val PAPER = "paper-upload"
  val OUTPUT_TYPE = "output-type"

  val static = "org/clulab/reach/export/server/static"

  private def processFile(tempFile: File, fileData: Multipart.FormData) = {
    val fileOutput = new FileOutputStream(tempFile)
    fileData.parts.mapAsync(1) { bodyPart ⇒
      def writeFileOnLocal(array: Array[Byte], byteString: ByteString): Array[Byte] = {
        val byteArray: Array[Byte] = byteString.toArray
        fileOutput.write(byteArray)
        array ++ byteArray
      }
      bodyPart.entity.dataBytes.runFold(Array[Byte]())(writeFileOnLocal)
    }.runFold(0)(_ + _.length)
  }

  def generateOutput(outputFormat: String): Route = fileUpload(PAPER) {
    case (fileInfo, fileStream) =>
      logger.info(s"File is ${fileInfo.fileName}")
      logger.info(s"Output type is $outputFormat")
      val temp = System.getProperty("java.io.tmpdir")
      val tempFile = new File(temp, fileInfo.fileName)
      val sink = FileIO.toPath(tempFile.toPath())
      val writeResult = fileStream.runWith(sink)
      onSuccess(writeResult) { result =>
        result.status match {
          case Success(_) =>
            //complete(s"${fileInfo.fileName} successfully saved to ${tempFile.getAbsolutePath}")
            val result = processFile(tempFile, outputFormat)
            logger.info(s"successfully processed ${fileInfo.fileName}")
            complete(result)
          case Failure(e) => throw e
        }
      }
  }

  def handleSubmission: Route = {
    logRequestResult("reach-exporter") {    // wrapper to log all results
      path("") {                            // index page
        getFromResource(s"$static/fileprocessorwebui.html")
      } ~
      path("process" / "paper") {
        (post & entity(as[Multipart.FormData])) { formdata =>
          // FIXME: how do I retrieve the value of the "output-type" field?
          val outputFormat: String = ???
          logger.info(s"OUTPUT_TYPE: $outputFormat")
          generateOutput(outputFormat)
          // HttpResponse(status = StatusCodes.InternalServerError, entity = "<h2>Failed to process your file</h2>")
        }
      } ~
      path("process" / "paper" / JSON ) {
        logger.info("received request to process/paper/json")
        (post & entity(as[Multipart.FormData])) { formdata => generateOutput(JSON) }
      }
    }
  }

  def processFile(tempFile: File, outputType: String): String = {
    val cms = PaperReader.getMentionsFromPaper(tempFile).map(_.toCorefMention)
    outputType match {
      case JSON => cms.json(false)
    }
  }

  val argMap = buildServerArgMap(args.toList)
  val akkaServerConfig = new AkkaServerConfig(argMap, Some("ReachExportServer"))
  val config = akkaServerConfig.config
  val host = akkaServerConfig.host
  val port = akkaServerConfig.port

  override implicit val system: ActorSystem = ActorSystem("reach-export", config)
  override implicit val executionContext = system.dispatcher
  override implicit val materializer = ActorMaterializer()
  override val logger = Logging(system, getClass)

  val bindingFuture = Http().bindAndHandle(handler = handleSubmission, interface = host, port = port)

  logger.info(s"Server online at http://${host}:${port}")
}
