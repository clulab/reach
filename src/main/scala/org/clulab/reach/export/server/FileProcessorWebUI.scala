package org.clulab.reach.export.server

import akka.http.scaladsl.Http
import akka.actor.ActorSystem
import akka.http.scaladsl.model.{Multipart, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl._
import akka.stream.{ActorMaterializer, Materializer}
import java.io.{File, FileOutputStream}
import akka.event.{Logging, LoggingAdapter}
import akka.util.ByteString
import com.typesafe.config.{Config, ConfigValueFactory}
import org.clulab.reach.assembly.server._
import org.clulab.reach.mentions._
import org.clulab.reach.mentions.serialization.json._
import org.clulab.reach.PaperReader
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.export.{AssemblyExporter, ExportFilters, Row}
import org.clulab.reach.assembly.sieves.{AssemblySieve, DeduplicationSieves}
import org.json4s.{DefaultFormats, native}
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Success, Failure}


trait FileUpload {

  /**
    * Route for uploading file
    */
  def handleSubmission: Route

  def processFile(tempFile: File, outputType: String): String

  implicit val serialization = native.Serialization
  implicit val formats = DefaultFormats
  implicit val system: ActorSystem
  implicit def executionContext: ExecutionContextExecutor
  implicit val materializer: Materializer

  def config: Config

  val logger: LoggingAdapter

  val routes = handleSubmission
}


object FileProcessorWebUI extends App with FileUpload {

  val ARIZONA = "arizona"
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
      val sink = FileIO.toFile(tempFile)
      val writeResult = fileStream.runWith(sink)
      onSuccess(writeResult) { result =>
        result.status match {
          case Success(_) =>
            //complete(s"${fileInfo.fileName} successfully saved to ${tempFile.getAbsolutePath}")
            val result = processFile(tempFile, outputFormat)
            logger.info(s"successfully processed ${fileInfo.fileName}")
            //              println(result)
            complete(result)
          case Failure(e) => throw e
        }
      }
  }

  def handleSubmission: Route = {
    // log results
    logRequestResult("reach-exporter") {
      // index page
      path("") {
        getFromResource(s"$static/index.html")
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
        path("process" / "paper" / ARIZONA ) {
          logger.info("received request to process/paper/json")
          (post & entity(as[Multipart.FormData])) { formdata => generateOutput(ARIZONA) }
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
      case ARIZONA =>
        // perform deduplication
        val dedup = new DeduplicationSieves()
        val orderedSieves =
        // track relevant mentions
          AssemblySieve(dedup.trackMentions)
        val am: AssemblyManager = orderedSieves.apply(cms)
        val ae = new AssemblyExporter(am)

        val cols = Seq(
          AssemblyExporter.INPUT,
          AssemblyExporter.OUTPUT,
          AssemblyExporter.CONTROLLER,
          AssemblyExporter.EVENT_ID,
          AssemblyExporter.EVENT_LABEL,
          AssemblyExporter.NEGATED,
          AssemblyExporter.INDIRECT,
          // context
          AssemblyExporter.CONTEXT_SPECIES,
          AssemblyExporter.CONTEXT_ORGAN,
          AssemblyExporter.CONTEXT_CELL_LINE,
          AssemblyExporter.CONTEXT_CELL_TYPE,
          AssemblyExporter.CONTEXT_CELLULAR_COMPONENT,
          AssemblyExporter.CONTEXT_TISSUE_TYPE,
          // evidence
          AssemblyExporter.SEEN,
          AssemblyExporter.EVIDENCE,
          AssemblyExporter.SEEN_IN
        )
        def arizonaFilter(rows: Set[Row]): Set[Row] = rows.filter { r =>
          // remove unseen
          (r.seen > 0) &&
            // keep only the events
            ExportFilters.isEvent(r)
        }
        ae.rowsToString(cols, AssemblyExporter.SEP, arizonaFilter)

      case JSON =>
        cms.json(false)
    }
  }

  val argMap = buildArgMap(ServerConfig.defaults, args.toList)

  val p: Int = argMap(ServerConfig.port).toInt
  val h: String = argMap(ServerConfig.host)

  // Update config with values from command line
  val config = ServerConfig.defaultConfig
    .withValue(ServerConfig.defaultHostName, ConfigValueFactory.fromAnyRef(h))
    .withValue(ServerConfig.defaultPort, ConfigValueFactory.fromAnyRef(p))


  override implicit val system: ActorSystem = ActorSystem("reach-export", config)
  override implicit val executionContext = system.dispatcher
  override implicit val materializer = ActorMaterializer()
  override val logger = Logging(system, getClass)

  val bindingFuture =  Http().bindAndHandle(handler = routes, interface = h, port = p)

  logger.info(s"Server online at http://$h:$p")

}
