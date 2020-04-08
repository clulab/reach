package org.clulab.reach.export.server

import scala.concurrent.{ ExecutionContextExecutor, Future }
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.io.Source

import com.typesafe.config.{ Config, ConfigFactory }

import akka.event.Logging
// import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest

import org.scalatest.{ Matchers, WordSpec }

import org.clulab.reach.export.server._
import org.clulab.reach.export.server.ApiServer._

/**
  * Unit tests of the API service class.
  *   Written by: Tom Hicks. 8/17/2017.
  *   Last Modified: Add tests for new text body call.
  */
class TestApiServer extends WordSpec
    with Matchers
    with ScalatestRouteTest
{

  implicit val executionContext = system.dispatcher
  val logger = Logging(system, getClass)

  val argMap = Map[String,String]()
  val appConfig = new AkkaServerConfig(argMap, Some("ApiServer"))
  val serverConfig = appConfig.config

  val apiService = new ApiService(appConfig)
  val route = apiService.makeRoute(serverConfig) // create the service route to test
  val version = serverConfig.getString("version")

  val nxmlIn = Source.fromURL(getClass.getResource("/inputs/nxml/PMCfake.nxml")).mkString

  "The class under test" should {

    // GETs

    "return correct JSON version string" in {
      Get("/version") ~> route ~> check {
        status should equal(StatusCodes.OK)
        val resp = responseAs[HttpResponse]
        (resp) should not be (null)
        // logger.info(s"resp.entity=${resp.entity}") // DEBUGGING
        (resp.entity) should not be (null)
        contentType should equal(ContentTypes.`text/plain(UTF-8)`)
        val vers:String = resp.entity.asInstanceOf[HttpEntity.Strict].data.utf8String
        // logger.info(s"vers=${vers}")        // DEBUGGING
        (vers) should equal (version)
      }
    }

    "GET text, default output" in {
      Get("/api/text?text=ZZZ4%20phosphorylates%20ATM%20") ~> route ~> check {
        status should equal(StatusCodes.OK)
        val resp = responseAs[HttpResponse]
        (resp) should not be (null)
        val entity = resp.entity
        // logger.info(s"resp.entity=${entity}") // DEBUGGING
        (resp.entity) should not be (null)
        contentType should equal(ContentTypes.`application/json`)
        val entLen = entity.getContentLengthOption.orElse(-1L)
        // logger.info(s"entity.length=${entLen}") // DEBUGGING
        (entLen > 200) should be (true)
      }
    }

    "GET HTML file" in {
      Get("/") ~> route ~> check {
        status should equal(StatusCodes.OK)
        val resp = responseAs[HttpResponse]
        (resp) should not be (null)
        // logger.info(s"response=${response}") // DEBUGGING
        mediaType should equal(MediaTypes.`text/html`)
      }
    }

    "GET HTML index file" in {
      Get("/index.html") ~> route ~> check {
        status should equal(StatusCodes.OK)
        val resp = responseAs[HttpResponse]
        (resp) should not be (null)
        // logger.info(s"response=${response}") // DEBUGGING
        mediaType should equal(MediaTypes.`text/html`)
      }
    }

    "GET CSS file" in {
      Get("/application.css") ~> route ~> check {
        status should equal(StatusCodes.OK)
        val resp = responseAs[HttpResponse]
        (resp) should not be (null)
        // logger.info(s"response=${response}") // DEBUGGING
        status should equal(StatusCodes.OK)
        mediaType should equal(MediaTypes.`text/css`)
      }
    }

    "GET image file from static subdirectory" in {
      Get("/images/spinner66.gif") ~> route ~> check {
        // logger.info(s"response=${response}") // DEBUGGING
        status should equal(StatusCodes.OK)
        mediaType should equal(MediaTypes.`image/gif`)
      }
    }


    // POSTs

    "POST text, default output" in {
      Post("/api/text?text=akt1%20dephosphorylates%20mek1") ~> route ~> check {
        status should equal(StatusCodes.OK)
        val resp = responseAs[HttpResponse]
        (resp) should not be (null)
        val entity = resp.entity
        // logger.info(s"resp.entity=${entity}") // DEBUGGING
        (resp.entity) should not be (null)
        contentType should equal(ContentTypes.`application/json`)
        val entLen = entity.getContentLengthOption.orElse(-1L)
        // logger.info(s"entity.length=${entLen}") // DEBUGGING
        (entLen > 200) should be (true)
      }
    }

    "POST text, args in body, default output" in {
      Post("/api/textBody",
        FormData(
          "text" -> "The AICAR molecule increases TBC1D1 phosphorylation.")) ~> route ~> check
      {
        status should equal(StatusCodes.OK)
        val resp = responseAs[HttpResponse]
        (resp) should not be (null)
        val entity = resp.entity
        // logger.info(s"resp.entity=${entity}") // DEBUGGING
        (resp.entity) should not be (null)
        contentType should equal(ContentTypes.`application/json`)
        val entLen = entity.getContentLengthOption.orElse(-1L)
        // logger.info(s"entity.length=${entLen}") // DEBUGGING
        (entLen > 200) should be (true)
      }
    }

    "POST upload text, default output" in {
      val mpForm = Multipart.FormData(
        Multipart.FormData.BodyPart.Strict(
          "file",
          HttpEntity(ContentTypes.`text/plain(UTF-8)`, "akt1 ubiquitinates mek1"),
          Map("filename" -> "test.txt")))

      Post("/api/uploadFile", mpForm) ~> route ~> check {
        status should equal(StatusCodes.OK)
        val resp = responseAs[HttpResponse]
        (resp) should not be (null)
        val entity = resp.entity
        // logger.info(s"resp.entity=${entity}") // DEBUGGING
        (resp.entity) should not be (null)
        contentType should equal(ContentTypes.`application/json`)
        val entLen = entity.getContentLengthOption.orElse(-1L)
        // logger.info(s"entity.length=${entLen}") // DEBUGGING
        (entLen > 200) should be (true)
      }
    }

    "POST upload nxml, default output" in {
      val mpForm = Multipart.FormData(
        Multipart.FormData.BodyPart.Strict(
          "file",
          HttpEntity(ContentTypes.`text/plain(UTF-8)`, nxmlIn),
          Map("filename" -> "test.nxml")))

      Post("/api/uploadFile", mpForm) ~> route ~> check {
        val resp = responseAs[HttpResponse]
        (resp) should not be (null)
        val entity = resp.entity
        // logger.info(s"resp.entity=${entity}") // DEBUGGING
        (resp.entity) should not be (null)
        contentType should equal(ContentTypes.`application/json`)
        val entLen = entity.getContentLengthOption.orElse(-1L)
        // logger.info(s"entity.length=${entLen}") // DEBUGGING
        (entLen > 200) should be (true)
      }
    }

    "POST shutdown" in {
      Post("/shutdown") ~> route ~> check {
        status should equal(StatusCodes.OK)
        val resp = responseAs[String]
        (resp) should not be (null)
        (resp) should be ("Stopping API server...")
      }
    }

  }
}
