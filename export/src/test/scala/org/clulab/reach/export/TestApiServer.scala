package org.clulab.reach.export.server

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source

import com.typesafe.config.{ Config, ConfigFactory }

import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.{ Formats, DefaultFormats, jackson, native }

import akka.event.Logging
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport._
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
  *   Last Modified: Get the GET text test working. Begin POST tests (not working).
  */
class TestApiServer extends WordSpec
    with Matchers
    with ScalatestRouteTest
    with Json4sSupport
{

  implicit val serialization = jackson.Serialization // or native.Serialization
  implicit val formats = DefaultFormats

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

    "return correct JSON version string" in {
      Get("/version") ~> route ~> check {
        status should equal(StatusCodes.OK)
        val resp = responseAs[Map[String,String]]
        (resp.get("version")) should equal (Some(version))
      }
    }

    // GETs

    "GET text" in {
      Get("/api/text?text=ZZZ4%20phosphorylates%20ATM%20") ~> route ~> check {
        status should equal(StatusCodes.OK)
        val resp = responseAs[HttpResponse]
        (resp) should not be (null)
        val entity = resp.entity
        // logger.info(s"resp.entity=${entity}") // DEBUGGING
        (resp.entity) should not be (null)
        val entLen = entity.getContentLengthOption.orElse(-1L)
        // logger.info(s"entity.length=${entLen}") // DEBUGGING
        (entLen > 200) should be (true)
      }
    }

    "GET HTML file" in {
      Get("/") ~> route ~> check {
        // logger.info(s"response=${response}") // DEBUGGING
        mediaType should equal(MediaTypes.`text/html`)
      }
    }

    "GET HTML index file" in {
      Get("/index.html") ~> route ~> check {
        // logger.info(s"response=${response}") // DEBUGGING
        mediaType should equal(MediaTypes.`text/html`)
      }
    }

    "GET CSS file" in {
      Get("/application.css") ~> route ~> check {
        // logger.info(s"response=${response}") // DEBUGGING
        status should equal(StatusCodes.OK)
        mediaType should equal(MediaTypes.`text/css`)
      }
    }

    // SHOULD WORK BUT DOES NOT:
    // "GET CSS file from static directory" in {
    //   Get("/static/application.css") ~> route ~> check {
    //     // logger.info(s"response=${response}") // DEBUGGING
    //     status should equal(StatusCodes.OK)
    //     mediaType should equal(MediaTypes.`text/css`)
    //   }
    // }


    // POSTs

    "POST text" in {
      Post("/api/text",
        HttpEntity(ContentTypes.`application/json`, """{ "text": "akt1 phosphorylates XXX" }""")) ~> route ~> check
        {
          status should equal(StatusCodes.OK)
          val resp = responseAs[String]
          // logger.info(s"resp=${resp}")        // DEBUGGING
          (resp) should not be (empty)
        }
    }

    "POST nxml" in {
      Post("/api/nxml",
        HttpEntity(ContentTypes.`application/json`, """{ "nxml": "Xyz1224" }""")) ~> route ~> check
        {
          status should equal(StatusCodes.OK)
          val resp = responseAs[String]
          // logger.info(s"resp=${resp}")        // DEBUGGING
          (resp) should not be (empty)
        }
    }

  }
}
