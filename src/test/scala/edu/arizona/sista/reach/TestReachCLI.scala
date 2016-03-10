package edu.arizona.sista.reach

import java.io.File

import edu.arizona.sista.utils.Files
import edu.arizona.sista.reach.context.ContextEngineFactory.Engine
import edu.arizona.sista.reach.context.ContextEngineFactory.Engine._
import org.scalatest.{Matchers, FlatSpec}

/**
  * Tests the functionality of ReachCLI on the NXML papers in src/test/resources/inputs/nxml
  * User: mihais
  * Date: 12/4/15
  * Last Modified: Change test input directory.
  */
class TestReachCLI extends FlatSpec with Matchers {
  val nxmlDir = new File("src/test/resources/inputs/test-nxml")

  lazy val tmpFriesDir = Files.mkTmpDir("tmpFries", deleteOnExit = true)
  lazy val friesDir = new File(tmpFriesDir)
  lazy val friesLogFile = new File(tmpFriesDir + File.separator + "log.txt")

  lazy val tmpICDir = Files.mkTmpDir("tmpIC", deleteOnExit = true)
  lazy val icDir = new File(tmpICDir)
  lazy val icLogFile = new File(tmpICDir + File.separator + "log.txt")

  lazy val tmpTxtDir = Files.mkTmpDir("tmpTxt", deleteOnExit = true)
  lazy val txtDir = new File(tmpTxtDir)
  lazy val txtLogFile = new File(tmpTxtDir + File.separator + "log.txt")

  val encoding = "utf-8"
  val ignoreSections = List("references", "materials", "materials|methods", "methods", "supplementary-material")

  val contextEngineType:Engine = Engine.withName("Policy4")
  val contextEngineParams:Map[String, String] = Map("bound" -> "3")

  "ReachCLI" should "output TEXT correctly on NXML papers" in {
    println(s"Will output TEXT output in directory ${txtDir.getAbsolutePath}")
    val outputType = "text"
    val cli = new ReachCLI(nxmlDir, txtDir, encoding, outputType, ignoreSections, contextEngineType, contextEngineParams, txtLogFile)
    val errorCount = cli.processPapers(threadLimit = None)
    if(errorCount > 0) dumpLog(friesLogFile)
    errorCount should be (0)
  }

  "ReachCLI" should "output FRIES correctly on NXML papers" in {
    println(s"Will output FRIES output in directory ${friesDir.getAbsolutePath}")
    val outputType = "fries"
    val cli = new ReachCLI(nxmlDir, friesDir, encoding, outputType, ignoreSections, contextEngineType, contextEngineParams, friesLogFile)
    val errorCount = cli.processPapers(threadLimit = None)
    if(errorCount > 0) dumpLog(friesLogFile)
    errorCount should be (0)
  }

  "ReachCLI" should "output IndexCard correctly on NXML papers" in {
    println(s"Will output IndexCard output in directory ${icDir.getAbsolutePath}")
    val outputType = "indexcard"
    val cli = new ReachCLI(nxmlDir, icDir, encoding, outputType, ignoreSections, contextEngineType, contextEngineParams, icLogFile)
    val errorCount = cli.processPapers(threadLimit = None)
    if(errorCount > 0) dumpLog(icLogFile)
    errorCount should be (0)
  }

  def dumpLog(logFile:File): Unit = {
    println("LOG FILE:")
    for(line <- io.Source.fromFile(logFile)) {
      println(line)
    }
  }
}
