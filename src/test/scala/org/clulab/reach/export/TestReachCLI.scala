package org.clulab.reach.export

import java.io.File
import org.clulab.reach.ReachCLI
import org.clulab.reach.TestUtils._
import org.clulab.utils.Files
import org.scalatest.{FlatSpec, Matchers}


/**
  * Tests the functionality of ReachCLI on the NXML papers in src/test/resources/inputs/nxml
  * User: mihais
  * Date: 12/4/15
  * Last Modified: GHP rewrite for consolidated ReachCLI, cleanups, and changed config file.
  */
class TestReachCLI extends FlatSpec with Matchers {
  val nxmlDir = readResourceAsFile("inputs/test-nxml")

  val nThreads = Some(2)

  // FRIES no assembly
  lazy val tmpFriesDir = Files.mkTmpDir("tmpFries", deleteOnExit = true)
  lazy val friesDir = new File(tmpFriesDir)
  lazy val friesLogFile = new File(tmpFriesDir + File.separator + "log.txt")

  // FRIES + assembly
  lazy val tmpFriesWithAssemblyDir = Files.mkTmpDir("tmpWithAssemblyFries", deleteOnExit = true)
  lazy val friesWithAssemblyDir = new File(tmpFriesWithAssemblyDir)
  lazy val friesWithAssemblyLogFile = new File(tmpFriesWithAssemblyDir + File.separator + "log.txt")

  // Index Cards
  lazy val tmpICDir = Files.mkTmpDir("tmpIC", deleteOnExit = true)
  lazy val icDir = new File(tmpICDir)
  lazy val icLogFile = new File(tmpICDir + File.separator + "log.txt")

  // Text
  lazy val tmpTxtDir = Files.mkTmpDir("tmpTxt", deleteOnExit = true)
  lazy val txtDir = new File(tmpTxtDir)
  lazy val txtLogFile = new File(tmpTxtDir + File.separator + "log.txt")


  "ReachCLI" should "output TEXT correctly on NXML papers" in {
    println(s"Will output TEXT output in directory ${txtDir.getAbsolutePath}")
    val cli = new ReachCLI(papersDir = nxmlDir, outputDir = txtDir, outputFormat = "text", logFile = txtLogFile)
    val errorCount = cli.processPapers(threadLimit = nThreads, withAssembly = false)
    if(errorCount > 0) dumpLog(txtLogFile)
    errorCount should be (0)
  }

  it should "output FRIES correctly on NXML papers without assembly" in {
    println(s"Will output FRIES output in directory ${friesDir.getAbsolutePath}")
    val cli = new ReachCLI(papersDir = nxmlDir, outputDir = friesDir, outputFormat = "fries", logFile = friesLogFile)
    val errorCount = cli.processPapers(threadLimit = nThreads, withAssembly = false)
    if(errorCount > 0) dumpLog(friesLogFile)
    errorCount should be (0)
  }

  it should "output FRIES correctly on NXML papers with Assembly" in {
    println(s"Will output FRIES with Assembly output in directory ${friesWithAssemblyDir.getAbsolutePath}")
    val cli = new ReachCLI(papersDir = nxmlDir, outputDir = friesWithAssemblyDir, outputFormat = "fries", logFile = friesWithAssemblyLogFile)
    val errorCount = cli.processPapers(threadLimit = nThreads, withAssembly = true)
    if(errorCount > 0) dumpLog(friesWithAssemblyLogFile)
    errorCount should be (0)
  }

  it should "output IndexCard correctly on NXML papers" in {
    println(s"Will output IndexCard output in directory ${icDir.getAbsolutePath}")
    val cli = new ReachCLI(papersDir = nxmlDir, outputDir = icDir, outputFormat = "indexcard", logFile = icLogFile)
    val errorCount = cli.processPapers(threadLimit = nThreads, withAssembly = false)
    if(errorCount > 0) dumpLog(icLogFile)
    errorCount should be (0)
  }

  // TODO: Add test to ensure correct handling of csv and tsv files

  def dumpLog(logFile:File): Unit = {
    println("LOG FILE:")
    println(io.Source.fromFile(logFile).mkString)
  }
}
