package org.clulab.reach.export

import java.io.File
import org.clulab.reach.ReachCLI
import org.clulab.reach.TestUtils._
import org.clulab.utils.Files
import org.scalatest.{FlatSpec, Matchers}

/**
  * Tests the functionality of ReachCLI on the NXML papers in src/test/resources/inputs/nxml
  * Author: mihais and hickst
  * Date: 12/4/15
  * Last Modified: Update for past breaking logging changes: obsoleting of log file.
  */
class TestReachCLI extends FlatSpec with Matchers {
  val nxmlDir = readResourceAsFile("inputs/test-nxml")
  val nThreads = Some(2)

  // FRIES no assembly
  lazy val tmpFriesDir = Files.mkTmpDir("tmpFries", deleteOnExit = true)
  lazy val friesDir = new File(tmpFriesDir)

  // FRIES + assembly
  lazy val tmpFriesWithAssemblyDir = Files.mkTmpDir("tmpWithAssemblyFries", deleteOnExit = true)
  lazy val friesWithAssemblyDir = new File(tmpFriesWithAssemblyDir)

  // Index Cards
  lazy val tmpICDir = Files.mkTmpDir("tmpIC", deleteOnExit = true)
  lazy val icDir = new File(tmpICDir)

  // Text
  lazy val tmpTxtDir = Files.mkTmpDir("tmpTxt", deleteOnExit = true)
  lazy val txtDir = new File(tmpTxtDir)


  "ReachCLI" should "output TEXT correctly on NXML papers" in {
    println(s"Will output TEXT output in directory ${txtDir.getAbsolutePath}")
    val cli = new ReachCLI(papersDir = nxmlDir, outputDir = txtDir, outputFormat = "text")
    val errorCount = cli.processPapers(threadLimit = nThreads, withAssembly = false)
    errorCount should be (0)
  }

  it should "output FRIES correctly on NXML papers without assembly" in {
    println(s"Will output FRIES output in directory ${friesDir.getAbsolutePath}")
    val cli = new ReachCLI(papersDir = nxmlDir, outputDir = friesDir, outputFormat = "fries")
    val errorCount = cli.processPapers(threadLimit = nThreads, withAssembly = false)
    errorCount should be (0)
  }

  it should "output FRIES correctly on NXML papers with Assembly" in {
    println(s"Will output FRIES with Assembly output in directory ${friesWithAssemblyDir.getAbsolutePath}")
    val cli = new ReachCLI(papersDir = nxmlDir, outputDir = friesWithAssemblyDir, outputFormat = "fries")
    val errorCount = cli.processPapers(threadLimit = nThreads, withAssembly = true)
    errorCount should be (0)
  }

//  it should "output IndexCard correctly on NXML papers" in {
//    println(s"Will output IndexCard output in directory ${icDir.getAbsolutePath}")
//    val cli = new ReachCLI(papersDir = nxmlDir, outputDir = icDir, outputFormat = "indexcard")
//    val errorCount = cli.processPapers(threadLimit = nThreads, withAssembly = false)
//    errorCount should be (0)
//  }

}
