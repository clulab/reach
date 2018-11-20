package org.clulab.reach.export

import java.io.File
import org.clulab.reach.TestUtils._
import org.clulab.utils.Files
import org.scalatest.{FlatSpec, Matchers}

import org.clulab.reach.ReachCLI

/**
  * Tests the functionality of ReachCLI on the NXML papers in src/test/resources/inputs/nxml
  * Author: mihais and hickst
  * Date: 12/4/15
  * Last Modified: Move import to highlight oddity that test is in a different package tested class.
  */
class TestReachCLI extends FlatSpec with Matchers {
  val nxmlDir = readResourceAsFile("inputs/test-nxml")
  val nThreads = Some(2)

  // Text + FRIES no assembly
  lazy val comboDir: File = {
    val tmpComboDir = Files.mkTmpDir("tmpCombo", deleteOnExit = true)
    new File(tmpComboDir)
  }

  // FRIES + assembly
  lazy val tmpFriesWithAssemblyDir = Files.mkTmpDir("tmpWithAssemblyFries", deleteOnExit = true)
  lazy val friesWithAssemblyDir = new File(tmpFriesWithAssemblyDir)

  // Index Cards
  lazy val tmpICDir = Files.mkTmpDir("tmpIC", deleteOnExit = true)
  lazy val icDir = new File(tmpICDir)

  // Tests usage of multiple output types
  "ReachCLI" should "output TEXT and FRIES correctly on NXML papers without assembly" in {
    println(s"Will output TEXT and FRIES output in directory ${comboDir.getAbsolutePath}")
    val cli = new ReachCLI(papersDir = nxmlDir, outputDir = comboDir, outputFormats = Seq("text", "fries"))
    val errorCount = cli.processPapers(threadLimit = nThreads, withAssembly = false)
    errorCount should be (0)
  }

  // Tests usage of a single output type
  ignore should "output FRIES correctly on NXML papers with Assembly" in {
    println(s"Will output FRIES with Assembly output in directory ${friesWithAssemblyDir.getAbsolutePath}")
    val cli = ReachCLI(papersDir = nxmlDir, outputDir = friesWithAssemblyDir, outputFormat = "fries")
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
