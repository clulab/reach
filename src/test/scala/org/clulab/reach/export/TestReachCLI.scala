package org.clulab.reach.export

import java.io.File
import org.clulab.reach.TestUtils._
import org.clulab.utils.Files
import org.scalatest.{ FlatSpec, Matchers }

import org.clulab.reach.AnnotationsCLI

/**
  * Tests the functionality of ReachCLI on the NXML papers in src/test/resources/inputs/nxml
  * Author: mihais and hickst
  * Date: 12/4/15
  * Last Modified: Removed tests involving assembly.
  */
class TestReachCLI extends FlatSpec with Matchers {
  val nxmlDir = readResourceAsFile("inputs/test-nxml")
  val nThreads = Some(2)

  // Text + FRIES
  lazy val comboDir: File = {
    val tmpComboDir = Files.mkTmpDir("tmpCombo", deleteOnExit = true)
    new File(tmpComboDir)
  }

  // Index Cards
  lazy val tmpICDir = Files.mkTmpDir("tmpIC", deleteOnExit = true)
  lazy val icDir = new File(tmpICDir)

  // Tests usage of multiple output types
  "ReachCLI" should "output TEXT and FRIES correctly on NXML papers without assembly" in {
    println(s"Will output TEXT and FRIES output in directory ${comboDir.getAbsolutePath}")
    val cli = new AnnotationsCLI(papersDir = nxmlDir, outputDir = comboDir, outputFormats = Seq("text", "fries"))
    val errorCount = cli.preAnnotatePapers(threadLimit = nThreads, withAssembly = false)
    errorCount should be (0)
  }

//  it should "output IndexCard correctly on NXML papers" in {
//    println(s"Will output IndexCard output in directory ${icDir.getAbsolutePath}")
//    val cli = new ReachCLI(papersDir = nxmlDir, outputDir = icDir, outputFormat = "indexcard")
//    val errorCount = cli.processPapers(threadLimit = nThreads)
//    errorCount should be (0)
//  }

}
