package org.clulab.reach.`export`

import org.clulab.reach.PaperReader
import org.clulab.reach.ReachCLI
import org.clulab.reach.ReachTest

import java.io.File

class TestCrashes extends ReachTest {

  val short: ItWord = it
  val long: IgnoreWord = ignore
  val infinite: IgnoreWord = ignore

  val withAssembly = true
  val outputDirname: String = System.getProperty("java.io.tmpdir") // ./tmpTest"
  val reachCLI: ReachCLI = {
    val papersDir = new File("")
    val outputDir = new File(outputDirname)
    val outputFormats = Seq.empty[String]

    new ReachCLI(papersDir, outputDir, outputFormats)
  }

  def getOutputFilenames(pmcid: String, outputFormat: String): Seq[String] = {
    outputFormat match {
      case "fries" => Seq(s"$pmcid.uaz.entities.json", s"$pmcid.uaz.events.json", s"$pmcid.uaz.sentences.json")
      case "serial-json" => Seq.empty
      case "indexcard" => Seq(pmcid)
      case "cmu" => Seq(s"$pmcid-cmu-out.tsv")
    }
  }

  def mkFilename(pmcid: String): String = s"./src/test/resources/testCrashes/$pmcid.nxml"

  def runTest(pmcid: String, outputFormat: String): Unit = {
    val filename = mkFilename(pmcid)
    val file = new File(filename)
    val entry = PaperReader.getEntryFromPaper(file)
    val mentions = PaperReader.getMentionsFromEntry(entry)
    val paperId = pmcid
    val startTime = ReachCLI.now

    try {
      reachCLI.outputMentions(mentions, entry, paperId, startTime, reachCLI.outputDir, outputFormat, withAssembly)
    }
    finally {
      // Try to clean up after yourself.
      val outputFilenames = getOutputFilenames(pmcid, outputFormat)
      outputFilenames.foreach { filename =>
        val pathname = s"$outputDirname/$filename"
        val file = new File(pathname)
        if (file.isDirectory)
          file.listFiles().map(_.delete)
        file.delete()
      }
    }
  }

  def runTest(pmcid: String): Unit = {
    val filename = mkFilename(pmcid)
    val file = new File(filename)

    reachCLI.processPaper(file, withAssembly)
  }

  {
    val outputFormat = "fries"
    def test(pmcid: String): Unit = runTest(pmcid, outputFormat)

    behavior of "fries format"

    long should "not throw an IllegalArgumentException when Controllers of an Activation are not Entities" in {
      val pmcid = "PMC4265014"
      test(pmcid)
    }

    short should "not throw a NoSuchElementException when key is not found" in {
      val pmcid = "ShortPMC6940835"
      test(pmcid)
    }
  }

  {
    val outputFormat = "serial-json"
    def test(pmcid: String): Unit = runTest(pmcid, outputFormat)

    behavior of "serial-json format"

    infinite should "not throw a NegativeArraySizeException" in {
      val pmcid = "PMC7176272"
      test(pmcid)
    }
  }

  {
    val outputFormat = "indexcard"
    def test(pmcid: String): Unit = runTest(pmcid, outputFormat)

    behavior of "indexcard format"

    short should "not throw a RuntimeException when argument type 'event' not supported" in {
      val pmcid = "ShortPMC3822968"
      test(pmcid)
    }

    short should "not throw a RuntimeException when unknown event type 'Disease' in event" in {
      val pmcid1 = "ShortPMC6539695"
      test(pmcid1)
    }

    short should "not throw a RuntimeException when unknown event type 'Family' in event" in {
      val pmcid2 = "ShortPMC5327768"
      test(pmcid2)
    }

    short should "not throw a RuntimeException when unknown event type 'Gene_or_gene_product' in event" in {
      val pmcid3 = "ShortPMC5985311"
      test(pmcid3)
    }

    short should "not throw a RuntimeException when unknown event type 'Simple_chemical' in event" in {
      val pmcid4 = "ShortPMC6213605"
      test(pmcid4)
    }
  }

  {
    val outputFormat = "cmu"
    def test(pmcid: String): Unit = runTest(pmcid, outputFormat)

    behavior of "cmu format"

    short should "not throw a NoSuchElementException on empty iterator" in {
      val pmcid = "ShortPMC6681624"
      test(pmcid)
    }
  }

  {
    def test(pmcid: String): Unit = runTest(pmcid)

    behavior of "reading"

    short should "not throw an InvocationTargetException" in {
      val pmcid = "ShortPMC7040422"
      test(pmcid)
    }

    short should "not throw a NoSuchElementException for 'one'" in {
      val pmcid1 = "ShortPMC5504966"
      test(pmcid1)
    }

    short should "not throw a NoSuchElementException for 'controller'" in {
      val pmcid1 = "ShortPMC5809884"
      test(pmcid1)
    }
  }
}
