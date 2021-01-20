package org.clulab.reach.`export`

import org.clulab.reach.PaperReader
import org.clulab.reach.ReachCLI
import org.clulab.reach.ReachTest

import java.io.File

class TestCrashes extends ReachTest {
  val papersDir = new File("")
  val outputDir = new File("")
  val outputFormats = Seq.empty[String]
  val reachCLI = new ReachCLI(papersDir, outputDir, outputFormats)

  def runTest(pmcid: String, outputFormat: String): Unit = {
    val filename = s"./src/test/resources/testCrashes/$pmcid.nxml"
    val file = new File(filename)
    val entry = PaperReader.getEntryFromPaper(file)
    val mentions = PaperReader.getMentionsFromEntry(entry)
    val paperId = pmcid
    val startTime = ReachCLI.now
    val withAssembly = true

    reachCLI.outputMentions(mentions, entry, paperId, startTime, outputDir, outputFormat, withAssembly)
  }

  behavior of "fries format"

  it should "not throw an IllegalArgumentException when Controllers of an Activation are not Entities" in {
    val pmcid = "PMC4265014"
    val outputFormat = "fries"

    runTest(pmcid, outputFormat)
  }

  ignore should "not throw a NoSuchElementException when key is not found" in {
    val text = "Activated ANP is a peptide hormone consisting of 28 amino acids that binds to NPR1 , a receptor in target organs such as the kidneys and peripheral blood vessels , converting intracellular GTP into cGMP to promote the excretion of Na  , inhibit Na   reuptake , and induce vasodilation [ 16,17 ] ."
    val pmcid = "PMC6940835"
    val key = "theme"
  }

  behavior of "serial-json format"

  ignore should "not throw a NegativeArraySizeException" in {
    val pmcid = "PMC7176272"
  }

  behavior of "reading"

  ignore should "not throw an InvocationTargetException" in {
    val pmcid = "PMC7040422"

  }

  ignore should "not throw a NoSuchElementException" in {
    val key1 = "controlled"
    val pmcid1 = "PMC5504966"
    val text1 = "Bacteria in the human gut can produce hydrogen gas , and hydrogen can be converted to methane in the gut by methane producing bacteria [ 15 ] ."

    val key2 = "controller"
    val pmcid2 = "PMC5809884"
    val text2 = "( 2 ) Noise exposure led to enhanced JNK phosphorylation and IRS1 serine phosphorylation as well as reduced Akt phosphorylation in skeletal muscles in response to exogenous insulin stimulation ."

    val key3 = "theme"
    val pmcid3 = "PMC6940835"
    val text3 = "Activated ANP is a peptide hormone consisting of 28 amino acids that binds to NPR1 , a receptor in target organs such as the kidneys and peripheral blood vessels , converting intracellular GTP into cGMP to promote the excretion of Na  , inhibit Na   reuptake , and induce vasodilation [ 16,17 ] ."
  }

  behavior of "indexcard format"

  ignore should "not throw a RuntimeException when argument type not supported" in {
    val typ = "event"
    val pmcid = "PMC3822968"
  }

  ignore should "not throw a RuntimeException when event type conversion not supported" in {
    val typ = "conversion"
    val pmcid = "PMC3822968"
  }

  ignore should "not throw a RuntimeException when unknown event type in event" in {

    def runIndexcardTest(text: String) = runTest(text, "indexcard")

    val text1 = "Disease"
    val pmcid1 = "PMC6539695"
    runIndexcardTest(text1)

    val text2 = "Family"
    val pmcid2 = "PMC5327768"
    runIndexcardTest(text2)

    val text3 = "Gene_or_gene_product"
    val pmcid3 = "PMC5985311"
    runIndexcardTest(text3)

    val text4 = "Simple_chemical"
    val pmcid4 = "PMC6213605"
    runIndexcardTest(text4)
  }

  behavior of "cmu format"

  ignore should "not throw a NoSuchElementException on empty iterator" in {
    val pmcid = "PMC6681624"
  }
}
