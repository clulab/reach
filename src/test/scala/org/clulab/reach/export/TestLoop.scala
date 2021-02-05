package org.clulab.reach.`export`

import org.clulab.odin.Mention
import org.clulab.odin.EventMention
import org.clulab.odin.RelationMention
import org.clulab.odin.TextBoundMention
import org.clulab.reach.PaperReader
import org.clulab.reach.ReachCLI
import org.clulab.reach.ReachTest
import org.clulab.reach.mentions.Anaphoric
import org.clulab.reach.utils.LoopChecker
import org.clulab.reach.mentions._

import java.io.File

class TestLoop extends ReachTest {

  class MentionLoopChecker() extends LoopChecker[Mention] {

    protected def getAntecendents: Seq[Mention] = {
      Seq.empty
    }

    override def getChildren(mention: Mention): Seq[Mention] = {
      val antecedentMentions = Seq(mention).collect {
        case mention: Anaphoric =>
          mention.antecedents.toSeq.collect {
            case mention: Mention => mention
          }: Seq[Mention]
      }.flatten
      val mutantMentions = Seq(mention).collect {
        case mention: BioMention =>
          mention.mutants.map(_.evidence)
      }.flatten
      val childMentions = mention match {
        case _: TextBoundMention => Seq.empty
        case mention: EventMention => mention.trigger +: mention.arguments.values.flatten.toSeq
        case mention: RelationMention => mention.arguments.values.flatten.toSeq
        case _: Mention => throw new RuntimeException(s"Unknown mention type: ${mention.getClass.getName}")
      }
      val corefAntecedentMentions = antecedentMentions.map(_.toCorefMention)
      val corefMutantMentions = mutantMentions.map(_.toCorefMention)
      val corefChildMentions = childMentions.map(_.toCorefMention)

      corefAntecedentMentions ++ corefMutantMentions ++ corefChildMentions
    }
  }

  val infinite: ItWord = it

  val withAssembly = true
  val outputDirname: String = System.getProperty("java.io.tmpdir") // ./tmpTest"
  val reachCLI: ReachCLI = {
    val papersDir = new File("")
    val outputDir = new File(outputDirname)
    val outputFormats = Seq.empty[String]

    new ReachCLI(papersDir, outputDir, outputFormats)
  }
  val loopChecker = new MentionLoopChecker()

  def getOutputFilenames(pmcid: String, outputFormat: String): Seq[String] = {
    outputFormat match {
      // Real test
      case "serial-json" => Seq.empty
      // Short test
      case "indexcard" => Seq.empty
    }
  }

  def mkFilename(pmcid: String): String = s"./src/test/resources/testCrashes/$pmcid.nxml"

  def checkForLoops(outputFormat: String, mentions: Seq[Mention]): Boolean = {
    val loopMentions = outputFormat match {
      case "serial-json" => mentions.map(_.toCorefMention)
      case _ => mentions
    }
    loopChecker.checkForLoops(loopMentions)
  }

  def runTest(pmcid: String, outputFormat: String): Boolean = {
    val filename = mkFilename(pmcid)
    val file = new File(filename)
    val entry = PaperReader.getEntryFromPaper(file)
    val mentions = PaperReader.getMentionsFromEntry(entry)
    val paperId = pmcid
    val startTime = ReachCLI.now

    try {
      val hasLoops = checkForLoops(outputFormat, mentions)
      if (!hasLoops)
        reachCLI.outputMentions(mentions, entry, paperId, startTime, reachCLI.outputDir, outputFormat, withAssembly)
      hasLoops
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

  {
    // Real test
    val outputFormat = "serial-json"
    // Short test
    // val outputFormat = "indexcard"
    def test(pmcid: String): Boolean = runTest(pmcid, outputFormat)

    behavior of "serial-json format"

    infinite should "not have loops" in {
      // Real test
      val pmcid = "PMC7176272"
      // Short test
      // val pmcid = "ShortPMC3822968"
      test(pmcid) should be (false)
    }
  }
}
