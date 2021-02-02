package org.clulab.reach.`export`

import org.clulab.odin.Mention
import org.clulab.reach.PaperReader
import org.clulab.reach.ReachCLI
import org.clulab.reach.ReachTest
import org.clulab.reach.mentions.BioEventMention
import org.clulab.reach.mentions.BioRelationMention
import org.clulab.reach.mentions.BioTextBoundMention

import java.io.File
import java.util.IdentityHashMap

class TestLoop extends ReachTest {

  val infinite = it

  val withAssembly = true
  val outputDirname = System.getProperty("java.io.tmpdir") // ./tmpTest"
  val reachCLI = {
    val papersDir = new File("")
    val outputDir = new File(outputDirname)
    val outputFormats = Seq.empty[String]

    new ReachCLI(papersDir, outputDir, outputFormats)
  }

  def getOutputFilenames(pmcid: String, outputFormat: String): Seq[String] = {
    outputFormat match {
      case "serial-json" => Seq.empty
    }
  }

  def mkFilename(pmcid: String): String = s"./src/test/resources/testCrashes/$pmcid.nxml"

  case class MentionRecord(id: Int, parentIds: Seq[Int] = Seq.empty, childIds: Seq[Int] = Seq.empty) {

    def withParent(parentId: Int): MentionRecord = copy(parentIds = parentIds :+ parentId)

    def withChild(childId: Int): MentionRecord = copy(childIds = childIds :+ childId)
  }

  def getChildren(mention: Mention): Seq[Mention] = {
    mention match {
      case _: BioTextBoundMention => Seq.empty
      case bioMention: BioEventMention => bioMention.trigger +: bioMention.arguments.values.flatten.toSeq
      case bioMention: BioRelationMention => bioMention.arguments.values.flatten.toSeq
      case _: Mention => throw new RuntimeException(s"Unknown mention type: ${mention.getClass.getName}")
    }
  }

  def checkForLoops(mentions: Vector[Mention]): Boolean = {
    val mentionMap = new IdentityHashMap[Mention, MentionRecord]

    def hasLoops(): Boolean = {
      true
    }

    def addMentionAndChildren(parentOpt: Option[Mention], mention: Mention, children: Seq[Mention]): Boolean = {
      val mentionRecordOpt = Option(mentionMap.get(mention))
      val isDuplicate = mentionRecordOpt.isDefined

      val mentionRecord = if (isDuplicate) {
        val mentionRecord = mentionRecordOpt.get
        val parentIdOpt = parentOpt.map { parent => mentionMap.get(parent).id }
        // See if need to add the parent
        if (parentIdOpt.isDefined)


        if (mentionRecord.parentIds)
        // All the children should be known and/or will be added later.

      }
      else {

      }

      mentionMap.put(mention, mentionRecord)
      isDuplicate
    }

    def addMention(parentOpt: Option[Mention], mention: Mention): Unit = {
      val children = getChildren(mention)
      val isDuplicate = addMentionAndChildren(parentOpt, mention, children)

      if (!isDuplicate)
        children.foreach { child =>
          addMention(Some(mention), child)
        }
    }

    mentions.foreach { mention => addMention(None, mention) }
    hasLoops()
  }

  def runTest(pmcid: String, outputFormat: String): Unit = {
    val filename = mkFilename(pmcid)
    val file = new File(filename)
    val entry = PaperReader.getEntryFromPaper(file)
    val mentions = PaperReader.getMentionsFromEntry(entry)
    val paperId = pmcid
    val startTime = ReachCLI.now

    try {
      if (!checkForLoops(mentions))
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
    val outputFormat = "serial-json"
    def test(pmcid: String): Unit = runTest(pmcid, outputFormat)

    behavior of "serial-json format"

    infinite should "not throw a NegativeArraySizeException" in {
      val pmcid = "PMC7176272"
      test(pmcid)
    }
  }
}
