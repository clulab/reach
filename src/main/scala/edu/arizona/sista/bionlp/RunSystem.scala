package edu.arizona.sista.bionlp

import java.io.File
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import edu.arizona.sista.odin.domains.bigmechanism.dryrun2015.mentionToStrings

object RunSystem extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val nxmlDir = new File(config.getString("nxmlDir"))
  val friesDir = new File(config.getString("friesDir"))
  val encoding = config.getString("encoding")
  val outputType = config.getString("outputType")

  // if nxmlDir does not exist there is nothing to do
  if (!nxmlDir.exists) {
    sys.error(s"${nxmlDir.getCanonicalPath} does not exist")
  }

  // if friesDir does not exist create it
  if (!friesDir.exists) {
    println(s"creating ${friesDir.getCanonicalPath}")
    FileUtils.forceMkdir(friesDir)
  } else if (!friesDir.isDirectory) {
    sys.error(s"${friesDir.getCanonicalPath} is not a directory")
  }

  println("initializing reach ...")
  val reach = new ReachSystem

  println("initializing nxml2fries ...")
  val nxml2fries = new Nxml2Fries(
    config.getString("nxml2fries.executable"),
    config.getBoolean("nxml2fries.removeCitations"),
    config.getStringList("nxml2fries.ignoreSections").asScala.toSet,
    encoding)

  // process papers in parallel
  for (file <- nxmlDir.listFiles.par if file.getName.endsWith(".nxml")) {
    val paperId = FilenameUtils.removeExtension(file.getName)

    // process individual sections and collect all mentions
    val paperMentions = for {
      entry <- nxml2fries.extractEntries(file)
      mention <- reach.extractFrom(entry)
    } yield mention

    if (outputType != "text") {             // if reach will handle output
      reach.outputMentions(paperMentions, outputType, paperId, friesDir)
    }
    else {                                  // else dump all paper mentions to file
      val lines = paperMentions.flatMap(mentionToStrings)
      val outFile = new File(friesDir, s"$paperId.txt")
      println(s"writing ${outFile.getName} ...")
      FileUtils.writeLines(outFile, lines.asJavaCollection)
    }
  }
}
