package edu.arizona.sista.reach

import java.io.{PrintWriter,File}
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.reach.brat._

object ExportBrat extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val nxmlDir = new File(config.getString("nxmlDir"))
  val txtDir = new File(config.getString("txtDir"))
  val bratDir = new File(config.getString("bratDir"))
  val encoding = config.getString("encoding")

  // if nxmlDir does not exist there is nothing to do
  if (!nxmlDir.exists) {
    sys.error(s"${nxmlDir.getCanonicalPath} does not exist")
  }

  // if bratDir does not exist create it
  if (!bratDir.exists) {
    println(s"creating ${bratDir.getCanonicalPath}")
    FileUtils.forceMkdir(bratDir)
  } else if (!bratDir.isDirectory) {
    sys.error(s"${bratDir.getCanonicalPath} is not a directory")
  }

  // if txtDir does not exist create it
  if (!txtDir.exists) {
    println(s"creating ${txtDir.getCanonicalPath}")
    FileUtils.forceMkdir(txtDir)
  } else if (!txtDir.isDirectory) {
    sys.error(s"${txtDir.getCanonicalPath} is not a directory")
  }

  println("initializing reach ...")
  val reach = new ReachSystem

  println("initializing nxml2fries ...")
  val nxml2fries = new Nxml2Fries(
    config.getString("nxml2fries.executable"),
    txtDir,
    config.getBoolean("nxml2fries.removeCitations"),
    config.getStringList("nxml2fries.ignoreSections").asScala.toSet,
    encoding)

  // process papers in parallel
  for (file <- nxmlDir.listFiles.par if file.getName.endsWith(".nxml")) {
    val paperId = FilenameUtils.removeExtension(file.getName)

    // process individual sections and collect all mentions

    val entries = nxml2fries.extractEntries(file).filter(_.isSuccess).map(_.get)
    val text = entries.map(_.text).mkString("\n")
    val doc = reach.mkDoc(text, paperId)
    val mentions = reach.extractFrom(doc)

    val events = mentions.filter(m => m.isInstanceOf[EventMention])
    val textbound = events flatMap (e => e.arguments.values) flatMap (x=>x)
    val standoff = Brat.dumpStandoff(textbound++events, doc)

    val outputText = new PrintWriter(new File(bratDir, s"${paperId}.txt"))
    val outputStanoff = new PrintWriter(new File(bratDir, s"${paperId}.ann"))

    outputText.write(doc.text.get)
    outputStanoff.write(standoff)
    outputText.close()
    outputStanoff.close()


  }

}
