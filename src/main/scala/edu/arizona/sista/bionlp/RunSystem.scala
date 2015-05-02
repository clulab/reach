package edu.arizona.sista.bionlp

import java.io.File
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FileUtils
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.odin._
import edu.arizona.sista.odin.domains.bigmechanism.dryrun2015.Ruler.readRules
import edu.arizona.sista.odin.domains.bigmechanism.dryrun2015.DarpaActions
import edu.arizona.sista.odin.domains.bigmechanism.dryrun2015.mentionToStrings
import edu.arizona.sista.odin.domains.bigmechanism.summer2015.{ LocalGrounder, Coref }

object RunSystem extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val nxmlDir = new File(config.getString("nxmlDir"))
  val friesDir = new File(config.getString("friesDir"))
  val encoding = config.getString("encoding")

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

  println("initializing processors ...")
  val proc = new BioNLPProcessor
  proc.annotate("something")

  println("initializing odin ...")
  val rules = readRules()
  val actions = new DarpaActions
  val grounder = new LocalGrounder
  val coref = new Coref
  val flow = grounder andThen coref
  val engine = ExtractorEngine(rules, actions, flow.apply)

  println("initializing nxml2fries ...")
  val nxml2fries = new Nxml2Fries(
    config.getString("nxml2fries.executable"),
    config.getBoolean("nxml2fries.removeCitations"),
    encoding)

  for {
    file <- nxmlDir.listFiles
    if file.getName.endsWith(".nxml")
    entry <- nxml2fries.extractEntries(file).par  // process sections in parallel
  } {
    val name = s"${entry.name}_${entry.chunkId}"
    println(s"working on $name ...")
    val doc = proc.annotate(entry.text, keepText = true)
    doc.id = Some(name)
    val mentions = engine.extractFrom(doc)
    val lines = mentions.flatMap(mentionToStrings)
    val outFile = new File(friesDir, s"${name}.txt")
    FileUtils.writeLines(outFile, lines.asJavaCollection)
  }
}
