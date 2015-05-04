package edu.arizona.sista.odin.extern.export.fries

import java.io._

import edu.arizona.sista.processors.{DocumentSerializer, Document}
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.odin._
import edu.arizona.sista.odin.domains.bigmechanism.dryrun2015.Ruler.readRules
import edu.arizona.sista.odin.domains.bigmechanism.dryrun2015.DarpaActions
import edu.arizona.sista.odin.domains.bigmechanism.summer2015.{ LocalGrounder, Coref }
import edu.arizona.sista.odin.extern.inward._

import org.slf4j.LoggerFactory

/**
  * Top-level test driver for Fries output development.
  *   Author: by Tom Hicks. 4/30/2015.
  *   Last Modified: Redo file handling. Rewrite flow more like bionlp.
  */
object FriesDriver extends App {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  val ds = new DocumentSerializer
  val processor = new BioNLPProcessor()

  val rules = readRules()
  val actions = new DarpaActions
  val grounder = new LocalGrounder
  val coref = new Coref
  val flow = grounder andThen coref
  val engine = ExtractorEngine(rules, actions, flow.apply)

  val PapersDir = s"${System.getProperty("user.dir")}/src/test/resources/papers/"
  val paperNames = Seq(
    "MEKinhibition.txt.ser",
    "UbiquitinationofRas.txt.ser",
    "PMC3441633.txt.ser",
    "PMC3847091.txt.ser"
  )

  def cleanText (m: Mention): String = {
    """(\s+|\n|\t|[;])""".r.replaceAllIn(m.document.sentences(m.sentence).getSentenceText(), " ")
  }

  def docFromSerializedFile (filename: String): Document = {
    val br = new BufferedReader(new FileReader(filename))
    val doc = ds.load(br)
    doc
  }

  def getText(fileName: String):String = scala.io.Source.fromFile(fileName).mkString

  val outDir = s"${System.getProperty("user.dir")}" + File.separator
  def mkOutputName (paper:String, ext:String): String = {
    outDir + {"""^.*?/|.txt.ser""".r.replaceAllIn(paper, "")} + ext
  }

  def processPapers (papers:Seq[String], asStrings:Boolean=false) = {
    papers.foreach { paper => processPaper(paper, asStrings) }
  }

  def processPaper (paper: String, asStrings:Boolean=false) = {
    val ext = if (asStrings) ".txt" else ".json"
    val outName = mkOutputName(paper, ext)
    val outFile = new File(outName)
    val inFile = s"$PapersDir/$paper"

    val doc = paper match {
      case ser if ser.endsWith("ser") => docFromSerializedFile(inFile)
      case _ => processor.annotate(getText(inFile), keepText=true)
    }
    doc.id = Some(paper)                    // fake document ID for now

    // val mentions = extractor.extractFrom(doc)
    val mentions = engine.extractFrom(doc)
    val sortedMentions = mentions.sortBy(m => (m.sentence, m.start)) // sort by sentence, start idx
    if (asStrings)
      outputEventMentions(sortedMentions, doc, outFile)
    else
      outputFries(sortedMentions, doc, outFile)
  }

  /** Output string representations for the given sequence of mentions. */
  def outputAllMentions (mentions:Seq[Mention], doc:Document, outFile:File) = {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
    val menMgr = new MentionManager()
    mentions.foreach { m =>
      menMgr.mentionToStrings(m).foreach { str => out.println(str) }
    }
    out.flush()
    out.close()
  }

  /** Output a FRIES representation for the given sequence of mentions. */
  def outputFries (mentions:Seq[Mention], doc:Document, outFile:File) = {
    val frier = new FriesOutput()
    frier.toJSON(mentions, doc, outFile)
  }

  /** Output string representations for event mentions in the given sequence. */
  def outputEventMentions (mentions:Seq[Mention], doc:Document, outFile:File) = {
    val menMgr = new MentionManager()
    menMgr.outputSelectedMentions("Event", mentions, outFile)
  }


  // Top-level Main of script:
  var asStrings:Boolean = false
  if (!args.isEmpty) asStrings = true
  processPapers(paperNames, asStrings)

}
