package edu.arizona.sista.bionlp.reach.ruler


import java.io.{PrintWriter, File, BufferedReader, FileReader}
import edu.arizona.sista.matcher.{Mention, ExtractorEngine, EventMention}
import edu.arizona.sista.processors.{DocumentSerializer, Document}
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

/** This class applies DARPA rules to all the files specified a command
 *  line arguments.
 *
 *  Based on the DARPAoutput class
 *
 *  Created by Enrique on 2/11/15.
 */
object CommandLineOutput extends App {

  val entityRules = BasicRuler.readEntityRules()
  val eventRules = BasicRuler.readEventRules()
  val rules = entityRules + "\n\n" + eventRules

  val ds = new DocumentSerializer

  val actions = new DarpaActions

  val proc = new BioNLPProcessor()
  val extractor = new BasicRuler(rules, actions)

  val outDir = s"${System.getProperty("user.home")}/Desktop/processed_papers/"

  val paperNames = args // Take the arguments from the command line

  def mkOutputName(paper: String): String = s"$outDir${ """^.*?/|.txt.ser""".r.replaceAllIn(paper, "")}.csv"

  def processPapers(papers: Seq[String]) = papers.foreach { paper => processPaper(paper)}

  def processPaper(paper: String): Unit = {

    println(s"Processing $paper...")

    val outName = mkOutputName(paper.split("/").last) // Make sure to remove the directory from the file name
    val output = new PrintWriter(new File(outName))

    val header = s"Mention Count;Relation;Model Link (BioPax or BEL);‘English-like’ Description;Model Representation;Source Text\n"

    println(s"Writing output to $outName")
    output.write(header)

    val doc = docFromSerializedFile(s"$paper")
    val mentions: Map[String, Seq[EventMention]] =
      retrieveMentions(doc)
        .groupBy(m => m.repr)

    mentions.foreach(pair => writeEvents(pair._1, pair._2, output))

    output.close()
  }

  def docFromSerializedFile(filename: String): Document = {
    val br = new BufferedReader(new FileReader(filename))
    val doc = ds.load(br)
    doc
  }

  def retrieveMentions(doc: Document): Seq[EventMention] = {
    extractor.extractFrom(doc).filter(_.isInstanceOf[EventMention]).map(_.asInstanceOf[EventMention])
  }

  def cleanText(m: Mention): String = {
    """(\s+|\n|\t|[;])""".r.replaceAllIn(m.document.sentences(m.sentence).getSentenceText(), " ")
  }

  def writeEvents(representation: String, mentions: Seq[EventMention], output: PrintWriter) {
    def getText: String = {
      mentions.sortBy(m => (m.sentence, m.start)) // sort by sentence, start idx
        .map(m => cleanText(m)) // get text
        .mkString("  ")
    }
    output.write(s"${mentions.size};;;;$representation;$getText\n")
  }

  processPapers(paperNames)
}
