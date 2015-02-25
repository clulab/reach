package edu.arizona.sista.bionlp.reach.ruler


import java.io.{PrintWriter, File, BufferedReader, FileReader}
import edu.arizona.sista.processors.{DocumentSerializer, Document}
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.odin._

/**
 * Created by gus on 12/22/14.
 */
object DARPAoutput extends App {

  val entityRules = BasicRuler.readEntityRules()
  val eventRules = BasicRuler.readEventRules()
  val rules = entityRules + "\n\n" + eventRules

  val ds = new DocumentSerializer

  val actions = new DarpaActions

  val proc = new BioNLPProcessor()
  val extractor = new BasicRuler(rules, actions)

  val outDir = s"${System.getProperty("user.home")}/Desktop/"
  val PapersDir = s"${System.getProperty("user.dir")}/src/main/resources/edu/arizona/sista/bionlp/extractors/papers/"

  val paperNames = Seq(//"train/MEKinhibition.txt.ser",
    //"train/UbiquitinationofRas.txt.ser",
    //"dryrun/Nat_Struct_Mol_Biol_2013_Jan_25_20(1)_46-52.txt.ser",
    //"dryrun/PLoS_One_2013_Dec_2_8(12)_e82022.txt.ser"
    "test/PMC3441633.txt.ser",
    "test/PMC3847091.txt.ser"
  )

  def mkOutputName(paper: String): String = s"$outDir${ """^.*?/|.txt.ser""".r.replaceAllIn(paper, "")}.csv"

  def processPapers(papers: Seq[String]) = papers.foreach { paper => processPaper(paper)}

  def processPaper(paper: String): Unit = {

    val outName = mkOutputName(paper)
    val output = new PrintWriter(new File(outName))

    val header = s"Mention Count;Relation;Model Link (BioPax or BEL);‘English-like’ Description;Model Representation;Source Text\n"

    println(s"Writing output to $outName")
    output.write(header)

    val doc = docFromSerializedFile(s"$PapersDir/$paper")
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
