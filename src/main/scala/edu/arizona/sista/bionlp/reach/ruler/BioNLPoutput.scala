package edu.arizona.sista.bionlp.reach.ruler


import java.io.{PrintWriter, File}
import edu.arizona.sista.bionlp.reach.brat._
import edu.arizona.sista.odin._
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

/**
 * Created by dane on 2/5/15.
 */
object BioNLPoutput extends App {

  val entityRules = BasicRuler.readEntityRules()
  val eventRules = BasicRuler.readEventRules()
  val rules = entityRules + "\n\n" + eventRules

  val actions = new DarpaActions

  val proc = new BioNLPProcessor(withNER=false)
  val extractor = new BasicRuler(rules, actions)

  val outDir = s"${System.getProperty("user.home")}/Desktop/devents2/"
  val dataDir = s"${System.getProperty("user.home")}/Documents/corpora/bionlp2013GE_dev/"

  val files = new File(dataDir).listFiles.filter(_.getName.endsWith(".txt"))

  for (f <- files) {
    // get filename minus extension
    val pmid = f.getName.takeWhile(_ != '.')
    println(pmid + " ...")

    // read text and standoff
    val text = readFile(s"$dataDir/$pmid.txt")
    val entities = readFile(s"$dataDir/$pmid.a1")

    // annotate text using gold named entities
    val doc = proc.annotate(text)
    val annotations = Brat.readStandOff(entities)
    val neLabels = Brat.alignLabels(doc, annotations)

    // relabel sentences
    neLabels.zipWithIndex foreach {
      case (labels, i) => doc.sentences(i).entities = Some(labels.toArray)
    }

    val mentions = extractor.extractFrom(doc) filter (_.isInstanceOf[EventMention]) filter (_.label != "Hydrolysis")
    //val mentions = extractor.extractFrom(doc) filter (_.isInstanceOf[TextBoundMention]) filter (_.label != "Hydrolysis")

    // open new .a2 file based on .txt filename
    val output = new PrintWriter(new File(s"$outDir/$pmid.a2"))

    // print mentions to new .a2 file
    output.write(Brat.dumpStandoff(mentions, doc, annotations))

    // close new .a2 file
    output.close()
  }


}
