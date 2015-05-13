package edu.arizona.sista.bionlp.reach.preprocessing

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Calendar
import edu.arizona.sista.bionlp.reach.rulelearning.PathFinder
import edu.arizona.sista.bionlp.reach.utils.FileReader
import edu.arizona.sista.struct.Interval
import org.slf4j.LoggerFactory

/**
 * Generate odin-style entity rules (token-based) from KB files
 * Use at your own peril!
 * This needs to make use of RegexTrie to have any hope of executing in a timely fashion
 */
object RuleGenerator {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  /**
   * Creates a lemma-based token pattern from a string
   * @param synonym a word or phrase
   * @return a lemma-based token pattern
   */
  def lemmaPatternFromSynonym(synonym: String):String = {
    // This should only be a single "sentence", but just to be safe...
    val pattern = (for (s <- preprocessor.annotate(synonym).sentences) yield {
      val pathFinder = new PathFinder(s)
      val lemmaPattern =
        pathFinder.mkTokenConstraints(
          new Interval(0, s.words.length),
          withWords = false,
          withLemmas = true).get
      lemmaPattern
    }).mkString(" ")

    //logger.debug(s"$line => $pattern")
    pattern
  }

  /**
   * Creates a set of token pattern rules for all entries in a kb (.tsv) file.
   * @param f is a Java File object
   * @param chunkSize The number of lines to join into a single rule
   * @return A sequence of rule representation Strings
   */
  def mkRulesFromKBFile(f: File, chunkSize: Int = 100):Seq[String] = {
    // Filename without extension
    val basename = FileReader.removeExtension(f)
    val filteredLines:Seq[String] = filterLines(f)
    // Get labels
    val labels = mkLabelRepresentation(basename)

    // Create our chunks from the specified kb file
    val lemmaPatterns:Seq[String] =
      (for (line <- filteredLines) yield {
        // Only worry about the first column of the file
        val synonym = line.split("\t").head
        // get lemma-based pattern
        lemmaPatternFromSynonym(synonym)
      })
        .distinct

    // Generate rules from chunks of lemmatized kb lines
    val rules = for ((chunk, i) <-
                     lemmaPatterns
                       .grouped(chunkSize)
                       .zipWithIndex) yield {

      // Create a disjunct of the token based patterns for the N entities
      val entityRule = chunk
        .sortBy(_.length)
        .reverse
        .mkString(" |\n    ")

      s"""
       |- name: entity-rule-$basename-${i+1}
       |  label: $labels
       |  priority: 1
       |  type: token
       |  pattern: |
       |    $entityRule
     """.stripMargin
    }
    rules.toSeq
  }

  /**
   * Write kb-derived rules to a yml file under biogrammar
   * @param kbFile a kb File object
   */
  def kbToRuleFile(kbFile: File): Unit = {

    logger.debug(s"processing ${kbFile.getName}...")
    val rules = mkRulesFromKBFile(kbFile)
    // File name without extension
    val basename = FileReader.removeExtension(kbFile)
    val ruleFile =  s"$basename.yml"
    val outFile = new File(s"src/main/resources/edu/arizona/sista/odin/domains/bigmechanism/summer2015/biogrammar/$ruleFile")
    val bw = new BufferedWriter(new FileWriter(outFile))

    val timeStamp = Calendar.getInstance().getTime.toString
    val validSpecies = SpeciesLUT(basename)
    val speciesRepresentation: String =
      validSpecies match {
        case nothing if validSpecies.isEmpty => {
          logger.debug(s"$basename: using all species")
          "# all species"
        }
        case _ => {
          logger.debug(s"$basename: only considering the following species: ${validSpecies.mkString(", ")}")
          s"# only for species: ${validSpecies.mkString(", ")}"
        }
    }
    // Write header
    bw.write(s"# $timeStamp\n")
    bw.write(s"$speciesRepresentation\n")

    // Write rules
    bw.write(rules.mkString("\n"))
    bw.close()
    logger.info(s"$ruleFile written to biogrammar!")
  }
}

object RuleWriter extends App {
  // Get the relevant files
  val kbFiles =
    new File("src/main/resources/edu/arizona/sista/odin/domains/bigmechanism/summer2015/kb")
      .listFiles
      .filter(f => f.getName.matches(".*(tsv|gz)$"))
  kbFiles.foreach(RuleGenerator.kbToRuleFile)

}
