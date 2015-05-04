package edu.arizona.sista.bionlp.reach.rulelearning

import java.io.{FileWriter, BufferedWriter, File}
import java.util.Calendar
import edu.arizona.sista.bionlp.reach.utils.FileReader
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.struct.Interval
import org.slf4j.LoggerFactory


/**
 * Created by gus
 */
object RuleGenerator {

  //TODO: move these two Maps to the package object
  // Retrieve relevant rule labels associated with a kb file basename
  val KBLUT: Map[String, String] =
    Map("hmdb" -> "[BioChemicalEntity]",
        "ProteinFamilies" -> "[Family, BioChemicalEntity]",
        "tissue-type" -> "[BioChemicalEntity]",
        "uniprot-proteins" -> "[Protein, BioChemicalEntity]")
      .withDefaultValue("[BioChemicalEntity]")

  val speciesOfInterest = Seq("Human", "Homo sapiens")
  val SpeciesLUT: Map[String, Seq[String]] =
    Map("uniprot-proteins" -> speciesOfInterest,
        "ProteinFamilies" -> speciesOfInterest).withDefaultValue(Nil)

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  val proc = new BioNLPProcessor(withNER = false)

  /**
   * Removes extension from filename (Apache Commons seemed to have trouble with .tsv)
   * @param f a File object
   * @return a String representation of the File name without its extension
   */
  def removeExtension(f: File): String = {
    val fname = f.getName
    fname.toCharArray.takeWhile(_ != '.').mkString("")
  }

  /**
   * Creates a lemma-based token pattern from a string
   * @param synonym a word or phrase
   * @return a lemma-based token pattern
   */
  def lemmaPatternFromSynonym(synonym: String):String = {
    // This should only be a single "sentence", but just to be safe...
    val pattern = (for (s <- proc.annotate(synonym).sentences) yield {
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
   * Only consider lines that match the set of valid species labels (defined in SpeciesLUT).
   * @param f a KB File
   * @return the valid lines pertaining to the specified species
   */
  def filterLines(f: File): Seq[String] = {
    val lines = FileReader.readFile(f).toSeq
    val basename = removeExtension(f)
    val validSpecies = SpeciesLUT(basename)

    validSpecies match {
      case Nil => lines
      case theseSpecies => {
        val filteredLines =
          lines.filter{ case line => theseSpecies.contains(line.split("\t")(1).trim) }
        logger.debug(s"${lines.length - filteredLines.length} entries ignored for $basename")
        filteredLines
       }
    }
  }

  /**
   * Creates a set of token pattern rules for all entries in a kb (.tsv) file.
   * @param f is a Java File object
   * @param chunkSize The number of lines to join into a single rule
   * @return A sequence of rule representation Strings
   */
  def mkRulesFromKBFile(f: File, chunkSize: Int = 100):Seq[String] = {
    // Filename without extension
    val basename = removeExtension(f)
    val filteredLines:Seq[String] = filterLines(f)
    // Get labels
    val labels = KBLUT(basename)

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
    val basename = removeExtension(kbFile)
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
