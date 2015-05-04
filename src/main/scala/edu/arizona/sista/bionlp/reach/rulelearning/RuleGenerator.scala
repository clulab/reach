package edu.arizona.sista.bionlp.reach.rulelearning

import java.io.{FileWriter, BufferedWriter, PrintWriter, File}

import edu.arizona.sista.bionlp.reach.utils.FileReader
import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.struct.Interval
import org.apache.commons.io.FilenameUtils
import org.slf4j.LoggerFactory


/**
 * Created by gus
 */
object RuleGenerator {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  val proc = new BioNLPProcessor(withNER = false)

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
   * Creates a set of token pattern rules for all entries in a kb (.tsv) file.
   * @param f is a Java File object
   * @param chunkSize The number of lines to join into a single rule
   * @return A sequence of rule representation Strings
   */
  def mkRulesFromKBFile(f: File, chunkSize: Int = 100):Seq[String] = {
    val entitiesForSomething:Iterator[String] = FileReader.readFile(f)
    // Create our chunks from the specified kb file
    val rules = for ((chunk, i) <- entitiesForSomething.grouped(chunkSize).toArray.zipWithIndex) yield {
      // Get token patterns for a chunk of entities
      val linesAsTokenConstraints: Seq[String] = for (line: String <- chunk) yield {
        // Only worry about the first column of the file
        val synonym = line.split("\t").head
        // get lemma-based pattern
        lemmaPatternFromSynonym(synonym)
      }
      // Create a disjunct of the token based patterns for the N entities
      val entityRule = linesAsTokenConstraints
        .sortBy(_.size)
        .reverse
        .mkString(" |\n    ")

      s"""
       |- name: entity-rule-${f.getName}-$i
       |  label: [BioChemicalEntity]
       |  priority: 1
       |  type: token
       |  pattern: |
       |    $entityRule
     """.stripMargin
    }
    rules
  }

  /**
   * Write kb-derived rules to a yml file under biogrammar
   * @param kbFile a kb File object
   */
  def kbToRuleFile(kbFile: File): Unit = {
    val rules = mkRulesFromKBFile(kbFile)
    val fname =  s"${FilenameUtils.removeExtension(kbFile.getName)}.yml"
    val outFile = new File(s"src/main/resources/edu/arizona/sista/odin/domains/bigmechanism/summer2015/biogrammar/$fname")
    val bw = new BufferedWriter(new FileWriter(outFile))
    bw.write(rules.mkString("\n"))
    bw.close()
  }
}

object RuleWriter extends App {
  val kbFile: File = new File(getClass.getResource("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/kb/tissue-type.tsv").getPath)
  RuleGenerator.kbToRuleFile(kbFile)
}
