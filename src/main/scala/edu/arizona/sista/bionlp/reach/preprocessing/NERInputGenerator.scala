package edu.arizona.sista.bionlp.reach.preprocessing

import java.io.{FileWriter, BufferedWriter, File}
import java.util.Calendar
import edu.arizona.sista.bionlp.reach.utils.FileReader
import org.slf4j.LoggerFactory


case class Syns(label: String, lines: Lines)


/**
 * Methods for creating Trie-based NER system input from kb files
 */
object NERInputGenerator {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  /**
   * Creates a tokenized representation of each valid line in some kb file.
   * @param f is a Java File object
   * @return Syns (entityLabel, Lines)
   */
  def tokenizeKBFile(f: File):Syns = {

    logger.debug(s"processing ${f.getName}...")

    // Filename without extension
    val basename = FileReader.removeExtension(f)

    // lookup basename for prefix
    val entityLabel = KBLUT(basename).head
    val filteredLines:Seq[String] = filterLines(f)

    // Tokenize each relevant line of the specified kb file
    val tokenizedLines:Seq[Array[String]] =
      (for (line <- filteredLines) yield {
        // Only worry about the first column of the file
        val synonym = line.split("\t").head
        // get tokens
        val doc = preprocessor.mkDocument(synonym, keepText = false)
        doc.sentences.flatMap(_.words)
      })
        .distinct
    Syns(entityLabel, tokenizedLines)
  }

  /**
   * Writes tokenized kb lines to a tsv file
   * @param syns a Syns (entityLabel, Lines) tuple
   */
  def mkNERInputFile(syns: Syns, outPath: String): Unit = {

    val basename = syns.label
    val NERinputFile =  s"$basename.tsv"
    val outFile = new File(s"$outPath/$NERinputFile")
    // Make intermediate directories if they don't yet exist
    println(s"making any req. intermediate directories...")
    outFile.getParentFile.mkdirs

    val bw = new BufferedWriter(new FileWriter(outFile))

    val timeStamp = Calendar.getInstance().getTime.toString

    // Write header
    bw.write(s"# $timeStamp\n")
    //bw.write(s"$speciesRepresentation\n")

    val lines = syns.lines
      // distinct on Arrays doesn't work...
      .map(_.toSeq)
      .distinct
      .filter(_.nonEmpty)
      .sortWith(_.head(0) < _.head(0))

    val representation =
      lines.map(_.mkString("\t"))
      .mkString("\n")

    // Write rules
    bw.write(representation)
    bw.close()
    logger.info(s"$NERinputFile written to ${outFile.getAbsolutePath}!")
  }
}

/**
 * KB files => tokenized input files for Trie-based NER
 * the only command line parameter is the path to the desired output folder
 */
object NERInputWriter extends App {

  def onError() {
    Console.println(s"\n${Console.RED}ERROR:\tnot a valid filename.${Console.RESET}")
    Console.println(s"""USAGE:\n\t${Console.BOLD} sbt "run-main edu.arizona.sista.bionlp.reach.preprocessing.NERInputWriter path/to/output/folder"${Console.RESET}""")
    sys.exit(0)
  }

  // present usage instructions and exit if arguments not given
  println(s"length of args: ${args.length}")
  if (args.length != 1) onError()

  // read json file from command line
  val outPath = args.last.replaceFirst("^~",System.getProperty("user.home"))
  // Get relevant files
  val kbFiles =
    new File("src/main/resources/edu/arizona/sista/odin/domains/bigmechanism/summer2015/kb")
      .listFiles
      .filter(f => f.getName.matches(".*(tsv|gz)$") & !f.getName.contains("tissue-type"))

  val entitySyns: Seq[Syns] =
    kbFiles.map(NERInputGenerator.tokenizeKBFile)
    .groupBy(_.label)
      // get lines from Syns object
      .mapValues(_.map(_.lines)
      // flatten Lines from each Syns object of that type
      .flatMap(line => line).toSeq)
      // create the consolidated Syns for a distinct entityLabel
      .map{pair => Syns(pair._1,pair._2)}
      .toSeq


  entitySyns.foreach( syn => NERInputGenerator.mkNERInputFile(syn, outPath))
}

