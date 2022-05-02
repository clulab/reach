package org.clulab.reach

import java.io._
import scala.collection.JavaConverters._
import scala.io.Source
import ai.lum.nxmlreader.{NxmlDocument, NxmlReader}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FilenameUtils
import ai.lum.common.FileUtils._
import org.clulab.odin._
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.utils.{DSVParser, Preprocess}
import org.clulab.utils.Serializer
import org.clulab.utils.ThreadUtils

object PaperReader extends LazyLogging {

  type PaperId = String
  type Dataset = Map[PaperId, Vector[Mention]]

  logger.debug("loading ...")
  // to set a custom conf file add -Dconfig.file=/path/to/conf/file to the cmd line for sbt
  val config = ConfigFactory.load()

  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")
  val ignoreSections = config.getStringList("ignoreSections").asScala
  val fileEncoding = config.getString("encoding")

  // create appropriate context engine with which to initialize ReachSystem
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  val contextConfig = config.getConfig("contextEngine.params").root
  val contextEngineParams: Map[String, String] =
    context.createContextEngineParams(contextConfig)

  // initialize ReachSystem
  val procAnnotator = new BioNLPProcessor()
  val preproc = new Preprocess
  lazy val reachSystem = new ReachSystem(
    processorAnnotator = Some(procAnnotator),
    contextEngineType = contextEngineType,
    contextParams = contextEngineParams
  )

  // systems for reading papers
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
  val dsvReader = new DSVParser()

  /**
   * Produces Dataset from a directory of nxml and csv papers
   * @param path a directory of nxml and csv papers
   * @return a Dataset (PaperID -> Mentions)
   */
  def readPapers(path: String): Dataset = readPapers(new File(path))

  /**
   * Produces Dataset from a directory of nxml and csv papers.
   * @param dir a File (directory) of nxml and csv papers
   * @return a Dataset (PaperID -> Mentions)
   */
  def readPapers(dir: File): Dataset = {
    require(dir.isDirectory, s"'${dir.getCanonicalPath}' is not a directory")
    // read papers in parallel
    // limit parallelization
    val files = ThreadUtils.parallelize(dir
        .listFilesByRegex(pattern=ReachInputFilePattern, caseInsensitive=true, recursive=true)
        .toArray,
      threadLimit
    )
    // build dataset
    val data = for {
      file <- files                         // read papers in parallel
    } yield readPaper(file)
    data.seq.toMap
  }

  /**
    * Produces Mentions from a few different types of input files.
    * @param file a File with either the .csv, .tsv, .txt, or .nxml extension
    * @return (PaperID, Mentions)
    */
  def readPaper(file: File): (String, Vector[Mention]) = file match {
    case nxml if nxml.getName.endsWith(".nxml") =>
      readNXMLPaper(nxml)
    case dsv if dsv.getName.endsWith(".csv") || dsv.getName.endsWith(".tsv") =>
      readDSVPaper(dsv)
    case txt if txt.getName.endsWith(".txt") =>
      readPlainTextPaper(txt)
    case other =>
      throw new Exception(s"Given ${file.getAbsolutePath}, but readPaper doesn't support ${FilenameUtils.getExtension(other.getAbsolutePath)}")
  }

  private def readNXMLPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".nxml"), s"Given ${file.getAbsolutePath}, but readNXMLPaper only handles .nxml files!")
    val paperID = FilenameUtils.removeExtension(file.getName)
    logger.debug(s"reading paper $paperID ...")
    paperID -> reachSystem.extractFrom(nxmlReader.read(file)).toVector
  }

  private def readDSVPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".tsv") || file.getName.endsWith(".csv"), s"Given ${file.getAbsolutePath}, but readDSVPaper only handles .tsv and .csv files!")
    val paperID = FilenameUtils.removeExtension(file.getName)
    logger.debug(s"reading paper $paperID ...")
    // get a single entry for the valid sections
    val entry = getEntryFromPaper(file)
    paperID -> reachSystem.extractFrom(entry).toVector
  }

  private def readPlainTextPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".txt"), s"Given ${file.getAbsolutePath}, but readPlainTextPaper only handles .txt files!")
    val entry = getEntryFromPaper(file)
    entry.name -> reachSystem.extractFrom(entry).toVector
  }

  def getContents(file: File): String = Source.fromFile(file, fileEncoding).getLines.mkString

  /**
    * Get a single FriesEntry representing a paper
    * @param file
    * @return [[FriesEntry]]
    */
  def getEntryFromPaper(file: File): FriesEntry = file match {
    case nxml if nxml.getName.endsWith(".nxml") =>
      val nxmlDoc: NxmlDocument = nxmlReader.read(nxml)
      new FriesEntry(nxmlDoc)

    case dsv if dsv.getName.endsWith(".csv") || dsv.getName.endsWith("tsv") =>
      dsvReader.toFriesEntry(dsv, sectionsToIgnore = ignoreSections.toSet)

    case txt if txt.getName.endsWith(".txt") =>
      val paperID = FilenameUtils.removeExtension(txt.getName)
      logger.debug(s"reading paper $paperID ...")
      val text = getContents(file)
      FriesEntry.mkFriesEntry(paperID, text)
  }

  def getEntryFromPaper(fileName: String): FriesEntry = getEntryFromPaper(new File(fileName))

  def getMentionsFromEntry(entry: FriesEntry): Vector[Mention] = reachSystem.extractFrom(entry).toVector

  def getMentionsFromPaper(file: File): Vector[Mention] = readPaper(file)._2

  /** Extract mentions from a single text string. */
  def getMentionsFromText(text: String): Seq[Mention] = reachSystem.extractFrom(text, "", "", None)

}


object ReadPapers extends App with LazyLogging {

  // to set a custom conf file add -Dconfig.file=/path/to/conf/file to the cmd line for sbt
  val config = ConfigFactory.load()

  val papersDir = config.getString("ReadPapers.papersDir")
  val outFile = config.getString("ReadPapers.serializedPapers")

  logger.info("reading papers ...")
  val dataset = PaperReader.readPapers(papersDir)

  logger.info("serializing ...")
  Serializer.save(dataset, outFile)
}
