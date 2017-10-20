package org.clulab.reach

import java.io._

import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray
import scala.collection.JavaConverters._

import ai.lum.nxmlreader.{NxmlDocument, NxmlReader}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FilenameUtils

import ai.lum.common.FileUtils._

import org.clulab.odin._
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.coserver.ProcessorCoreClient
import org.clulab.reach.utils.DSVParser
import org.clulab.utils.Serializer


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

  // systems for reading papers
  val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = rs.processor.preprocessText)
  val dsvReader = new DSVParser()

  // for context engine
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  val contextConfig = config.getConfig("contextEngine.params").root
  val contextEngineParams: Map[String, String] =
    context.createContextEngineParams(contextConfig)

  // initialize ReachSystem with appropriate context engine
  lazy val rs = new ReachSystem(pcc = Some(new ProcessorCoreClient),
                                contextEngineType = contextEngineType,
                                contextParams = contextEngineParams)

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
    val files = dir.listFilesByRegex(
      pattern=ReachInputFilePattern, caseSensitive=false, recursive=true).toArray.par

    // limit parallelization
    files.tasksupport =
      new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))

    // build dataset
    val data: ParArray[(String, Vector[Mention])] = for {
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
    paperID -> rs.extractFrom(nxmlReader.read(file)).toVector
  }

  private def readDSVPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".tsv") || file.getName.endsWith(".csv"), s"Given ${file.getAbsolutePath}, but readDSVPaper only handles .tsv and .csv files!")
    val paperID = FilenameUtils.removeExtension(file.getName)
    logger.debug(s"reading paper $paperID ...")
    // get a single entry for the valid sections
    val entry = getEntryFromPaper(file)
    paperID -> rs.extractFrom(entry).toVector
  }

  private def readPlainTextPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".txt"), s"Given ${file.getAbsolutePath}, but readPlainTextPaper only handles .txt files!")
    val entry = getEntryFromPaper(file)
    entry.name -> rs.extractFrom(entry).toVector
  }

  def getContents(file: File): String = scala.io.Source.fromFile(file, fileEncoding).getLines.mkString

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

  def getMentionsFromEntry(entry: FriesEntry): Vector[Mention] = rs.extractFrom(entry).toVector

  def getMentionsFromPaper(file: File): Vector[Mention] = readPaper(file)._2

  /** Extract mentions from a single text string. */
  def getMentionsFromText(text: String): Seq[Mention] = rs.extractFrom(text, "", "")

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
