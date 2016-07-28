package org.clulab.reach

import java.io._
import org.clulab.reach.context.ContextEngineFactory.Engine
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FilenameUtils
import org.clulab.odin._
import org.clulab.reach.utils.DSVParser
import org.clulab.utils.Serializer
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray
import ai.lum.nxmlreader.{NxmlDocument, NxmlReader}
import org.clulab.reach.nxml.FriesEntry


object PaperReader {

  type PaperId = String
  type Dataset = Map[PaperId, Vector[Mention]]

  println("loading ...")
  val config = ConfigFactory.load()
  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")
  val ignoreSections = config.getStringList("nxml2fries.ignoreSections").asScala

  // systems for reading papers
  val nxmlReader = new NxmlReader(ignoreSections.toSet)
  val dsvReader = new DSVParser()

  // for context engine
  val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
  val contextConfig = config.getConfig("contextEngine.params").root
  val contextEngineParams: Map[String, String] =
    context.createContextEngineParams(contextConfig)

  // initialize ReachSystem with appropriate context engine
  val rs = new ReachSystem(contextEngineType = contextEngineType, contextParams = contextEngineParams)

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
    val files = dir.listFiles.par
    // limit parallelization
    files.tasksupport =
      new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))
    // build dataset
    val data: ParArray[(String, Vector[Mention])] = for {
      file <- dir.listFiles.par // read papers in parallel
      // allow either nxml or csv files
      if file.getName.endsWith(".nxml") || file.getName.endsWith(".csv")
    } yield readPaper(file)
    data.seq.toMap
  }

  /**
    * Produces Mentions from either a .nxml or .csv/.tsv paper using [[NxmlReader]] or [[DSVParser]] and [[ReachSystem]]
    * @param file a File with either the .csv, .tsv, or .nxml extension
    * @return (PaperID, Mentions)
    */
  def readPaper(file: File): (String, Vector[Mention]) = file match {
    case nxml if nxml.getName.endsWith(".nxml") =>
      readNXMLPaper(nxml)
    case dsv if dsv.getName.endsWith(".csv") || dsv.getName.endsWith("tsv") =>
      readDSVPaper(dsv)
    case other =>
      throw new Exception(s"Given ${file.getAbsolutePath}, but readPaper doesn't support ${FilenameUtils.getExtension(other.getAbsolutePath)}")
  }

  def readNXMLPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".nxml"), s"Given ${file.getAbsolutePath}, but readNXMLPaper only handles .nxml files!")
    val paperID = FilenameUtils.removeExtension(file.getName)
    //info(s"reading paper $paperID . . .")
    paperID -> rs.extractFrom(nxmlReader.read(file)).toVector
  }

  def readDSVPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".tsv") || file.getName.endsWith(".csv"), s"Given ${file.getAbsolutePath}, but readDSVPaper only handles .tsv and .dsv files!")
    val paperID = FilenameUtils.removeExtension(file.getName)
    //info(s"reading paper $paperID . . .")
    paperID -> rs.extractFrom(dsvReader.toFriesEntry(file)).toVector
  }

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
      dsvReader.toFriesEntry(dsv)
  }

  def getMentionsFromEntry(entry: FriesEntry): Vector[Mention] = rs.extractFrom(entry).toVector

  def getMentionsFromPaper(file: File): Vector[Mention] = {
    readPaper(file)._2
  }

  /**
   * Get mentions from text
   */
  def getMentionsFromText(text: String): Seq[Mention] = rs.extractFrom(text, "", "")

}


object ReadPapers extends App {

  val config = ConfigFactory.load()

  val papersDir = config.getString("ReadPapers.papersDir")
  val outFile = config.getString("ReadPapers.serializedPapers")

  println("reading papers ...")
  val dataset = PaperReader.readPapers(papersDir)

  println("serializing ...")
  Serializer.save(dataset, outFile)
}
