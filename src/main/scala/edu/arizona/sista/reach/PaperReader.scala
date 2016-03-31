package edu.arizona.sista.reach

import java.io._
import edu.arizona.sista.reach.context.ContextEngineFactory.Engine
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FilenameUtils
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.nxml.NxmlReader
import edu.arizona.sista.reach.utils.CSVParser
import edu.arizona.sista.utils.Serializer
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray

object PaperReader {

  type PaperId = String
  type Dataset = Map[PaperId, Vector[Mention]]

  println("loading ...")
  val config = ConfigFactory.load()
  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")
  val ignoreSections = config.getStringList("nxml2fries.ignoreSections").asScala

  // systems for reading papers
  val nxmlReader = new NxmlReader(ignoreSections)
  val csvReader = new CSVParser()

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
   * Produces Mentions from either a .nxml or .csv paper using [[NxmlReader]] or [[CSVParser]] and [[ReachSystem]]
   * @param file a File with either the .csv or .nxml extension
   * @return (PaperID, Mentions)
   */
  def readPaper(file: File): (String, Vector[Mention]) = file match {
    case nxml if nxml.getName.endsWith(".nxml") => readNXMLPaper(nxml)
    case csv if csv.getName.endsWith(".csv") => readCSVPaper(csv)
  }

  /**
   * Produces Mentions from .csv papers using [[CSVParser]] and [[ReachSystem]]
   * @param file a File with the .csv extension
   * @return (PaperID, Mentions)
   */
  def readCSVPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".csv"), s"Given ${file.getAbsolutePath}, but readCSVPaper only handles .csv files!")
    val paperID: String = file.getName.replace(".csv", "")
    println(s"reading paper $paperID ...")
    val mentions: Seq[Mention] =
      rs.extractFrom(csvReader.toFriesEntries(file))
    paperID -> mentions.toVector
  }

  /**
   * Produces Mentions from .nxml papers using [[NxmlReader]] and [[ReachSystem]]
   * @param file a File with the .csv extension
   * @return (PaperID, Mentions)
   */
  def readNXMLPaper(file: File): (String, Vector[Mention]) = {
    require(file.getName.endsWith(".nxml"), s"Given ${file.getAbsolutePath}, but readNXMLPaper only handles .nxml files!")
    val paperID = FilenameUtils.removeExtension(file.getName)
    println(s"reading paper $paperID ...")
    val mentions = for {
      entry <- nxmlReader.readNxml(file)
      mention <- rs.extractFrom(entry)
    } yield mention
    paperID -> mentions.toVector
  }
}


object ReadPapers extends App {

  val config = ConfigFactory.load()

  val papersDir = config.getString("PaperReader.papersDir")
  val outFile = config.getString("PaperReader.serializedPapers")

  println("reading papers ...")
  val dataset = PaperReader.readPapers(papersDir)

  println("serializing ...")
  Serializer.save(dataset, outFile)
}
