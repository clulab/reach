package edu.arizona.sista.reach

import java.io._
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FilenameUtils
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.nxml.NxmlReader
import edu.arizona.sista.utils.Serializer

import scala.collection.parallel.ForkJoinTaskSupport

object PaperReader extends App {

  type PaperId = String
  type Dataset = Map[PaperId, Vector[Mention]]

  println("loading ...")
  val config = ConfigFactory.load()
  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")
  val papersDir = config.getString("PaperReader.papersDir")
  val outFile = config.getString("PaperReader.serializedPapers")
  val ignoreSections = config.getStringList("nxml2fries.ignoreSections").asScala
  val nxmlReader = new NxmlReader(ignoreSections)
  val reach = new ReachSystem

  println("reading papers ...")
  val dataset = readPapers(papersDir)

  println("serializing ...")
  Serializer.save(dataset, outFile)

  def readPapers(path: String): Dataset = readPapers(new File(path))

  def readPapers(dir: File): Dataset = {
    require(dir.isDirectory, s"'${dir.getCanonicalPath}' is not a directory")
    // read papers in parallel
    val files = dir.listFiles.par
    // limit parallelization
    files.tasksupport =
      new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))
    val data = for {
      file <- dir.listFiles.par // read papers in parallel
      if file.getName.endsWith(".nxml")
    } yield readPaper(file)
    data.seq.toMap
  }

  def readPaper(file: File): (String, Vector[Mention]) = {
    val paperId = FilenameUtils.removeExtension(file.getName)
    println(s"reading paper $paperId ...")
    val mentions = for {
      entry <- nxmlReader.readNxml(file)
      mention <- reach.extractFrom(entry)
    } yield mention
    paperId -> mentions.toVector
  }

}
