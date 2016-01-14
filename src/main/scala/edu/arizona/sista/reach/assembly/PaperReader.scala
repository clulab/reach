package edu.arizona.sista.reach.assembly

import java.io._
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FilenameUtils
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.nxml.NxmlReader
import edu.arizona.sista.reach.ReachSystem
import edu.arizona.sista.processors.Document
import edu.arizona.sista.utils.ClassLoaderObjectInputStream

object PaperReader extends App {

  type PaperId = String
  type Dataset = Map[PaperId, Vector[Mention]]

  println("loading ...")
  val config = ConfigFactory.load()
  val papersDir = "src/test/resources/inputs/nxml/"
  val outFile = "mentions.ser"
  val ignoreSections = config.getStringList("nxml2fries.ignoreSections").asScala
  val nxmlReader = new NxmlReader(ignoreSections)
  val reach = new ReachSystem

  println("reading papers ...")
  val dataset = readPapers(papersDir)

  println("serializing ...")
  save(dataset, outFile)

  def readPapers(path: String): Dataset = readPapers(new File(path))

  def readPapers(dir: File): Dataset = {
    require(dir.isDirectory, s"'${dir.getCanonicalPath}' is not a directory")
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

  def save(data: Dataset, path: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(path))
    oos.writeObject(data)
    oos.close()
  }

  def load(path: String): Dataset = {
    val cl = getClass().getClassLoader()
    val ois = new ClassLoaderObjectInputStream(cl, new FileInputStream(path))
    val data = ois.readObject().asInstanceOf[Dataset]
    ois.close()
    data
  }

}
