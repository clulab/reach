package org.clulab.reach.context

import java.io.{File, FileInputStream, ObjectInputStream}

import com.typesafe.config.ConfigFactory
import org.ml4ai.data.classifiers.LinearSVMWrapper
import org.ml4ai.data.utils.AggregatedRow

import scala.io.Source

object SVMCrossValidation extends App {

  val config = ConfigFactory.load()
  val configPath = config.getString("contextEngine.params.untrainedSVMPath")
  val svmWrapper = new LinearSVMWrapper(null)
  val unTrainedSVMInstance = svmWrapper.loadFrom(configPath)
  val labelFile = config.getString("svmContext.labelFile")
  val typeOfPaper = config.getString("svmContext.paperType")
  val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/")
  val fileListUnfiltered = new File(outPaperDirPath)
  val directories = fileListUnfiltered.listFiles().filter(_.isDirectory)
  val rowsSup = collection.mutable.ArrayBuffer[AggregatedRow]()
  for(d<-directories) {
    val rowFiles = d.listFiles()

    val rows = rowFiles.map(file => {
      val pmcid = file.getName.split("_")(1)
      val filePath = outPaperDirPath.concat(pmcid).concat(s"/${file.getName}")
      readAggRowFromFile(filePath)
    })
    rowsSup ++= rows


    //arrayOfAggRows ++= rows

  }

  val pmcid = rowsSup.map(r => s"PMC${r.PMCID.split("_")(0)}")
  val zip = pmcid zip rowsSup
  val map = zip.groupBy(_._1)
  println(map.size)


  def readAggRowFromFile(file: String):AggregatedRow = {
    val is = new ObjectInputStream(new FileInputStream(file))
    val c = is.readObject().asInstanceOf[AggregatedRow]
    is.close()
    c
  }


  def generateLabelMap(fileName: String): Map[(String,String,String), Int] = {
    val map = collection.mutable.HashMap[(String,String,String), Int]()
    val source = Source.fromFile(fileName)
    val lines = source.getLines()
    val content = lines.drop(1)
    for(c <- content) {
      val array = c.split(",")
      val pmcid = array(0)
      val evtID = array(1)
      val ctxID = array(2)
      val label = Integer.parseInt(array(3))
      val tup = (pmcid,evtID,ctxID)
      map ++= Map(tup -> label)
    }

    map.toMap
  }
}
