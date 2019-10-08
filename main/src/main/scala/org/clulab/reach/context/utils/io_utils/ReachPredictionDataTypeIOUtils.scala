package org.clulab.reach.context.utils.io_utils

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.learning.RVFDataset
import org.clulab.reach.context.feature_utils.ContextFeatureUtils

import scala.collection.mutable

object ReachPredictionDataTypeIOUtils {

  def readRVFDatasetFromFile(fileName: String): RVFDataset[Int, String] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val dataset = is.readObject.asInstanceOf[RVFDataset[Int, String]]
    dataset
  }

  def writeAllFeaturesToFile(allFeatures:Seq[String], fileName:String):Seq[String] = {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    val str = new mutable.StringBuilder()
    for(i<- 0 until allFeatures.size - 1) {
      val current = allFeatures(i)
      str.append(current+",")
    }
    str.append(allFeatures.last)
    val stringEquiv = str.toString()
    val arr = stringEquiv.split(",")
    os.writeObject(arr.asInstanceOf[Array[String]])
    os.close()
    allFeatures
  }

  def readAggrRowsWithSpecsFromFile(pathToRows:String):(Map[String,Seq[AggregatedContextInstance]], Map[String,Seq[(String,String,String)]], Map[AggregatedContextInstance, (String,String,String)]) = {
    val paperIDByNewRows = collection.mutable.ListBuffer[(String, Seq[AggregatedContextInstance])]()
    val paperIDByRowSpecs = collection.mutable.ListBuffer[(String,Seq[(String,String,String)])]()
    val rowByID = collection.mutable.ListBuffer[(AggregatedContextInstance,(String,String,String))]()
    val parentDirfileInstanceToLoadNewRows = new File(pathToRows)
    val paperDirs = parentDirfileInstanceToLoadNewRows.listFiles().filter(x => x.isDirectory && x.getName.startsWith("PMC"))
    for (paperDir <- paperDirs) {

      val listOfRowsInPaper = collection.mutable.ListBuffer[AggregatedContextInstance]()
      val listOfSpecsInPaper = collection.mutable.ListBuffer[(String,String,String)]()
      val paperID = paperDir.getName
      val rowFilesInThisPaper = paperDir.listFiles().filter(_.getName.startsWith("Aggreg"))
      for(rowFile <- rowFilesInThisPaper) {
        val rowSpecs = ContextFeatureUtils.createAggRowSpecsFromFile(rowFile)
        val row = ContextFeatureUtils.readAggRowFromFile(rowFile)
        if(!listOfRowsInPaper.contains(row)) {
          listOfRowsInPaper += row
          listOfSpecsInPaper += rowSpecs
          val rowMappedToSpecs = (row,rowSpecs)
          rowByID += rowMappedToSpecs

        }
      }

      val tuple2 = (paperID, listOfRowsInPaper)
      paperIDByNewRows += tuple2
      val tuple2Specs = (paperID,listOfSpecsInPaper)
      paperIDByRowSpecs += tuple2Specs

    }

    (paperIDByNewRows.toMap,paperIDByRowSpecs.toMap, rowByID.toMap)
  }



}
