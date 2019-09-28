package org.clulab.reach.context.utils.io_utils

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import org.clulab.learning.RVFDataset

import scala.collection.mutable

object SVMDataTypeIOUtils {

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

}
