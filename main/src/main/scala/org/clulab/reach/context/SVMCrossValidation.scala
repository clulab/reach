package org.clulab.reach.context

import com.typesafe.config.ConfigFactory
import org.ml4ai.data.classifiers.LinearSVMWrapper

import scala.io.Source

object SVMCrossValidation extends App {

  val config = ConfigFactory.load()
  val configPath = config.getString("contextEngine.params.untrainedSVMPath")
  val svmWrapper = new LinearSVMWrapper(null)
  val unTrainedSVMInstance = svmWrapper.loadFrom(configPath)
  val labelFile = config.getString("svmContext.labelFile")

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
