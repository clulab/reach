package org.clulab.reach.context.context_exec

import java.io.{FileInputStream, ObjectInputStream}

import com.typesafe.config.ConfigFactory

object PerformPolarityAnalysis extends App {

  val config = ConfigFactory.load()
  val operatingDir = config.getString("polarityContext.contextLabelsOutputDir")
  val activationLabelsFilePath = operatingDir.concat("activationContextLabels.txt")
  val inhibitionLabelsFilePath = operatingDir.concat("inhibitionContextLabels.txt")
  val activationInputStream = new ObjectInputStream(new FileInputStream(activationLabelsFilePath))
  val inhibitionInputStream = new ObjectInputStream(new FileInputStream(inhibitionLabelsFilePath))
  val activationLabels = activationInputStream.readObject().asInstanceOf[Array[String]]
  val inhibitionLabels = inhibitionInputStream.readObject().asInstanceOf[Array[String]]
  println("********** Non-unique activation labels *************")
  println(activationLabels.mkString(","))

  println("\n ********** Non-unique inhibition labels *************")
  println(inhibitionLabels.mkString(","))


}
