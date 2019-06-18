package org.clulab.reach.context.context_exec
import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.util.parsing.json.JSON
import java.io.{File, PrintWriter}
object Polarity extends App {
  val config = ConfigFactory.load()
  val polarityDir = config.getString("polarityContext.polarityDir")
  val typeOfPaper = config.getString("polarityContext.typeOfPaper")
  val paperList = List("PMC2958340", "PMC2910130", "PMC4236140", "PMC4142739", "PMC4446607", "PMC4092102", "PMC2587086", "PMC3138418", "PMC3666248", "PMC2636845", "PMC3635065", "PMC3640659", "PMC2686753", "PMC3119364")
  val header = List("PMCID","evtID","ctxID","polarity_type","evt_sentence")
  val polFileName = polarityDir.concat("polarity_instances.txt")
  val polarityFile = new File(polFileName)
  if (!polarityFile.exists())
    polarityFile.createNewFile()
  val printWriter = new PrintWriter(polarityFile)
  printWriter.write(header.mkString(","))
  printWriter.write("\n")
  val linePerRow = collection.mutable.ListBuffer[String]()
  for(pmcid<-paperList) {
    val currentFile = new File(config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/${pmcid}/polarity_output.txt"))
    val lines = Source.fromFile(currentFile).getLines()
    linePerRow ++= lines
  }

  for(l<- linePerRow) {
    printWriter.write(l)
    printWriter.write("\n")
  }

}
