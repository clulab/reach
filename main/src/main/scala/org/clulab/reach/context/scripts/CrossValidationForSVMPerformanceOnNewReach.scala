package org.clulab.reach.context.scripts
import java.io.{File}
import com.typesafe.config.ConfigFactory

object CrossValidationForSVMPerformanceOnNewReach extends App {
  val config = ConfigFactory.load()
  val reach2019RootDir = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
  val dirInstance = new File(reach2019RootDir)
  val paperDirs = dirInstance.listFiles().filter(_.isDirectory)
  print(paperDirs.size)

}
