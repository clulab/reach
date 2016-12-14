package org.clulab.reach.export

import ai.lum.common.ConfigUtils._
import ai.lum.common.FileUtils._
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils
import org.clulab.reach.PaperReader
import org.clulab.reach.serialization.json._
import scala.collection.parallel.ForkJoinTaskSupport
import java.io.File


/** Built with resuming aborted jobs in mind,
  * PapersToJSON checks to see if the output file for a paper has already been produced
  * before attempting to read the paper
  * */
object PapersToJSON extends App with LazyLogging {

  val config = ConfigFactory.load()

  val papersDir: File = config[File]("papersDir")
  val outDir: File = config[File]("outDir")
  val threadLimit: Int = config[Int]("threadLimit")

  logger.info("reading papers ...")
  val papers = papersDir.listFilesByWildcard("*.nxml").par

  papers.tasksupport =
    new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))

  if (!outDir.exists) {
    logger.info(s"Creating output directory: ${outDir.getCanonicalPath}")
    FileUtils.forceMkdir(outDir)
  }

  for {
    p <- papers
    // the PMID
    paperID = p.getBaseName()
    outFile = new File(outDir, s"$paperID-mentions.json")
    if !outFile.exists
  } {
    logger.info(s"reading $paperID")
    val mns = PaperReader.getMentionsFromPaper(p)
    mns.saveJSON(outFile, pretty = true)
    logger.info(s"serialized $paperID to ${outFile.getAbsolutePath}")
  }
}