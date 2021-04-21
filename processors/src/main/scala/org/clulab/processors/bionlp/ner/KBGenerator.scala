package org.clulab.processors.bionlp.ner

import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigFactory
import scala.collection.convert.ImplicitConversions._
import scala.collection.JavaConverters.asScalaBufferConverter

import java.io._
import java.text.SimpleDateFormat
import java.util.Date
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.utils.ScienceUtils
import org.slf4j.{Logger, LoggerFactory}

import java.io.File

case class KBEntry(kbName:String, path:String, neLabel:String, validSpecies:Set[String])

/**
  * This is used in bioresources to format the data into a format that is easy to load at runtime
  * User: mihais
  * Date: 2/7/16
  * Last Modified: Fix compiler issue: import scala.io.Source.
  */
object KBGenerator {
  val logger: Logger = LoggerFactory.getLogger(classOf[BioNLPProcessor])
  val su = new ScienceUtils

  val NAME_FIELD_NDX = 0                    // config column containing the KB name
  val LABEL_FIELD_NDX = 1                   // config column containing the type label
  val SPECIES_FIELD_NDX = 2                 // KB column containing the species of the name entity


  def loadFromConf():Seq[KBEntry] = {

    val configuredKBs:Map[String, Seq[String]] = {
      val conf = ConfigFactory.load()
      val kbConf = conf.getConfig("KnowledgeBases")

      val loadedKBs = mutable.ListBuffer[(String, String, Int)]()
      // Load all the KBs specified in the configuraction file of bioresources
      kbConf.root() foreach  {
        case (_, obj:ConfigObject) =>
          val c = obj.toConfig

          val labels =
            if(c.hasPath("labels"))
              c.getStringList("labels").asScala
            else
              List("BioEntity")

          val fileName = c.getString("path")

          val priority =
            if(c.hasPath("priority"))
              c.getInt("priority")
            else
              1

          loadedKBs ++= (labels map (l => (l, fileName, priority)))
        case _ =>
          throw new RuntimeException("Error in the configuration file of bioresources")
      }

      // Make them a dictionary where the key is label and the value are the references to KBs with this label
      loadedKBs groupBy {
        case (label, _, _) => label
      } mapValues {
        _.sortBy{
          case (label, path, priority) =>
            priority
        }.reverse.map{
          case (_, path, _) => path
        }.toList
      }
    }

    val entries =
      for{
        (label, paths) <- configuredKBs
        path <- paths
      } yield {
        val name = new File(path).getName.split("\\.").dropRight(1).mkString("")
        KBEntry(label, path, label, Set.empty[String])
      }

    entries.toList
  }

  def processKBFiles():Map[String, Seq[String]] = {

//    val entries = loadConfig(configFile)
    val entries = loadFromConf()
    logger.info(s"Will convert a total of ${entries.size} KBs:")

    (for(entry <- entries) yield {
      val processedLines = convertKB(entry)
      entry.kbName -> processedLines
    }).toMap
  }

  def convertKB(entry:KBEntry): Seq[String] = {
    logger.info(s"Loading ${entry.kbName}...")
    val inputPath = entry.path//inputDir + File.separator + entry.kbName + ".tsv"
    val b =
      if(new File(inputPath).exists())
        new BufferedReader(new InputStreamReader(new FileInputStream(inputPath)))
      else
        new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(inputPath + ".gz"))))

    val tokenizer: Tokenizer = (new BioNLPProcessor).tokenizer

    var done = false
    var lineCount = 0
    val outputLines = new ArrayBuffer[String]()
    while(! done) {
      val line = b.readLine()
      if(line == null) {
        done = true
      } else {
        val trimmedLine = line.trim
        if(trimmedLine.nonEmpty && ! trimmedLine.startsWith("#")) { // skip comments
          val kbTokens = line.split("\t")
          if(containsValidSpecies(entry, kbTokens)) { // this is a protein from a species we want
            lineCount += 1
            val ne = kbTokens(0) // we enforce that the first token is the actual NE to be considered
            val tokens = tokenizeResourceLine(ne, tokenizer) // tokenize using BioNLPProcessor
            outputLines += tokens.mkString(" ")
          }
        }
      }
    }
    b.close()


    val uniqLines = outputLines
      .filter(_.nonEmpty)
      .sorted
      .distinct

    logger.info(s"Done. Read $lineCount lines (${uniqLines.size} distinct) from ${entry.kbName}")
    uniqLines
  }

  def now:String = {
    val dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
    val date = new Date()
    dateFormat.format(date)
  }

  /**
    * Tokenizes a resource line with BioNLPProcessor
    * This is important! We must guarantee that KB text is processed similarly to raw text!
    *
    * @param line The KB line
    * @return The tokenized line
    */
  def tokenizeResourceLine(line:String, tokenizer:Tokenizer):Array[String] = {
    val text = su.replaceUnicodeWithAscii(line)
    val origTokens = tokenizer.tokenize(text, sentenceSplit = false).head.words
    origTokens
  }

  def containsValidSpecies(entry:KBEntry, tokens:Array[String]):Boolean = {
    if(entry.validSpecies.isEmpty)
      return true

    // if mentioned, the species must be a token at the correct zero-indexed position.
    if(tokens.length < 3 || entry.validSpecies.contains(tokens(SPECIES_FIELD_NDX)))
      return true

    false
  }
}
