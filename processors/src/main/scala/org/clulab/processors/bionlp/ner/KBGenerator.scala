package org.clulab.processors.bionlp.ner

import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigFactory

import scala.collection.convert.ImplicitConversions._
import scala.collection.JavaConverters.asScalaBufferConverter
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.mutable
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.sequences.StandardKbSource
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

    val configuredKBs:Seq[(String, String, Set[String])] = {
      val conf = ConfigFactory.load()
      val kbConf = conf.getConfig("KnowledgeBases")

      val loadedKBs = mutable.ListBuffer[(String, String, Int, Set[String])]()
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

          val species =
            if(c.hasPath("species"))
              c.getStringList("species").toSet
            else
              Set.empty[String]

          loadedKBs ++= (labels map (l => (l, fileName, priority, species)))
        case _ =>
          throw new RuntimeException("Error in the configuration file of bioresources")
      }

      // Make them a dictionary where the key is label and the value are the references to KBs with this label
//      loadedKBs groupBy {
//        case (label, _, _, _) => label
//      } mapValues {
//        _.sortBy{
//          case (label, path, priority, _) =>
//            priority
//        }.reverse.map{
//          case (_, path, _, species) => (path, species)
//        }.toList
//      }

      loadedKBs.sortBy{
        case (_, _, priority, _) => priority
      }.map {
        case (label, path, _, species) => (label, path, species)
      }.toList
    }

    val entries =
      for{
        (label, path, species) <- configuredKBs
      } yield {
        val name = new File(path).getName.split("\\.").dropRight(1).mkString("")
        KBEntry(label, path, label, species)
      }

    entries.toList
  }

  def processKBFiles(caseInsensitiveMatching: Boolean): Seq[StandardKbSource] = {
    val kbEntries = loadFromConf()
    // This should only be performed on small collections.
    // It is important to keep both the labels and the KbEntries in the same order.
    val actualLabels = kbEntries.map(_.neLabel).distinct
    val expectedLabels = Seq("Gene_or_gene_product", "Family", "Cellular_component", "Simple_chemical", "Site", "BioProcess", "Disease", "Species", "CellLine", "TissueType", "CellType", "Organ")
    val kbEntriesSeq = expectedLabels.map { label => kbEntries.filter(_.neLabel == label) }
    val standardKbSources = kbEntriesSeq.map(new ReachMultiStandardKbSource(_, caseInsensitiveMatching))

    standardKbSources
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
