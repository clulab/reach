package org.clulab.reach.apis

import java.util.{List => JList}

import com.typesafe.config.ConfigFactory
import org.clulab.reach._
import org.clulab.reach.mentions._
import org.clulab.reach.nxml.{FriesEntry, NxmlReader}

import scala.collection.JavaConverters._

/**
  * External interface class to accept and process text strings and NXML documents,
  * returning Reach results as a sequence of BioMentions.
  *   Author: Tom Hicks. 10/19/2015.
  *   Last Modified: Initial creation, after api ruler class.
  */
object Api {
  // Reach results for Scala consumption are a sequence of BioMentions
  type ReachResults = Seq[BioMention]

  // Reach results for Java consumption are a java.util.List of BioMentions
  type JReachResults = JList[BioMention]

  // some internal defaults for parameters required in lower layers
  private val NoSec = "NoSection"
  private val Prefix = "api"
  private val Suffix = "Reach"

  // read configuration to determine processing parameters
  val config = ConfigFactory.load()
  val ignoreSections = config.getStringList("nxml2fries.ignoreSections").asScala.toList
  val encoding = config.getString("encoding")

  val reader = new NxmlReader(ignoreSections)

  val reach = new ReachSystem               // start reach system

  //
  // Scala API
  //

  /** Extracts raw text from given nxml string and returns Reach results. */
  def runOnNxml (nxml: String): ReachResults = {
    val entries = reader.readNxml(nxml, Prefix)
    entries flatMap extractMentions
  }

  /** Annotates text by converting it to a FriesEntry and calling runOnFriesEntry().
      Uses fake document ID and chunk ID. */
  def runOnText (text: String): ReachResults =
    runOnFriesEntry(FriesEntry(Prefix, Suffix, NoSec, NoSec, false, text))

  /** Annotates text by converting it to a FriesEntry and calling runOnFriesEntry(). */
  def runOnText (text: String, docId: String=Prefix, chunkId: String=Suffix): ReachResults =
    runOnFriesEntry(FriesEntry(docId, chunkId, NoSec, NoSec, false, text))

  /** Annotates a single FriesEntry and returns Reach results. */
  def runOnFriesEntry (entry: FriesEntry): ReachResults =
    extractMentions(entry)


  //
  // Java API
  //

  /** Extracts raw text from given nxml string and returns Java Reach results. */
  def runOnNxmlToJava (nxml: String): JReachResults = {
    val entries = reader.readNxml(nxml, Prefix)
    (entries flatMap extractMentions).asJava
  }

  /** Annotates text by converting it to a FriesEntry and calling
      runOnFriesEntryToJava(). Uses fake document ID and chunk ID. */
  def runOnTextToJava (text: String): JReachResults =
    runOnFriesEntryToJava(FriesEntry(Prefix, Suffix, NoSec, NoSec, false, text))

  /** Annotates text by converting it to a FriesEntry and calling
      runOnFriesEntryToJava(). */
  def runOnTextToJava (text: String, docId: String=Prefix, chunkId: String=Suffix): JReachResults =
    runOnFriesEntryToJava(FriesEntry(docId, chunkId, NoSec, NoSec, false, text))

  /** Annotates a single FriesEntry and returns Reach results. */
  def runOnFriesEntryToJava (entry: FriesEntry): JReachResults =
    extractMentions(entry).asJava


  /** Extracts the mentions from a FriesEntry to Reach results. */
  private def extractMentions(entry: FriesEntry): ReachResults = reach.extractFrom(entry)

}
