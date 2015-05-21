package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import java.io._
import java.util.zip.GZIPInputStream

import scala.io.Source

/**
  * Support methods for writing local KB accessors.
  *   Written by Tom Hicks. 4/16/2015.
  *   Last Modified: Remove redundant key transform methods.
  */
object LocalKBUtils {

  /** The set of characters to remove from the text to create a lookup key. */
  val KeyCharactersToRemove = " /-".toSet

  /** The set of words to remove from the text to create a lookup key. */
  val KeyStopSuffixes = Set("_human")

  /** The set of words to remove from the text to create a lookup key. */
  val HumanLabels = Set("homo sapiens", "human")


  /** Tell whether the given species string is label for humans or not. */
  def isHumanSpecies (species: String): Boolean = {
    if (HumanLabels.contains(species.toLowerCase)) true else false
  }

  /** Canonicalize the given text string into a key for both storage and lookup. */
  def makeKBCanonKey (text:String): String = {
    var key:String = text.toLowerCase
    // KeyStopWords.foreach { word => key = key.replaceAll(word, "") }
    key = key.filterNot(KeyCharactersToRemove)
    KeyStopSuffixes.foreach { suffix =>
      key = key.stripSuffix(suffix)
    }
    return key
  }


  /** Return a Scala Source object created from the given resource path string. If the
    * resource path ends with ".gz" the source is created around a gzip input stream. */
  def sourceFromResource (resourcePath:String): Source = {
    val inStream = this.getClass.getResourceAsStream(resourcePath)
    if (resourcePath.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(inStream)))
    else
      Source.fromInputStream(inStream)
  }

  /** Return a file path for the given filename in the current user working directory. */
  def makeOutputFileInUserDir (filename:String): File = {
    new File(System.getProperty("user.dir") + File.separator + filename)
  }

}


/** Class to implement an incrementing counter for generating unique IDs. */
class IncrementingCounter {
  protected var cntr:Int = 0
  def current(): Int = { cntr }
  def next(): Int = {
    cntr += 1
    return cntr
  }
}
