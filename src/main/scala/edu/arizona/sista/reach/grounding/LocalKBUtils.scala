package edu.arizona.sista.reach.grounding

import java.io._
import java.util.zip.GZIPInputStream

import scala.io.Source

/**
  * Support methods for writing local KB accessors.
  *   Written by Tom Hicks. 4/16/2015.
  *   Last Modified: Refactor: separate out method to strip a suffix.
  */
object LocalKBUtils extends KnowledgeBaseConstants {

  /** Tell whether the given species string is label for humans or not. */
  def isHumanSpecies (species: String): Boolean = {
    if (HumanLabels.contains(species.toLowerCase)) true else false
  }


  /** Canonicalize the given text string into a key for both storage and lookup. */
  def makeKBCanonKey (text:String): String = {
    var key:String = text.toLowerCase
    // KeyStopWords.foreach { word => key = key.replaceAll(word, "") }
    key = key.filterNot(KeyCharactersToRemove)
    return stripASuffix(AllKeysStopSuffixes, key)
  }

  /** Try to remove one of the suffixes in the given set from the given text. */
  def stripASuffix (suffixes:Set[String], text:String): String = {
    var key = text
    suffixes.foreach { suffix =>
      key = key.stripSuffix(suffix)
    }
    return key
  }


  /** Return a Scala Source object created from the given resource path string. If the
    * resource path ends with ".gz" the source is created around a gzip input stream. */
  def sourceFromResource (resourcePath:String): Source = {
    val inStream = this.getClass.getResourceAsStream(resourcePath)
    if (resourcePath.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(inStream)), "utf8")
    else
      Source.fromInputStream(inStream, "utf8")
  }


  /** Return a file for the given filename in the knowledge bases directory. */
  def makeFileInKBDir (filename:String): File = {
    return new File(KBDirFilePath + File.separator + filename)
  }

  /** Return a resource path string for the given filename in the knowledge bases directory. */
  def makePathInKBDir (filename:String): String = {
    return KBDirResourcePath + File.separator + filename
  }

  /** Return a file for the given filename in the current user working directory. */
  def makeFileInUserDir (filename:String): File = {
    return new File(makePathInUserDir(filename))
  }

  /** Return a path string for the given filename in the current user working directory. */
  def makePathInUserDir (filename:String): String = {
    return System.getProperty("user.dir") + File.separator + filename
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
