package edu.arizona.sista.reach.grounding

import java.io._
import java.util.zip.GZIPInputStream

import scala.io.Source

import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Support methods for writing local KB accessors.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Move tsvRowToFields method here.
  */
object ReachKBUtils {

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

  /** Convert a single row string from a TSV file to a sequence of string fields. */
  def tsvRowToFields (row:String): Seq[String] = {
    return row.split("\t").map(_.trim)
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
