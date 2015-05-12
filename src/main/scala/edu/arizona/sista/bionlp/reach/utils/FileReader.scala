package edu.arizona.sista.bionlp.reach.utils

import java.io._
import java.util.zip.GZIPInputStream

import scala.collection.JavaConverters._
import org.apache.commons.io.{ FileUtils, FilenameUtils }

/**
 * Created by gus
 */

// see http://stackoverflow.com/questions/17436549/uncompress-and-read-gzip-file-in-scala
case class BRI(reader: BufferedReader)
  extends Iterator[String] {
 override def hasNext() = reader.ready
 override def next() = reader.readLine()
}

object FileReader {

 /**
  * Tests whether a File is a .gz file based on the extension
  * @param f a File object
  * @return true or false
  */
 def isGZFile(f: File) = FilenameUtils.getExtension(f.toString) == "gz"

 /**
  * Turn a .gz File object into an Iterator
  * @param gzf
  * @return a line-by-line iterator
  */
 def readGZFile(gzf: File):Iterator[String] = {
  val stream =
   new BRI(
    new BufferedReader(
     new InputStreamReader(
      new GZIPInputStream(
       new FileInputStream(gzf)))))
  stream
 }

 /**
  * Create an Iterator from some File (including .gz)
  * @param f is a File object
  * @return Returns an Iterator containing the lines of a File
  */
 def readFile(f: File):Iterator[String] = f match {

  case gzf if isGZFile(gzf) => readGZFile(gzf)
  case _ => FileUtils.lineIterator(f).asScala

 }

 /**
  * Removes extension from filename (Apache Commons seemed to have trouble with .tsv)
  * @param f a File object
  * @return a String representation of the File name without its extension
  */
 def removeExtension(f: File): String = {
  val fname = f.getName
  fname.toCharArray.takeWhile(_ != '.').mkString("")
 }
}
