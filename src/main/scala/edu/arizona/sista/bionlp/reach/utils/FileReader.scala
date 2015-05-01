package edu.arizona.sista.bionlp.reach.utils

import java.io._
import java.util.zip.GZIPInputStream

import scala.collection.JavaConverters._
import org.apache.commons.io.{ FileUtils, FilenameUtils }

/**
 * Created by gus
 */

// see http://stackoverflow.com/questions/17436549/uncompress-and-read-gzip-file-in-scala
case class BufferedReaderIterator(reader: BufferedReader)
  extends Iterator[String] {
 override def hasNext() = reader.ready
 override def next() = reader.readLine()
}

object FileReader {

 def isGZFile(f: File) = FilenameUtils.getExtension(f.toString) == "gz"


 def readGZFile(gzf: File):Iterator[String] = {
  val stream =
   new BufferedReaderIterator(
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
}
