package edu.arizona.sista.reach.grounding2

import java.io._
import java.util.zip.GZIPInputStream

import scala.io.Source

import edu.arizona.sista.reach.grounding2.LocalKBConstants._

/**
  * Support methods for writing local KB accessors.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Refactor KB loading and key methods elsewhere.
  */
object LocalKBUtils {

  // def readAndFillKB (memKB:InMemoryKB, kbFilename:String) = {
  //   val kbResourcePath = makePathInKBDir(kbFilename)
  //   val source = sourceFromResource(kbResourcePath)
  //   source.getLines.map(tsvRowToFields(_)).filter(validateFields(_)).foreach { fields =>
  //     var text = ""
  //     var species = ""
  //     var refId = ""

  //     if (fields.size == 3) {               // with species
  //       text = fields(0)
  //       species = fields(1)
  //       refId = fields(2)
  //     }
  //     else if (fields.size == 2) {          // w/o species
  //       text = fields(0)
  //       refId = fields(1)
  //     }
  //     else                                  // should never happen if validation works
  //       throw new Exception(s"BAD INPUT: validation failed to spot missing required fields: ${fields}")

  //     // make new key and entry for the KB:
  //     val key = makeKBCanonKey(text)        // make canonical storage key
  //     val newEntry =
  //       if (species == "")
  //         new KBEntry(text, key, refId)
  //       else
  //         new KBEntry(text, key, refId, Some(species))

  //     // fetch any existing entry with the same key and take action accordingly
  //     val entry = memKB.get(key)            // look for existing entry
  //     if (entry.isDefined) {                // if entry is already in this KB
  //       val selected = compareAndSelect(entry.get, newEntry)
  //       if (selected.isDefined)
  //         memKB(key) = newEntry             // overwrite old entry with selected one
  //       // else newEntry is totally ignored
  //     }
  //     else                                  // key not seen before
  //       memKB(key) = newEntry               // add new entry to KB
  //   }
  //   source.close()
  // }


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
