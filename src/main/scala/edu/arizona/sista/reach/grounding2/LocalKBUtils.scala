package edu.arizona.sista.reach.grounding2

import java.io._
import java.util.zip.GZIPInputStream

import scala.io.Source

import edu.arizona.sista.reach.grounding2.LocalKBConstants._

/**
  * Support methods for writing local KB accessors.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Refactor meta information to in-memory KB.
  */
object LocalKBUtils {

  /** The type for all in-memory data structures representing local knowledge bases. */
  type InMemoryKB = scala.collection.mutable.Map[String, KBEntry]
  def  InMemoryKB():InMemoryKB = scala.collection.mutable.Map[String, KBEntry]()


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


  def compareAndSelect (oldEntry:KBEntry, newEntry:KBEntry): Option[KBEntry] = {
    return None                             // TODO: IMPLEMENT LATER
  }

  /** Convert a single row string from a TSV file to a sequence of string fields. */
  def tsvRowToFields (row:String): Seq[String] = {
    return row.split("\t").map(_.trim)
  }


  def validateFields (fields:Seq[String]): Boolean = {
    return ((fields.size == 3) && fields(0).nonEmpty && fields(2).nonEmpty) ||
           ((fields.size == 2) && fields(0).nonEmpty && fields(1).nonEmpty)
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
