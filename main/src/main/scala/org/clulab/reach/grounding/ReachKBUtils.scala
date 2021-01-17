package org.clulab.reach.grounding

import java.io._
import java.util.concurrent.atomic.AtomicInteger
import java.util.zip.GZIPInputStream
import scala.io.Source
import org.clulab.reach.mentions._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.Speciated._


/**
  * Support methods for writing local KB accessors.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Modify methods to use species name sets.
  */
object ReachKBUtils {

  /** Tell whether the given mention is grounded from a protein family KB or not. */
  def isFamilyGrounded (mention:BioMention): Boolean =
    mention.isGrounded && mention.grounding.get.metaInfo.isFamilyKB

  /** Tell whether the given mention is grounded from a protein KB or not. */
  def isProteinGrounded (mention:BioMention): Boolean =
    mention.isGrounded && mention.grounding.get.metaInfo.isProteinKB


  /** Return a formatted string containing this entry's namespace and ID. */
  def makeNamespaceId (namespace:String, id:String): String =
    s"${namespace.trim.toLowerCase}${NamespaceIdSeparator}${id.trim}"


  /** Return a file for the given filename in the knowledge bases directory. */
  def makeFileInKBDir (filename:String): File = {
    return new File(KBDirFilePath + File.separator + filename)
  }

  /** Return a resource path string for the given filename in the knowledge bases directory. */
  def makePathInKBDir (filename:String): String = {
    // Resource paths use "/" instead of File.separator.  File.separator will break this under Windows.
    return KBDirResourcePath + "/" + filename
  }

  /** Return a file for the given filename in the current user working directory. */
  def makeFileInUserDir (filename:String): File = {
    return new File(makePathInUserDir(filename))
  }

  /** Return a path string for the given filename in the current user working directory. */
  def makePathInUserDir (filename:String): String = {
    return System.getProperty("user.dir") + File.separator + filename
  }

  /** Read and return all the lines from the specified file. */
  def readLines (filename:String): List[String] = {
    val kbResourcePath = makePathInKBDir(filename)
    val source = sourceFromResource(kbResourcePath)
    val lines = source.getLines().toList
    source.close()
    lines
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

  /** Convert a single row string from a TSV file to a sequence of string fields. */
  def tsvRowToFields (row:String): Seq[String] = {
    return row.split("\t").map(_.trim)
  }

  /** Filter sequence to return human resolutions (sorted). */
  def selectHuman (resSeq:Seq[KBResolution]): Seq[KBResolution] =
    resSeq.filter(kbr => isHumanSpecies(kbr.species)).sortBy(kbr => (kbr.species, kbr.id))

  /** Filter sequence to return resolutions which have no species. */
  def selectNoSpecies (resSeq:Seq[KBResolution]): Seq[KBResolution] =
    resSeq.filter(kbr => kbr.hasNoSpecies)

  /** Filter sequence to return only resolutions (sorted) with a non-human species. */
  def selectNotHuman (resSeq:Seq[KBResolution]): Seq[KBResolution] = {
    resSeq.filter(kbr => kbr.hasSpecies && !isHumanSpecies(kbr.species))
          .sortBy(kbr => (kbr.species, kbr.id))
  }

  /** Filter sequence to return only resolutions (sorted) with the given species. */
  def selectASpecies (resSeq:Seq[KBResolution], species:String): Seq[KBResolution] =
    resSeq.filter(kbr => kbr.species == species).sortBy(_.id)

  /** Filter sequence to return only resolutions (sorted) without the given species. */
  def selectNotASpecies (resSeq:Seq[KBResolution], species:String): Seq[KBResolution] =
    resSeq.filter(kbr => kbr.species != species).sortBy(kbr => (kbr.species, kbr.id))

  /** Filter sequence to return only resolutions (sorted) with a species in the given set. */
  def selectBySpecies (resSeq:Seq[KBResolution], species:SpeciesNameSet): Seq[KBResolution] =
    resSeq.filter(kbr => species.contains(kbr.species)).sortBy(kbr => (kbr.species, kbr.id))

  /** Filter sequence to return only resolutions (sorted) without a species in the given set. */
  def selectByNotSpecies (resSeq:Seq[KBResolution], species:SpeciesNameSet): Seq[KBResolution] =
    resSeq.filter(kbr => !species.contains(kbr.species)).sortBy(kbr => (kbr.species, kbr.id))

  /** Return given sequence of resolutions ordered in the application default manner. */
  def orderResolutions (resSeq: Seq[KBResolution]): Seq[KBResolution] =
    selectHuman(resSeq) ++ selectNoSpecies(resSeq) ++ selectNotHuman(resSeq)

}

/**
  * Class to implement an incrementing counter for generating unique IDs.
  * This is used in a multi-threaded environment and therefore should be atomic.
  * current() doesn't have a well-defined meaning in such an environment and
  * has therefore been removed.
  */
class IncrementingCounter {
  protected var counter: AtomicInteger = new AtomicInteger()

  //def current(): Int = counter.get
  def next(): Int = counter.incrementAndGet
}
