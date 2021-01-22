package org.clulab.reach.grounding

import org.clulab.odin._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.Speciated._

import java.nio.charset.StandardCharsets

/**
  * KB accessor implementation which always resolves each mention with a local, fake ID.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Update for changing IMKB.
  */
class AzFailsafeKBML extends IMKBMentionLookup {

  private val idCntr = new IncrementingCounter() // counter sequence class

  def mkRefIdByCounter(text: String): String = {
    "UAZ%05d".format(idCntr.next())
  }

  def mkRefIdByContent(text: String): String = {
    text
        .getBytes(StandardCharsets.UTF_8)
        .map { byte => String.format("%02X", Byte.box(byte)) }
        .mkString("UAZ", "", "")
  }

  val mkRefId: String => String = mkRefIdByContent

  // base resolve of text string which does all the work for this class
  // We have a mutable, non-atomic KB here, so synchronization is necessary.
  override def resolve (text:String): Resolutions = synchronized {
    val resolutions = memoryKB.lookupNoSpecies(text)
    if (resolutions.isDefined)                           // text has been resolved
      resolutions
    else {                                               // else no existing entries for this text
      val refId = mkRefId(text)                          // so create a new reference ID
      memoryKB.addEntries(text, DefaultNamespace, refId) // create new KB entries for this text
      memoryKB.lookupNoSpecies(text)                     // and return results from repeating lookup
    }
  }

  // implementations which ignore the given species and defer to the base text resolve
  override def resolveHuman (text:String): Resolutions = resolve(text)
  override def resolveByASpecies (text:String, species:String): Resolutions = resolve(text)
  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Resolutions = resolve(text)
  override def resolveNoSpecies (text:String): Resolutions = resolve(text)

  // mention resolves which also ignore the given species and defer to the base text resolve
  override def resolve (mention:Mention): Resolutions = resolve(mention.text)
  override def resolveHuman (mention:Mention): Resolutions = resolve(mention.text)
  override def resolveByASpecies (mention:Mention, species:String): Resolutions = resolve(mention.text)
  override def resolveBySpecies (mention:Mention, speciesSet:SpeciesNameSet): Resolutions =
    resolveBySpecies(mention.text, speciesSet)
  override def resolveNoSpecies (mention:Mention): Resolutions = resolve(mention.text)
}


object AzFailsafeKBML {
  /** Singleton instance of the AZ failsafe KBML. */
  val AzFailsafe = new AzFailsafeKBML
}
