package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * KB accessor implementation which always resolves each mention with a local, fake ID.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Update for IMKB mention lookup changes.
  */
class AzFailsafeKBML extends IMKBMentionLookup {

  private val idCntr = new IncrementingCounter() // counter sequence class

  // base resolve of text string which does all the work for this class
  override def resolve (text:String): Resolutions = {
    val key = makeCanonicalKey(text)
    val resolutions = memoryKB.lookupNoSpecies(key)
    if (resolutions.isDefined)                  // text key has been resolved
      return resolutions
    else {                                      // else no existing entry for this text key
      val refId = "UAZ%05d".format(idCntr.next) // so create a new reference ID
      val entry = new KBEntry(text, key, DefaultNamespace, refId) // create a new KB entry
      memoryKB.addEntry(entry)                  // insert the new KB entry
      return memoryKB.toResolutions(entry)      // wrap return value as resolutions
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
