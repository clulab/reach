package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * KB accessor implementation which always resolves each mention with a local, fake ID.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Refactored into own class file.
  */
class AzFailsafeKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB()           // no external KB file to load!

  private val idCntr = new IncrementingCounter() // counter sequence class

  // base resolve of text string which does all the work for this class
  override def resolve (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)
    val entry = memoryKB.lookupNoSpecies(key)   // look for an existing entry
    if (entry.isDefined)                        // if KB entry is already defined
      return memoryKB.newResolution(entry)      // create/wrap return value
    else {                                      // else no existing entry, so
      val refId = "UAZ%05d".format(idCntr.next) // create a new reference ID
      val kbe = new KBEntry(text, key, DefaultNamespace, refId) // create a new KB entry
      memoryKB.insertOrUpdateEntry(kbe)         // insert the new KB entry
      return Some(memoryKB.newResolution(kbe))  // wrap return value in optional
    }
  }

  // implementations which ignore the given species and defer to the base text resolve
  override def resolveHuman (text:String): Option[KBResolution] = resolve(text)
  override def resolveByASpecies (text:String, species:String): Option[KBResolution] = resolve(text)
  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] = Some(Iterable(resolve(text).get))
  override def resolveNoSpecies (text:String): Option[KBResolution] = resolve(text)

  // mention resolves which also ignore the given species and defer to the base text resolve
  override def resolve (mention:Mention): Option[KBResolution] = resolve(mention.text)
  override def resolveHuman (mention:Mention): Option[KBResolution] = resolve(mention.text)
  override def resolveByASpecies (mention:Mention, species:String): Option[KBResolution] =
    resolve(mention.text)
  override def resolveBySpecies (mention:Mention, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] = resolveBySpecies(mention.text, speciesSet)
  override def resolveNoSpecies (mention:Mention): Option[KBResolution] = resolve(mention.text)
}
