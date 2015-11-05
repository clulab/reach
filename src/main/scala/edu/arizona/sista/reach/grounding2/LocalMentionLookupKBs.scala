package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding2.LocalKBConstants._

/**
  * A collection of classes which provide mappings of Mentions to identifiers
  * using an encapsulated, locally-sourced knowledge base.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Make protein and protein family KBMLs extend alternate KBML.
  */

//
// Subcellular Location Accessors
//

/** KB accessor to resolve subcellular location names via KBs generated from the BioPax model. */
class GendCellLocationKBML extends LocalKBMentionLookup {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), GendCellLocationFilename)
}

/** KB accessor to resolve subcellular location names via manually maintained KBs. */
class ManualCellLocationKBML extends LocalKBMentionLookup {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), ManualCellLocationFilename)
}

/** KB mention lookup to resolve subcellular location names via static KBs. */
class StaticCellLocationKBML extends LocalKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/go/", "go", "MIR:00000022"),
                   StaticCellLocationFilename)
}

/** KB mention lookup to resolve alternate subcellular location names via static KBs. */
class StaticCellLocationKBML2 extends LocalKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00000005"),
                   StaticCellLocation2Filename)
}


//
// Small Molecule (Chemical and Metabolite) Accessors
//

/** KB accessor to resolve small molecule (chemical) names via KBs generated from the BioPax model. */
class GendChemicalKBML extends LocalKBMentionLookup {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), GendChemicalFilename)
}

/** KB accessor to resolve small molecule (chemical) names via manually maintained KBs. */
class ManualChemicalKBML extends LocalKBMentionLookup {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), ManualChemicalFilename)
}

/** KB accessor to resolve small molecule (metabolite) names via static KBs. */
class StaticMetaboliteKBML extends LocalKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/hmdb/", "hmdb", "MIR:00000051"),
                   StaticMetaboliteFilename)
}

/** KB accessor to resolve small molecule (chemical) names via static KBs. */
class StaticChemicalKBML extends LocalKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/chebi/", "chebi", "MIR:00100009"),
                   StaticChemicalFilename)
}


//
// Protein Accessors
//

/** KB accessor to resolve protein names via KBs generated from the BioPax model. */
class GendProteinKBML extends LocalAltKBMentionLookup {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), GendProteinFilename)
}

/** KB accessor to resolve protein names via manually maintained KBs. */
class ManualProteinKBML extends LocalAltKBMentionLookup {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), ManualProteinFilename)
}

/** KB accessor to resolve protein names via static KBs. */
class StaticProteinKBML extends LocalAltKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00100164"),
                   StaticProteinFilename)
}


//
// Protein Family Accessors
//   These extend from LocalAltKBMentionLookup because protein & protein family
//   alternate lookups are the same for now.
//

/** KB accessor to resolve protein family names via KBs generated from the BioPax model. */
class GendProteinFamilyKBML extends LocalAltKBMentionLookup {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), GendProteinFilename)
}

/** KB accessor to resolve protein names via manually maintained KBs. */
class ManualProteinFamilyKBML extends LocalAltKBMentionLookup {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), ManualProteinFilename)
}

/** KB accessor to resolve protein family names via static KBs. */
class StaticProteinFamilyKBML extends LocalAltKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/interpro/", "interpro", "MIR:00000011"),
                   StaticProteinFamilyFilename)
}


//
// Tissue Type Accessor
//

/** KB accessor to resolve tissue type names via static KBs. */
class StaticTissueTypeKBML extends LocalKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00000005"),
                   StaticTissueTypeFilename)
}


//
// Failsafe Accessor
//

/** KB accessor implementation which always resolves each mention with a local, fake ID. */
class AzFailsafeKBML extends LocalKBMentionLookup {
  val memoryKB = new InMemoryKB(new KBMetaInfo()) // no external KB file to load!

  private val idCntr = new IncrementingCounter() // counter sequence class

  // base resolve of text string which does all the work for this class
  override def resolve (text:String): Option[KBResolution] = {
    val key = makeCanonicalKey(text)
    val entry = memoryKB.lookup(key)            // look for existing entry
    if (entry.isDefined)                        // if KB entry is already defined
      return Some(memoryKB.newResolution(entry.get)) // create/wrap return value
    else {                                      // else no existing entry, so
      val refId = "UAZ%05d".format(idCntr.next) // create a new reference ID
      val kbe = new KBEntry(text, key, refId)   // create a new KB entry
      memoryKB.insertOrUpdateEntry(kbe)         // insert the new KB entry
      return Some(memoryKB.newResolution(kbe))  // wrap return value in optional
    }
  }

  // implementations which ignore the given species and defer to the base text resolve
  override def resolveHuman (text:String): Option[KBResolution] = resolve(text)
  override def resolveByASpecies (text:String, species:String): Option[KBResolution] = resolve(text)
  override def resolveBySpecies (text:String, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] = Some(Iterable(resolve(text).get))

  // mention resolves which also ignore the given species and defer to the base text resolve
  override def resolveHuman (mention:Mention): Option[KBResolution] = resolve(mention.text)
  override def resolve (mention:Mention): Option[KBResolution] = resolve(mention.text)
  override def resolveByASpecies (mention:Mention, species:String): Option[KBResolution] =
    resolve(mention.text)
  override def resolveBySpecies (mention:Mention, speciesSet:SpeciesNameSet): Option[Iterable[KBResolution]] = resolveBySpecies(mention.text, speciesSet)
}
