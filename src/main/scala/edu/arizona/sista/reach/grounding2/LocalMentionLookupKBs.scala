package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding2.LocalKBConstants._

/**
  * A collection of classes which provide mappings of Mentions to identifiers
  * using an encapsulated, locally-sourced knowledge base.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Initial refactoring.
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
// class StaticCellLocationKBML2 extends LocalKBMentionLookup {
//  val memoryKB = new InMemoryKB(
//    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00000005"), StaticCellLocation2Filename)
// }


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

/** Base KB accessor to resolve protein names in mentions. */
trait LocalProteinKBML extends LocalKBMentionLookup {
  // TODO: LATER Override to perform alternate key lookups? (OR move to LookupKBs)?

  /** Overridden to perform alternate key lookups. */
  // override def resolve (mention:Mention): Map[String,String] = {
  //   val key = getLookupKey(mention)         // make a key from the mention
  //   val props = theKB.get(key)              // lookup key
  //   return if (props.isDefined) props.get   // find it or try alternate keys
  //          else tryAlternateKeys(key, LocalKeyTransforms.proteinKeyTransforms).getOrElse(Map.empty)
  // }
}

/** KB accessor to resolve protein names via KBs generated from the BioPax model. */
class GendProteinKBML extends LocalProteinKBML {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), GendProteinFilename)
}

/** KB accessor to resolve protein names via manually maintained KBs. */
class ManualProteinKBML extends LocalProteinKBML {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), ManualProteinFilename)
}

/** KB accessor to resolve protein names via static KBs. */
class StaticProteinKBML extends LocalProteinKBML {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00100164"),
                   StaticProteinFilename)
}


//
// Protein Family Accessors
//

/** Base KB accessor to resolve protein family names in mentions. */
trait LocalProteinFamilyKBML extends LocalKBMentionLookup {
  // TODO: LATER Override to perform alternate key lookups? (OR move to LookupKBs)?

  /** Override to perform alternate key lookups. */
  // override def resolve (mention:Mention): Map[String,String] = {
  //   val key = getLookupKey(mention)         // make a key from the mention
  //   val props = theKB.get(key)              // lookup key
  //   return if (props.isDefined) props.get   // find it or try alternate keys
  //          else tryAlternateKeys(key, LocalKeyTransforms.proteinKeyTransforms).getOrElse(Map.empty)
  // }
}

/** KB accessor to resolve protein family names via KBs generated from the BioPax model. */
class GendProteinFamilyKBML extends LocalProteinFamilyKBML {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), GendProteinFilename)
}

/** KB accessor to resolve protein names via manually maintained KBs. */
class ManualProteinFamilyKBML extends LocalProteinFamilyKBML {
  val memoryKB = new InMemoryKB(new KBMetaInfo(), ManualProteinFilename)
}

/** KB accessor to resolve protein family names via static KBs. */
class StaticProteinFamilyKBML extends LocalProteinFamilyKBML {
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
class AzFailsafeKBML extends KBMentionLookup {
  val memoryKB = new InMemoryKB(new KBMetaInfo()) // no external KB file to load!

  private val idCntr = new IncrementingCounter() // counter sequence class
  // TODO: IMPLEMENT LATER

  // private val seenIt = scala.collection.mutable.Map[String, Map[String,String]]()

  // override def resolve (mention:Mention): Map[String,String] = {
  //   val key = getLookupKey(mention)         // make a key from the mention
  //   seenIt.getOrElseUpdate(key, newResolution(key))  // return existing entry or new one
  // }

  // override of resolveBySpecies not necessary since trait default is to use resolve anyway

  // override def resolveBySpecies (mention:Mention, species:Set[String]): Map[String,String] = {
  //   return resolve(mention)                 // ignore species argument: irrelevant here
  // }

  // private def newResolution (key:String): Map[String,String] = {
  //   return Map(
  //     "referenceID" -> "UAZ%05d".format(idCntr.next),
  //     "namespace" -> namespace,
  //     "baseURI" -> baseURI,
  //     "key" -> key
  //   )
  // }
}
