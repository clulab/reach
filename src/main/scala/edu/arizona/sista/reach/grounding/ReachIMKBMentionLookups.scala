package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * A collection of classes which provide mappings of Mentions to identifiers
  * using an encapsulated, locally-sourced knowledge base.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Refactor AZ fail safe KBML to own class file. Update for IMKB factory.
  */

//
// Subcellular Location Accessors
//

/** KB accessor to resolve subcellular location names via KBs generated from the BioPax model. */
class GendCellLocationKBML extends IMKBMentionLookup {
  val memoryKB = new TsvIMKBFactory(GendCellLocationFilename).make()
}

/** KB accessor to resolve subcellular location names via manually maintained KBs. */
class ManualCellLocationKBML extends IMKBMentionLookup {
  val memoryKB = new TsvIMKBFactory(ManualCellLocationFilename).make()
}

/** KB mention lookup to resolve subcellular location names via static KBs. */
class StaticCellLocationKBML extends IMKBMentionLookup {
  val memoryKB = new TsvIMKBFactory("go", StaticCellLocationFilename,
    new IMKBMetaInfo("http://identifiers.org/go/", "MIR:00000022")).make()
}

/** KB mention lookup to resolve alternate subcellular location names via static KBs. */
class StaticCellLocationKBML2 extends IMKBMentionLookup {
  val memoryKB = new TsvIMKBFactory("uniprot", StaticCellLocation2Filename,
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005")).make()
}


//
// Small Molecule (Chemical and Metabolite) Accessors
//

/** KB accessor to resolve small molecule (chemical) names via KBs generated from the BioPax model. */
class GendChemicalKBML extends IMKBMentionLookup {
  val memoryKB = new TsvIMKBFactory(GendChemicalFilename).make()
}

/** KB accessor to resolve small molecule (chemical) names via manually maintained KBs. */
class ManualChemicalKBML extends IMKBMentionLookup {
  val memoryKB = new TsvIMKBFactory(ManualChemicalFilename).make()
}

/** KB accessor to resolve small molecule (metabolite) names via static KBs. */
class StaticMetaboliteKBML extends IMKBMentionLookup {
  val memoryKB = new TsvIMKBFactory("hmdb", StaticMetaboliteFilename,
    new IMKBMetaInfo("http://identifiers.org/hmdb/", "MIR:00000051")).make()
}

/** KB accessor to resolve small molecule (chemical) names via static KBs. */
class StaticChemicalKBML extends IMKBMentionLookup {
  val memoryKB = new TsvIMKBFactory("chebi", StaticChemicalFilename,
    new IMKBMetaInfo("http://identifiers.org/chebi/", "MIR:00100009")).make()
}


//
// Protein Accessors
//

/** KB accessor to resolve protein names via KBs generated from the BioPax model. */
class GendProteinKBML extends IMKBProteinMentionLookup {
  val memoryKB = new TsvIMKBFactory(GendProteinFilename).make()
}

/** KB accessor to resolve protein names via manually maintained KBs. */
class ManualProteinKBML extends IMKBProteinMentionLookup {
  val memoryKB = new TsvIMKBFactory(ManualProteinFilename).make()
}

/** KB accessor to resolve protein names via static KBs. */
class StaticProteinKBML extends IMKBProteinMentionLookup {
  val memoryKB = new TsvIMKBFactory("uniprot", StaticProteinFilename, true, // true = has species
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00100164")).make()
}


//
// Protein Family Accessors
//   These extend from LocalAltKBMentionLookup because protein & protein family
//   alternate lookups are the same for now.
//

/** KB accessor to resolve protein family names via KBs generated from the BioPax model. */
class GendProteinFamilyKBML extends IMKBFamilyMentionLookup {
  val memoryKB = new TsvIMKBFactory(GendProteinFilename).make()
}

/** KB accessor to resolve protein names via manually maintained KBs. */
class ManualProteinFamilyKBML extends IMKBFamilyMentionLookup {
  val memoryKB = new TsvIMKBFactory(ManualProteinFilename).make()
}

/** KB accessor to resolve protein family names via static KBs. */
class StaticProteinFamilyKBML extends IMKBFamilyMentionLookup {
  val memoryKB = new TsvIMKBFactory("interpro", StaticProteinFamilyFilename, true, // true = has species
    new IMKBMetaInfo("http://identifiers.org/interpro/", "MIR:00000011")).make()
}


//
// Tissue Type Accessor
//

/** KB accessor to resolve tissue type names via static KBs. */
class StaticTissueTypeKBML extends IMKBMentionLookup {
  val memoryKB = new TsvIMKBFactory("uniprot", StaticTissueTypeFilename,
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005")).make()
}
