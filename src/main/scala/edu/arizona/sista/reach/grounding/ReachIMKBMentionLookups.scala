package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * A collection of classes which provide mappings of Mentions to identifiers
  * using an encapsulated, locally-sourced knowledge base.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Update for tsv factory.
  */

//
// Bio Processes Accessors
//

/** KB accessor to resolve bio process names via static KB. */
class StaticBioProcessKBML extends IMKBMentionLookup {
  memoryKB = (new AdHocIMKBFactory).make(StaticBioProcessFilename)
}


//
// Subcellular Location Accessors
//

/** KB accessor to resolve subcellular location names via KBs generated from the BioPax model. */
class GendCellLocationKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(GendCellLocationFilename)
}

/** KB accessor to resolve subcellular location names via manually maintained KBs. */
class ManualCellLocationKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(ManualCellLocationFilename)
}

/** KB accessor to resolve subcellular location names via static KB. */
class StaticCellLocationKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make("go", StaticCellLocationFilename,
    new IMKBMetaInfo("http://identifiers.org/go/", "MIR:00000022"))
}

/** KB accessor to resolve alternate subcellular location names via static KB. */
class StaticCellLocationKBML2 extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make("uniprot", StaticCellLocation2Filename,
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005"))
}


//
// Small Molecule (Chemical and Metabolite) Accessors
//

/** KB accessor to resolve small molecule (chemical) names via KBs generated from the BioPax model. */
class GendChemicalKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(GendChemicalFilename)
}

/** KB accessor to resolve small molecule (chemical) names via manually maintained KBs. */
class ManualChemicalKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(ManualChemicalFilename)
}

/** KB accessor to resolve small molecule (metabolite) names via static KB. */
class StaticMetaboliteKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make("hmdb", StaticMetaboliteFilename,
    new IMKBMetaInfo("http://identifiers.org/hmdb/", "MIR:00000051"))
}

/** KB accessor to resolve small molecule (chemical) names via static KB. */
class StaticChemicalKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make("chebi", StaticChemicalFilename,
    new IMKBMetaInfo("http://identifiers.org/chebi/", "MIR:00100009"))
}


//
// Protein Accessors
//

/** KB accessor to resolve protein names via KBs generated from the BioPax model. */
class GendProteinKBML extends IMKBProteinMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(GendProteinFilename)
}

/** KB accessor to resolve protein names via manually maintained KBs. */
class ManualProteinKBML extends IMKBProteinMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(ManualProteinFilename)
}

/** KB accessor to resolve protein names via static KB. */
class StaticProteinKBML extends IMKBProteinMentionLookup {
  memoryKB = (new TsvIMKBFactory).make("uniprot", StaticProteinFilename, true,
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00100164")) // true = has species
}


//
// Protein Family Accessors
//   These extend from LocalAltKBMentionLookup because protein & protein family
//   alternate lookups are the same for now.
//

/** KB accessor to resolve protein family names via KBs generated from the BioPax model. */
class GendProteinFamilyKBML extends IMKBFamilyMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(GendProteinFilename)
}

/** KB accessor to resolve protein names via manually maintained KBs. */
class ManualProteinFamilyKBML extends IMKBFamilyMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(ManualProteinFilename)
}

/** KB accessor to resolve protein family names via static KB. */
class StaticProteinFamilyKBML extends IMKBFamilyMentionLookup {
  memoryKB = (new TsvIMKBFactory).make("interpro", StaticProteinFamilyFilename, true,
    new IMKBMetaInfo("http://identifiers.org/interpro/", "MIR:00000011")) // true = has species
}


//
// Tissue Type Accessor
//

/** KB accessor to resolve tissue type names via static KB. */
class StaticTissueTypeKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make("uniprot", StaticTissueTypeFilename,
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005"))
}
