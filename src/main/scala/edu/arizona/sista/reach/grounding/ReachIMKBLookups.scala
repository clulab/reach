package edu.arizona.sista.reach.grounding

import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * A collection of classes which provide mappings of text strings to identifiers
  * using an encapsulated, locally-sourced knowledge base.
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Update for IMKB factory.
  */

/** KB lookup to resolve subcellular location names via static KBs. */
class StaticCellLocationKBLookup extends IMKBLookup {
  val memoryKB = new TsvIMKBFactory("go", StaticCellLocationFilename,
    new IMKBMetaInfo("http://identifiers.org/go/", "MIR:00000022")).make()
}

/** KB lookup to resolve small molecule (metabolite) names via static KBs. */
class StaticMetaboliteKBLookup extends IMKBLookup {
  val memoryKB = new TsvIMKBFactory("hmbd", StaticMetaboliteFilename,
    new IMKBMetaInfo("http://identifiers.org/hmdb/", "MIR:00000051")).make()
}

/** KB lookup to resolve small molecule (chemical) names via static KBs. */
class StaticChemicalKBLookup extends IMKBLookup {
  val memoryKB = new TsvIMKBFactory("chebi", StaticChemicalFilename,
    new IMKBMetaInfo("http://identifiers.org/chebi/", "MIR:00100009")).make()
}

/** KB accessor to resolve protein names via static KBs with alternate lookups. */
class StaticProteinKBLookup extends IMKBProteinLookup {
  val memoryKB = new TsvIMKBFactory("uniprot", StaticProteinFilename, true, // true = has species
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00100164")).make()
}

/** KB lookup to resolve protein family names via static KBs with alternate lookups. */
class StaticProteinFamilyKBLookup extends IMKBFamilyLookup {
  val memoryKB = new TsvIMKBFactory("interpro", StaticProteinFamilyFilename, true, // true = has species
    new IMKBMetaInfo("http://identifiers.org/interpro/", "MIR:00000011")).make()
}

/** KB lookup to resolve tissue type names via static KBs. */
class StaticTissueTypeKBLookup extends IMKBLookup {
  val memoryKB = new TsvIMKBFactory("uniprot", StaticTissueTypeFilename,
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005")).make()
}
