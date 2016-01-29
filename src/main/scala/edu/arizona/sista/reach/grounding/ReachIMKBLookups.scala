package edu.arizona.sista.reach.grounding

import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * A collection of classes which provide mappings of text strings to identifiers
  * using an encapsulated, locally-sourced knowledge base.
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Update for tsv factory.
  */

/** KB lookup to resolve subcellular location names via static KBs. */
class StaticCellLocationKBLookup extends IMKBLookup {
  memoryKB = (new TsvIMKBFactory).make("go", StaticCellLocationFilename,
    new IMKBMetaInfo("http://identifiers.org/go/", "MIR:00000022"))
}

/** KB lookup to resolve small molecule (metabolite) names via static KBs. */
class StaticMetaboliteKBLookup extends IMKBLookup {
  memoryKB = (new TsvIMKBFactory).make("hmbd", StaticMetaboliteFilename,
    new IMKBMetaInfo("http://identifiers.org/hmdb/", "MIR:00000051"))
}

/** KB lookup to resolve small molecule (chemical) names via static KBs. */
class StaticChemicalKBLookup extends IMKBLookup {
  memoryKB = (new TsvIMKBFactory).make("chebi", StaticChemicalFilename,
    new IMKBMetaInfo("http://identifiers.org/chebi/", "MIR:00100009"))
}

/** KB accessor to resolve protein names via static KBs with alternate lookups. */
class StaticProteinKBLookup extends IMKBProteinLookup {
  memoryKB = (new TsvIMKBFactory).make("uniprot", StaticProteinFilename, true, // true = has species
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00100164"))
}

/** KB lookup to resolve protein family names via static KBs with alternate lookups. */
class StaticProteinFamilyKBLookup extends IMKBFamilyLookup {
  memoryKB = (new TsvIMKBFactory).make("interpro", StaticProteinFamilyFilename, true,
    new IMKBMetaInfo("http://identifiers.org/interpro/", "MIR:00000011")) // true = has species
}

/** KB lookup to resolve tissue type names via static KBs. */
class StaticTissueTypeKBLookup extends IMKBLookup {
  memoryKB = (new TsvIMKBFactory).make("uniprot", StaticTissueTypeFilename,
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005"))
}
