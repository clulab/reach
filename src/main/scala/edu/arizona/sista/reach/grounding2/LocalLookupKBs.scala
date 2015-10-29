package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.reach.grounding2.LocalKBConstants._

/**
  * A collection of classes which provide mappings of text strings to identifiers
  * using an encapsulated, locally-sourced knowledge base.
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Update file documentation.
  */

/** KB lookup to resolve subcellular location names via static KBs. */
class StaticCellLocationKBLookup extends LocalKBLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/go/", "go", "MIR:00000022"), StaticCellLocationFilename)
}

/** KB lookup to resolve small molecule (metabolite) names via static KBs. */
class StaticMetaboliteKBLookup extends LocalKBLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/hmdb/", "hmdb", "MIR:00000051"), StaticMetaboliteFilename)
}

/** KB lookup to resolve small molecule (chemical) names via static KBs. */
class StaticChemicalKBLookup extends LocalKBLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/chebi/", "chebi", "MIR:00100009"), StaticChemicalFilename)
}

/** KB accessor to resolve protein names via static KBs. */
class StaticProteinKBLookup extends LocalKBLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00100164"), StaticProteinFilename)
  // TODO: LATER OVERRIDE RESOLVES TO PERFORM ALTERNATE KEY LOOKUPS?
}

/** KB lookup to resolve protein family names via static KBs. */
class StaticProteinFamilyKBLookup extends LocalKBLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/interpro/", "interpro", "MIR:00000011"), StaticProteinFamilyFilename)
  // TODO: LATER OVERRIDE RESOLVES TO PERFORM ALTERNATE KEY LOOKUPS?
}

/** KB lookup to resolve tissue type names via static KBs. */
class StaticTissueTypeKBLookup extends LocalKBLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00000005"), StaticTissueTypeFilename)
}
