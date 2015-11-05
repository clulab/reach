package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.reach.grounding2.LocalKBConstants._

/**
  * A collection of classes which provide mappings of text strings to identifiers
  * using an encapsulated, locally-sourced knowledge base.
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Make protein and protein family KBLs extend alternate KBL.
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

/** KB accessor to resolve protein names via static KBs with alternate lookups. */
class StaticProteinKBLookup extends LocalAltKBLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00100164"), StaticProteinFilename)
}

/** KB lookup to resolve protein family names via static KBs with alternate lookups. */
class StaticProteinFamilyKBLookup extends LocalAltKBLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/interpro/", "interpro", "MIR:00000011"), StaticProteinFamilyFilename)
}

/** KB lookup to resolve tissue type names via static KBs. */
class StaticTissueTypeKBLookup extends LocalKBLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00000005"), StaticTissueTypeFilename)
}
