package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * A collection of classes which implement auxiliary knowledge base accessors.
  *   Written by Tom Hicks. 11/17/2015.
  *   Last Modified: Update for IMKB ctor changes.
  */

/** KB accessor to resolve biological process names via an auxiliary KB. */
class AuxBioProcessKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB("go", AuxBioProcessFilename,
    new IMKBMetaInfo("http://identifiers.org/go/", "MIR:00000022"))
}

/** KB accessor to resolve subcellular location names via an auxiliary KB. */
class AuxCellLocationKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB("go", AuxCellLocationFilename,
    new IMKBMetaInfo("http://identifiers.org/go/", "MIR:00000022"))
}

/** KB accessor to resolve small molecule (chemical) names via an auxiliary KB. */
class AuxChemicalKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB("chebi", AuxChemicalFilename,
    new IMKBMetaInfo("http://identifiers.org/chebi/", "MIR:00100009"))
}

/** KB accessor to resolve small molecule (metabolite) names via an auxiliary KB. */
class AuxMetaboliteKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB("hmdb", AuxMetaboliteFilename,
    new IMKBMetaInfo("http://identifiers.org/hmdb/", "MIR:00000051"))
}

/** KB accessor to resolve protein names via an auxiliary KB. */
class AuxProteinKBML extends IMKBProteinMentionLookup {
  val memoryKB = new InMemoryKB("uniprot", AuxProteinFilename, true, // true = has species
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00100164"))
}

/** KB accessor to resolve protein family names via an auxiliary KB. */
class AuxProteinFamilyKBML extends IMKBFamilyMentionLookup {
  val memoryKB = new InMemoryKB("interpro", AuxProteinFamilyFilename, true, // true = has species
    new IMKBMetaInfo("http://identifiers.org/interpro/", "MIR:00000011"))
}

/** KB accessor to resolve tissue type names via an auxiliary KB. */
class AuxTissueTypeKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB("uniprot", AuxTissueTypeFilename,
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005"))
}
