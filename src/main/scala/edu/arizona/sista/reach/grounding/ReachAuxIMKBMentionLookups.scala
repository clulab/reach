package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * A collection of classes which implement auxiliary knowledge base accessors.
  *   Written by Tom Hicks. 11/17/2015.
  *   Last Modified: Initial back-port and refactoring.
  */

/** KB accessor to resolve biological process names via an auxiliary KB. */
class AuxBioProcessKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/go/", "go", "MIR:00000022"),
                   AuxBioProcessFilename)
}

/** KB accessor to resolve subcellular location names via an auxiliary KB. */
class AuxCellLocationKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/go/", "go", "MIR:00000022"),
                   AuxCellLocationFilename)
}

/** KB accessor to resolve small molecule (chemical) names via an auxiliary KB. */
class AuxChemicalKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/chebi/", "chebi", "MIR:00100009"),
                   AuxChemicalFilename)
}

/** KB accessor to resolve small molecule (metabolite) names via an auxiliary KB. */
class AuxMetaboliteKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/hmdb/", "hmdb", "MIR:00000051"),
                   AuxMetaboliteFilename)
}

/** KB accessor to resolve protein names via an auxiliary KB. */
class AuxProteinKBML extends IMKBProteinMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00100164"),
                   AuxProteinFilename, true)  // true = has species
}

/** KB accessor to resolve protein family names via an auxiliary KB. */
class AuxProteinFamilyKBML extends IMKBFamilyMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/interpro/", "interpro", "MIR:00000011"),
                   AuxProteinFamilyFilename, true)  // true = has species
}

/** KB accessor to resolve tissue type names via an auxiliary KB. */
class AuxTissueTypeKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00000005"),
                   AuxTissueTypeFilename)
}
