package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.reach.grounding2.LocalKBConstants._
import edu.arizona.sista.reach.grounding2.LocalKBUtils._

/**
  * A collection of classes which encapsulate locally-sourced knowledge bases.
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Initial refactoring.
  */

/** KB lookup to resolve subcellular location names via static KBs. */
class StaticCellLocationKBLookup extends LocalKBLookup {
  val metaInfo = new KBMetaInfo("http://identifiers.org/go/", "go", "MIR:00000022")
  readAndFillKB(metaInfo, memKB, StaticCellLocationFilename) // load KB
}

/** KB lookup to resolve small molecule (metabolite) names via static KBs. */
class StaticMetaboliteKBLookup extends LocalKBLookup {
  val metaInfo = new KBMetaInfo("http://identifiers.org/hmdb/", "hmdb", "MIR:00000051")
  readAndFillKB(metaInfo, memKB, StaticMetaboliteFilename) // load KB
}

/** KB lookup to resolve small molecule (chemical) names via static KBs. */
class StaticChemicalKBLookup extends LocalKBLookup {
  val metaInfo = new KBMetaInfo("http://identifiers.org/chebi/", "chebi", "MIR:00100009")
  readAndFillKB(metaInfo, memKB, StaticChemicalFilename) // load KB
}

/** KB accessor to resolve protein names via static KBs. */
class StaticProteinKBLookup extends LocalKBLookup {
  val metaInfo = new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00100164")
  readAndFillKB(metaInfo, memKB, StaticProteinFilename) // load KB
}

/** KB lookup to resolve protein family names via static KBs. */
class StaticProteinFamilyKBLookup extends LocalKBLookup {
  val metaInfo = new KBMetaInfo("http://identifiers.org/interpro/", "interpro", "MIR:00000011")
  readAndFillKB(metaInfo, memKB, StaticProteinFamilyFilename) // load KB
}

/** KB lookup to resolve tissue type names via static KBs. */
class StaticTissueTypeKBLookup extends LocalKBLookup {
  val metaInfo = new KBMetaInfo("http://identifiers.org/uniprot/", "uniprot", "MIR:00000005")
  readAndFillKB(metaInfo, memKB, StaticTissueTypeFilename) // load KB
}
