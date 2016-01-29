package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * A collection of classes which implement knowledge base accessors for mention context.
  *   Written by Enrique Noriega and Tom Hicks.
  *   Last Modified: Update for tsv factory.
  */

/** KB accessor to resolve cell lines via a context KB. */
class ContextCellLineKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(ContextCellLineFilename)
}

/** KB accessor to resolve cell types via a context KB.
    Uses alternate key lookups for organ to cell type inference. */
class ContextCellTypeKBML extends IMKBOrganCellTypeMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(ContextCellTypeFilename)
}

/** KB accessor to resolve organ names via a context KB. */
class ContextOrganKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make(ContextOrganFilename)
}

/** KB accessor to resolve species names via a context KB. */
class ContextSpeciesKBML extends IMKBMentionLookup {
  memoryKB = (new TsvIMKBFactory).make("taxonomy", ContextSpeciesFilename,
    new IMKBMetaInfo("http://identifiers.org/taxonomy/", "MIR:00000006"))
}
