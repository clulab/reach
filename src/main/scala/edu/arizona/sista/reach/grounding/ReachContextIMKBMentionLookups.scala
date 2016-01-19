package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * A collection of classes which implement knowledge base accessors for mention context.
  *   Written by Enrique Noriega and Tom Hicks.
  *   Last Modified: Update for IMKB ctor changes.
  */

/** KB accessor to resolve cell lines via a context KB. */
class ContextCellLineKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB(ContextCellLineFilename)
}

/** KB accessor to resolve cell types via a context KB.
    Uses alternate key lookups for organ to cell type inference. */
class ContextCellTypeKBML extends IMKBOrganCellTypeMentionLookup {
  val memoryKB = new InMemoryKB(ContextCellTypeFilename)
}

/** KB accessor to resolve organ names via a context KB. */
class ContextOrganKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB(ContextOrganFilename)
}

/** KB accessor to resolve species names via a context KB. */
class ContextSpeciesKBML extends IMKBMentionLookup {
  val memoryKB = new InMemoryKB("taxonomy", ContextSpeciesFilename,
    new IMKBMetaInfo("http://identifiers.org/taxonomy/", "MIR:00000006"))
}

