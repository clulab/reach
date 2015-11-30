package edu.arizona.sista.reach.grounding

import com.typesafe.config.ConfigFactory

import edu.arizona.sista.odin._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.mentions._

/**
  * Class which implements project internal methods to ground entities.
  *   Written by Tom Hicks. 4/6/2015.
  *   Last Modified: Add auxiliary grounding controlled by config flag.
  */
class LocalGrounder extends DarpaFlow {

  /** An exception in case we somehow fail to assign an ID during resolution. */
  case class NoFailSafe(message:String) extends Exception(message)

  // Read config file to determine whether to include knowledge bases
  val config = ConfigFactory.load()
  val useAuxGrounding = config.getBoolean("useAuxGrounding")

  /** Project local sequence for resolving entities: check local facade KBs in this order:
    * 1. Proteins
    * 2. Protein Families
    * 3. Small Molecules (metabolites and chemicals)
    * 4. Subcellular Locations
    * 5. AZ Failsafe KB (failsafe: always generates an ID in a non-official, local namespace)
    */

  protected val searchSequence =
    if (!useAuxGrounding)
      Seq(
        new StaticProteinFamilyKBAccessor,
        new ManualProteinFamilyKBAccessor,
        new StaticProteinKBAccessor,
        new ManualProteinKBAccessor,
        // NB: generated protein families are included in the generated protein KB:
        new GendProteinKBAccessor,

        new StaticChemicalKBAccessor,
        new StaticMetaboliteKBAccessor,
        new ManualChemicalKBAccessor,
        new GendChemicalKBAccessor,

        new StaticCellLocationKBAccessor,
        new ManualCellLocationKBAccessor,
        new GendCellLocationKBAccessor,

        // Context-relevant accessors
        new CellTypeKBAccessor,
        new InferredCellTypeKBAccessor, // These mentions come from a rule
        new SpeciesKBAccessor,
        new CellLinesKBAccessor,
        new OrganKBAccessor,
        new StaticTissueTypeKBLookup,

        new AzFailsafeKBAccessor
      )
    else
      Seq(
        new AuxProteinKBAccessor,
        new AuxBioProcessKBAccessor,
        new AuxMetaboliteKBAccessor,
        new StaticProteinFamilyKBAccessor,
        new ManualProteinFamilyKBAccessor,
        new StaticProteinKBAccessor,
        new ManualProteinKBAccessor,
        // NB: generated protein families are included in the generated protein KB:
        new GendProteinKBAccessor,

        new StaticChemicalKBAccessor,
        new StaticMetaboliteKBAccessor,
        new ManualChemicalKBAccessor,
        new GendChemicalKBAccessor,

        new StaticCellLocationKBAccessor,
        new ManualCellLocationKBAccessor,
        new GendCellLocationKBAccessor,

        // Context-relevant accessors
        new CellTypeKBAccessor,
        new InferredCellTypeKBAccessor, // These mentions come from a rule
        new SpeciesKBAccessor,
        new CellLinesKBAccessor,
        new OrganKBAccessor,
        new StaticTissueTypeKBLookup,

        new AzFailsafeKBAccessor
      )


  /** Local implementation of trait: use project specific KBs to ground and augment given mentions. */
  def apply (mentions: Seq[Mention], state: State): Seq[Mention] = mentions map {
    case tm: BioTextBoundMention => resolveAndAugment(tm, state)
    case m => m
  }

  /** Search the KB accessors in sequence, use the first one which resolves the given mention. */
  private def resolveAndAugment(mention: BioMention, state: State): Mention = {
    searchSequence.foreach { kbAccessor =>
      val resInfo = kbAccessor.resolve(mention)
      if (!resInfo.isEmpty) {
        mention.ground(resInfo("namespace"), resInfo("referenceID"))
        return mention
      }
    }
    // we should never get here because our accessors include a failsafe ID assignment
    throw NoFailSafe(s"LocalGrounder failed to assign an ID to ${mention.displayLabel} '${mention.text}' in S${mention.sentence}")
  }

}
