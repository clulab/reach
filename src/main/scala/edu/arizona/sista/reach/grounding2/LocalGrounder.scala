package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.odin._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.mentions._

/**
  * Class which implements project internal methods to ground entities.
  *   Written by Tom Hicks. 11/9/2015.
  *   Last Modified: Initial refactoring: untested.
  */
class LocalGrounder extends DarpaFlow {

  /** An exception in case we somehow fail to assign an ID during resolution. */
  case class NoFailSafe(message:String) extends Exception(message)

  /** Project local sequence for resolving entities: check local facade KBs in this order:
    * 1. Protein Families
    * 2. Proteins
    * 3. Small Molecules (metabolites and chemicals)
    * 4. Subcellular Locations
    * 5. AZ Failsafe KB (failsafe: always generates an ID in a non-official, local namespace)
    */
  protected val searchSequence = Seq(
    new StaticProteinFamilyKBML,
    new ManualProteinFamilyKBML,
    new StaticProteinKBML,
    new ManualProteinKBML,
    // NB: generated protein families are included in the generated protein KB:
    new GendProteinKBML,

    new StaticChemicalKBML,
    new StaticMetaboliteKBML,
    new ManualChemicalKBML,
    new GendChemicalKBML,

    new StaticCellLocationKBML,
    new ManualCellLocationKBML,
    new GendCellLocationKBML,

    new AzFailsafeKBML
  )


  /** Local implementation of darpa flow trait: use project specific KBs to ground
      and augment given mentions. */
  def apply (mentions: Seq[Mention], state: State): Seq[Mention] = mentions map {
    case tm: BioTextBoundMention => resolveAndAugment(tm, state)
    case m => m
  }

  /** Search the KB accessors in sequence, use the first one which resolves the given mention. */
  private def resolveAndAugment(mention: BioMention, state: State): Mention = {
    searchSequence.foreach { kbl =>
      val resolution = kbl.resolve(mention)
      if (!resolution.isEmpty) {
        mention.ground(resolution.get.metaInfo.namespace, resolution.get.id)
        return mention
      }
    }
    // we should never get here because our accessors include a failsafe ID assignment
    throw NoFailSafe(s"LocalGrounder failed to assign an ID to ${mention.displayLabel} '${mention.text}' in S${mention.sentence}")
  }

}
