package edu.arizona.sista.reach.grounding

import scala.collection.JavaConverters._
import scala.collection.mutable.Map

import com.typesafe.config.{Config, ConfigFactory}

import edu.arizona.sista.odin._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.grounding.AzFailsafeKBML._
import edu.arizona.sista.reach.grounding.ReachEntityLookup._
import edu.arizona.sista.reach.grounding.ReachIMKBMentionLookups._

/**
  * Class which implements project internal methods to ground entities.
  *   Written by Tom Hicks. 11/9/2015.
  *   Last Modified: Use Uniprot Tissue List and Organs as tissue types.
  */
class ReachEntityLookup {

  /** Use project specific KBs to ground and augment given mentions. */
  def apply (mentions: Seq[BioMention]): Seq[BioMention] = mentions map {
    case tm: BioTextBoundMention => resolveMention(tm)
    case m => m
  }


  /** Return a new instance of an AdHoc KBML created using the given config map. */
  private def addAdHocFile (fileDef: Config): Option[IMKBMentionLookup] = {
    if (fileDef.isEmpty) return None        // sanity check
    val params: Map[String, _ >: String] = fileDef.root.unwrapped.asScala
    params.get("kb").map { fname =>
      new IMKBMentionLookup(AdHocIMKBFactory.make(fname.asInstanceOf[String]))
    }
  }

  /** Search a sequence of KB accessors, which sequence determined by the main mention label. */
  private def resolveMention (mention: BioMention): BioMention = {
    mention.label match {
      case "BioProcess" => augmentMention(mention, bioProcessSeq)
      case "CellLine" => augmentMention(mention, cellLineSeq)
      case "CellType" => augmentMention(mention, cellTypeSeq)
      case "Cellular_component" => augmentMention(mention, cellComponentSeq)
      case "Complex" | "GENE" | "Gene_or_gene_product" | "Protein" =>
        augmentMention(mention, proteinSeq)
      case "Family" =>  augmentMention(mention, familySeq)
      case "Organ" => augmentMention(mention, organSeq)
      case "Simple_chemical" => augmentMention(mention, chemicalSeq)
      case "Species" => augmentMention(mention, speciesSeq)
      case "TissueType" => augmentMention(mention, tissueSeq)
      case _ =>  augmentMention(mention, azFailsafeSeq)
    }
  }

  private def augmentMention (mention: BioMention, searchSequence: KBSearchSequence): BioMention = {
    searchSequence.foreach { kbml =>
      val resolutions = kbml.resolve(mention)
      if (resolutions.isDefined) {
        mention.nominate(resolutions)       // save candidate resolutions in mention
        return mention
      }
    }
    // if reach here, we assign a failsafe backup ID:
    mention.nominate(AzFailsafe.resolve(mention))
    return mention
  }

  // Reach Grounder Initialization

  /** Variable to hold additional "adhoc" KBs which are dynamically added to search. */
  var extraKBs: KBSearchSequence = Seq()

  // Read config file to determine which (if any) additional knowledge bases to include:
  val config = ConfigFactory.load()
  if (config.hasPath("grounding.adHocFiles")) {
    val moreKBs = config.getConfigList("grounding.adHocFiles").asScala.flatMap(addAdHocFile(_))
    if (!moreKBs.isEmpty) extraKBs = moreKBs
  }

  /** KB search sequence to use for Fallback grounding: when all others fail. */
  val azFailsafeSeq: KBSearchSequence = Seq(AzFailsafe)

  // instantiate the various search sequences, each sequence for a different label:
  val bioProcessSeq: KBSearchSequence = extraKBs ++ Seq( StaticBioProcess )
  val cellLineSeq: KBSearchSequence = extraKBs ++ Seq( ContextCellLine )
  val cellTypeSeq: KBSearchSequence = extraKBs ++ Seq( ContextCellType )

  val cellComponentSeq: KBSearchSequence = extraKBs ++ Seq(
    StaticCellLocation,                 // GO subcellular KB
    StaticCellLocation2,                // Uniprot subcellular KB
    ManualCellLocation,
    ModelGendCellLocation
  )

  val chemicalSeq: KBSearchSequence = extraKBs ++ Seq(
    StaticChemical,
    // StaticMetabolite,                    // REPLACED by PubChem
    ManualChemical,
    ModelGendChemical
  )

  val familySeq: KBSearchSequence = extraKBs ++ Seq(
    StaticProteinFamily,
    StaticProteinFamily2,
    ManualProteinFamily,
    ModelGendProteinAndFamily
  )

  val organSeq: KBSearchSequence = extraKBs ++ Seq( ContextOrgan )

  val proteinSeq: KBSearchSequence = extraKBs ++ Seq(
    StaticProtein,
    ManualProtein,
    ModelGendProteinAndFamily
  )

  val speciesSeq: KBSearchSequence = extraKBs ++ Seq( ContextSpecies )

  val tissueSeq: KBSearchSequence = extraKBs ++ Seq(
    ContextTissueType,
    ContextOrgan                            // Summer 2016 Eval: use organs as tissue types
  )
}


object ReachEntityLookup {

  /** Type alias for a sequence of KB search accessors. */
  type KBSearchSequence = Seq[IMKBMentionLookup]

}
