package org.clulab.reach.grounding

import scala.collection.JavaConverters._
import scala.collection.mutable.Map

import com.typesafe.config.{Config, ConfigFactory}

import org.clulab.odin._
import org.clulab.reach._
import org.clulab.reach.mentions._
import org.clulab.reach.grounding.AzFailsafeKBML._
import org.clulab.reach.grounding.ReachEntityLookup._
import org.clulab.reach.grounding.ReachIMKBMentionLookups._

/**
  * Class which implements project internal methods to ground entities.
  *   Written by Tom Hicks. 11/9/2015.
  *   Last Modified: Update for Bioentities KBs.
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
      val metaInfo = new IMKBMetaInfo(kbFilename = Some(fname.asInstanceOf[String]))
      new IMKBMentionLookup(AdHocIMKBFactory.make(metaInfo))
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
      case "Site" => augmentMention(mention, siteSeq)
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
    if (moreKBs.nonEmpty) extraKBs = moreKBs
  }

  /** KB search sequence to use for Fallback grounding: when all others fail. */
  val azFailsafeSeq: KBSearchSequence = Seq(AzFailsafe)

  // instantiate the various search sequences, each sequence for a different label:
  val bioProcessSeq: KBSearchSequence = extraKBs ++ Seq( StaticBioProcess )
  val cellTypeSeq: KBSearchSequence = extraKBs ++ Seq( ContextCellType )

  val cellLineSeq: KBSearchSequence = extraKBs ++ Seq(
    ContextCellLine,                        // Cellosaurus
    ContextCellLine2                        // atcc
  )

  val cellComponentSeq: KBSearchSequence = extraKBs ++ Seq(
    StaticCellLocation,                     // GO subcellular KB
    StaticCellLocation2,                    // Uniprot subcellular KB
    ModelGendCellLocation
  )

  val chemicalSeq: KBSearchSequence = extraKBs ++ Seq(
    StaticChemical,                         // PubChem
    StaticDrug,                             // HMS LINCS drugs
    // StaticMetabolite,                    // REPLACED by PubChem
    ModelGendChemical
  )

  val familySeq: KBSearchSequence = extraKBs ++ Seq(
    StaticProteinFamily0,                   // Bioentities families
    StaticProteinFamily,                    // PFAM families
    StaticProteinFamily2,                   // InterPro families
    ModelGendProteinAndFamily
  )

  val organSeq: KBSearchSequence = extraKBs ++ Seq( ContextOrgan )

  val proteinSeq: KBSearchSequence = extraKBs ++ Seq(
    StaticProteinComplex,                   // Bioentities complexes
    StaticProtein,                          // Uniprot proteins
    StaticGene,
    ModelGendProteinAndFamily
  )

  val siteSeq: KBSearchSequence = extraKBs ++ Seq() // nothing to add the extras to

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
