package edu.arizona.sista.reach.grounding

import scala.collection.JavaConverters._
import scala.collection.mutable.Map

import com.typesafe.config.{Config, ConfigFactory}

import edu.arizona.sista.odin._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.grounding.ReachGrounder._
import edu.arizona.sista.reach.grounding.ReachIMKBMentionLookups._

/**
  * Class which implements project internal methods to ground entities.
  *   Written by Tom Hicks. 11/9/2015.
  *   Last Modified: Move singletons to companion object.
  */
class ReachGrounder extends DarpaFlow {

  /** Local implementation of darpa flow trait: use project specific KBs to ground
      and augment given mentions. */
  def apply (mentions: Seq[Mention], state: State): Seq[Mention] = mentions map {
    case tm: BioTextBoundMention => resolveMention(tm, state)
    case m => m
  }


  /** Return a new instance of an AdHoc KBML created using the given config map. */
  private def addAdHocFile (fileDef: Config): Option[IMKBMentionLookup] = {
    if (fileDef.isEmpty) return None        // sanity check
    val params: Map[String, _ >: String] = fileDef.root.unwrapped.asScala
    params.get("kb").map { fname =>
      new IMKBMentionLookup(adHocFactory.make(fname.asInstanceOf[String]))
    }
  }

  /** Search a sequence of KB accessors, which sequence determined by the main mention label. */
  private def resolveMention (mention: BioMention, state: State): Mention = {
    mention.label match {
      case "Bioprocess" => augmentMention(mention, state, bioProcessSeq)
      case "CellLine" => augmentMention(mention, state, cellLineSeq)
      case "CellType" => augmentMention(mention, state, cellTypeSeq)
      case "Cellular_component" => augmentMention(mention, state, cellComponentSeq)
      case "Complex" | "GENE" | "Gene_or_gene_product" | "Protein" =>
        augmentMention(mention, state, proteinSeq)
      case "Family" =>  augmentMention(mention, state, familySeq)
      case "Organ" => augmentMention(mention, state, organSeq)
      case "Simple_chemical" => augmentMention(mention, state, chemicalSeq)
      case "Species" => augmentMention(mention, state, speciesSeq)
      case _ =>  augmentMention(mention, state, azFailsafeSeq)
    }
  }

  private def augmentMention (mention: BioMention, state: State,
                              searchSequence: KBSearchSequence): Mention = {
    searchSequence.foreach { kbml =>
      val resolutions = kbml.resolve(mention)
      if (resolutions.isDefined) {
        mention.nominate(resolutions)       // save candidate resolutions in mention
        return mention
      }
    }
    // if reach here, we assign a failsafe backup ID:
    mention.nominate(azFailsafe.resolve(mention))
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

  // instantiate the various search sequences, each sequence for a different label:
  val bioProcessSeq: KBSearchSequence = extraKBs ++ Seq( staticBioProcessKBML )
  val cellLineSeq: KBSearchSequence = extraKBs ++ Seq( contextCellLineKBML )
  val cellTypeSeq: KBSearchSequence = extraKBs ++ Seq( contextCellTypeKBML )

  val cellComponentSeq: KBSearchSequence = extraKBs ++ Seq(
    staticCellLocationKBML,                 // GO subcellular KB
    staticCellLocationKBML2,                // Uniprot subcellular KB
    manualCellLocationKBML,
    gendCellLocationKBML
  )

  val chemicalSeq: KBSearchSequence = extraKBs ++ Seq(
    staticChemicalKBML,
    staticMetaboliteKBML,
    manualChemicalKBML,
    gendChemicalKBML
  )

  val familySeq: KBSearchSequence = extraKBs ++ Seq(
    staticProteinFamilyKBML,
    manualProteinFamilyKBML,
    modelGendProteinAndFamily
  )

  val organSeq: KBSearchSequence = extraKBs ++ Seq( contextOrganKBML )

  val proteinSeq: KBSearchSequence = extraKBs ++ Seq(
    staticProteinKBML,
    manualProteinKBML,
    modelGendProteinAndFamily
  )

  val speciesSeq: KBSearchSequence = extraKBs ++ Seq( contextSpeciesKBML )
}


object ReachGrounder {

  /** Type alias for a sequence of KB search accessors. */
  type KBSearchSequence = Seq[IMKBMentionLookup]

  /** Factory class for instantiating multiple new AdHoc KBMLs. */
  val adHocFactory = new AdHocIMKBFactory

  /** KB to use for Fallback grounding: when all others fail. */
  val azFailsafe = new AzFailsafeKBML
  val azFailsafeSeq: KBSearchSequence = Seq(azFailsafe)

  // val staticTissueType = staticTissueTypeKBML()  // CURRENTLY UNUSED

  // this KB is used in multiple sequences, so allocate it once:
  val modelGendProteinAndFamily = gendProteinKBML // families included in generated KB

}
