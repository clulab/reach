package org.clulab.reach.grounding

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.reach.grounding.AzFailsafeKBML._
import org.clulab.reach.grounding.ReachEntityLookup._
import org.clulab.reach.grounding.ReachIMKBMentionLookups._
import org.clulab.reach.mentions._

import scala.collection.JavaConverters._
import scala.collection.mutable.Map

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
    val factory = new AdHocIMKBFactory
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
      case "BioProcess" => augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("BioProcess"))
      case "CellLine" => augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("CellLine"))
      case "CellType" => augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("CellType"))
      case "Cellular_component" => augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("Cellular_component"))
      case "Complex" | "GENE" | "Gene_or_gene_product" | "Protein" =>
        augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("Protein"))
      case "Disease" =>  augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("Disease"))
      case "Family" =>  augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("Family"))
      case "Organ" => augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("Organ"))
      case "Simple_chemical" => augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("Simple_chemical"))
      case "Site" => augmentMention(mention, extraKBs /*++ ReachIMKBMentionLookups.configuredKBML("Site")*/)
      case "Species" => augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("Species"))
      case "TissueType" => augmentMention(mention, extraKBs ++ ReachIMKBMentionLookups.configuredKBML("TissueType"))
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
    mention
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

}


object ReachEntityLookup {

  /** Type alias for a sequence of KB search accessors. */
  type KBSearchSequence = Seq[IMKBMentionLookup]

}
