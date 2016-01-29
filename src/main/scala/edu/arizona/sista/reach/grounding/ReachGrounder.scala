package edu.arizona.sista.reach.grounding

import scala.collection.JavaConverters._
import scala.collection.mutable.Map

import com.typesafe.config.{Config, ConfigFactory}

import edu.arizona.sista.odin._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.mentions._

/**
  * Class which implements project internal methods to ground entities.
  *   Written by Tom Hicks. 11/9/2015.
  *   Last Modified: Redo to optionally insert static AdHoc KB before standard KBs.
  */
class ReachGrounder extends DarpaFlow {

  /** An exception in case we somehow fail to assign an ID during resolution. */
  case class NoFailSafe(message:String) extends Exception(message)

  /** Project local sequence for resolving entities: check local facade KBs in this order:
    * 2. Protein Families
    * 3. Proteins
    * 4. Biological Processes
    * 5. Small Molecules (metabolites and chemicals)
    * 6. Subcellular Locations
    * 7. AZ Failsafe KB (failsafe: always generates an ID in a non-official, local namespace)
    */
  protected val standardSearchSequence: Seq[IMKBMentionLookup] = Seq(
    new StaticProteinFamilyKBML,
    new ManualProteinFamilyKBML,
    new StaticProteinKBML,
    new ManualProteinKBML,
    // NB: generated protein families are included in the generated protein KB:
    new GendProteinKBML,

    new StaticBioProcessKBML,

    new StaticChemicalKBML,
    new StaticMetaboliteKBML,
    new ManualChemicalKBML,
    new GendChemicalKBML,

    new StaticCellLocationKBML,
    new ManualCellLocationKBML,
    new GendCellLocationKBML,

    new ContextCellTypeKBML,
    new ContextSpeciesKBML,
    new ContextCellLineKBML,
    new ContextOrganKBML,
    new StaticTissueTypeKBML,

    new AzFailsafeKBML
  )

  /** Local implementation of darpa flow trait: use project specific KBs to ground
      and augment given mentions. */
  def apply (mentions: Seq[Mention], state: State): Seq[Mention] = mentions map {
    case tm: BioTextBoundMention => resolveAndAugment(tm, state)
    case m => m
  }

  /** Single instance of factory class for instantiating new AdHoc KBMLs. */
  private val adHocFactory = new AdHocIMKBFactory

  /** Return a new instance of an AdHoc KBML created using the given config map. */
  private def addAdHocFile (fileDef: Config): Option[IMKBMentionLookup] = {
    if (fileDef.isEmpty) return None        // sanity check
    val params: Map[String, _ >: String] = fileDef.root.unwrapped.asScala
    params.get("kb").map { fname =>
      new IMKBMentionLookup(adHocFactory.make(fname.asInstanceOf[String]))
    }
  }

  /** Search the KB accessors in sequence, use the first one which resolves the given mention. */
  private def resolveAndAugment (mention: BioMention, state: State): Mention = {
    searchSequence.foreach { kbml =>
      // There may be more than one entry returned in the set, so just take any one, for now:
      val resolution = kbml.resolve(mention).flatMap(kbeset => kbeset.headOption)
      if (!resolution.isEmpty) {
        mention.ground(resolution.get.namespace, resolution.get.id)
        return mention
      }
    }
    // we should never get here because our accessors include a failsafe ID assignment
    throw NoFailSafe(s"ReachGrounder failed to assign an ID to ${mention.displayLabel} '${mention.text}' in S${mention.sentence}")
  }


  // Reach Grounder Initialization
  //

  protected var searchSequence: Seq[IMKBMentionLookup] = standardSearchSequence

  // Read config file to determine which (if any) additional knowledge bases to include
  val config = ConfigFactory.load()
  if (config.hasPath("grounding.adHocFiles")) {
    val moreKBs = config.getConfigList("grounding.adHocFiles").asScala.flatMap(addAdHocFile(_))
    println(s"moreKBs=${moreKBs}")                // REMOVE LATER
    println(s"moreKBs.class=${moreKBs.getClass}") // REMOVE LATER
    if (!moreKBs.isEmpty)
      searchSequence = moreKBs ++ standardSearchSequence
    println(s"searchSequence=${searchSequence}")                // REMOVE LATER
    println(s"searchSequence.class=${searchSequence.getClass}") // REMOVE LATER
  }

}
