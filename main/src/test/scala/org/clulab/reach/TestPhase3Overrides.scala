package org.clulab.reach

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.Speciated._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._

/**
  * Test that our override KB works properly for NER and grounding.
  *   Written by: Tom Hicks. 12/26/2016.
  *   Last Modified: Update for Phase3 override version of 1/17/2016.
  */
class TestPhase3Overrides extends FlatSpec with Matchers {
  val GGP = "Gene_or_gene_product"
  val Protein = "Protein"

  val ggp1 = """
    14-3-3 beta, 14-3-3-beta, 14-3-3-beta_V, 4E-BP1, 4E-BP1_pS65,
    4EBP1_pS65_V, 4EBP1_V, 53BP1, alpha-tublin, AMPK alpha,
    AMPKa, AMPKa_C, AMPKa_pT172, AMPK_pT172_V, Androgen Receptor,
    AR, AR_V, ATM, ATM_pS1981, ATM_pS1981_mouse,
    ATR, ATR_pS428, b-Catenin, b-Catenin_V, B-Raf,
    B-Raf_pS445, BAD, Bad, Bad_pS112, BAD_pS112_C,
    Bak, BAK1, BAK_C, BAX, Bax,
    BAX_C, Bcl-xL, BCL-XL_C, BCL2A1, Bcl2A1,
    BCL2L1, BCL2L11, BCL2_C, beta Catenin, BID,
    Bid, BID_C, Bim, BIM_V, BRAF,
    bRAF_mouse, bRAF_pS445, c-Jun, c-JUN_pS73, c-Jun_pS73,
    c-Kit, c-KIT_V, c-Myc, C-Raf, and CASP7 are override GGPs.
  """

  val ggp1_ids = Seq(
    "P31946", "P31946", "P31946", "Q13541", "Q13541",
    "Q13541", "Q13541", "Q12888", "Q71U36", "Q13131",
    "Q13131", "Q13131", "Q13131", "Q13131", "P10275",
    "P10275", "P10275", "Q13315", "Q13315", "Q13315",
    "Q13535", "Q13535", "P35222", "P35222", "P15056",
    "P15056", "Q92934", "Q92934", "Q92934", "Q92934",
    "Q16611", "Q16611", "Q16611", "Q07812", "Q07812",
    "Q07812", "Q07817", "Q07817", "Q16548", "Q16548",
    "Q07817", "O43521", "Q16548", "P35222", "P55957",
    "P55957", "P55957", "O43521", "O43521", "P15056",
    "P15056", "P15056", "P05412", "P05412", "P05412",
    "P10721", "P10721", "P01106", "P04049", "P55210"
  )


  val ggpN = """
    PLK1, PRKAA1, PRKCA, PRKCD, PTGS2,
    PTK2, RAD51, Rad51, RAD51_mouse, RAF1,
    Rb, RB1, Rb_pS807_S811, Rb_pS807_V, Rb_V_mouse,
    RPS6, RPS6KB1, PKCa, PKCalpha, S6_pS235_S236,
    S6_pS235_V, S6_pS240_S244, S6_pS240_V, SERPINE1, Shc,
    SHC1, SHC_pY317, Shc_pY317, SMAD3, Smad3,
    STAT3, Stat3, Stat3_pY705, STAT3_pY705_V, STAT5A,
    Stat5a, STAT5_V, Stathmin 1, Stathmin-1, Stathmin_V,
    STMN1, TP53, TP53BP1, TSC2, TSC2/Tuberin,
    TSC2_C, TSC2_pT1462, TUBA1A, Tuberin, Tuberin_pT1462,
    XRCC1, YAP, YAP1, YAP_pS127, YAP_V,
    YB1, YB1_pS102, YBI_pS102, YBX1, and YWHAB are override GGPs.
  """

  val ggpN_ids = Seq(
    "P53350", "Q13131", "P17252", "Q05655", "P35354",
    "Q05397", "Q06609", "Q06609", "Q06609", "P04049",
    "P06400", "P06400", "P06400", "P06400", "P06400",
    "P62753", "P23443", "P17252", "P17252", "P62753",
    "P62753", "P62753", "P62753", "P05121", "P29353",
    "P29353", "P29353", "P29353", "P84022", "P84022",
    "P40763", "P40763", "P40763", "P40763", "P42229",
    "P42229", "P42229", "P16949", "P16949", "P16949",
    "P16949", "P04637", "Q12888", "P49815", "P49815",
    "P49815", "P49815", "Q71U36", "P49815", "P49815",
    "P18887", "P46937", "P46937", "P46937", "P46937",
    "P67809", "P67809", "P67809", "P67809", "P31946"
  )


  /** Override test driver method. */
  def testMentions (
    text: String,
    ids: Seq[String],
    label: String,
    displayLabel: Option[String] = None,
    debug: Boolean = false
  ): Unit = {
    val mentions = getBioMentions(text)

    if (debug)                              // allow debugging on group-by-group basis
      printMentions(Try(mentions), debug)

    text should "have expected number of results" in {
      mentions should not be (empty)
      mentions should have size (ids.size)
    }

    it should s"have labeled all mentions as ${label}" in {
      mentions.count(_ matches label) should be (ids.size)
    }

    if (displayLabel.isDefined) {
      it should s"have display labeled all mentions as ${displayLabel}" in {
        mentions.count(_.displayLabel == displayLabel.get) should be (ids.size)
      }
    }

    it should "have grounded all mentions as Human" in {
      mentions.forall(m => m.grounding.isDefined &&
        Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
    }

    it should "match expected grounding IDs" in {
      for ((m, ndx) <- mentions.zipWithIndex) {
        m.grounding.isDefined && (m.grounding.get.id == ids(ndx)) should be (true)
      }
    }
  }

  // Run the actual tests:
  testMentions(ggp1, ggp1_ids, GGP, Some(Protein))
  testMentions(ggpN, ggpN_ids, GGP, Some(Protein))

}
