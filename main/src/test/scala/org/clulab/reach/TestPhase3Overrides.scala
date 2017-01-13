package org.clulab.reach

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.Speciated._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._

/**
  * Test that our override KB works properly for NER and grounding.
  *   Written by: Tom Hicks. 12/26/2016.
  *   Last Modified: Redo for removal of species in KB.
  */
class TestPhase3Overrides extends FlatSpec with Matchers {
  val GGP = "Gene_or_gene_product"
  val Protein = "Protein"

  val ggp1 = """
    14-3-3 beta, 14-3-3 epsilon, 14-3-3 zeta, 14-3-3-beta, 14-3-3-beta_V,
    14-3-3-epsilon, 14-3-3-zeta, 4E-BP1, 4E-BP1_pS65, 4EBP1_pS65_V,
    4EBP1_V, 53BP1, A-Raf, ABL, ACACA,
    ACC1, ACC1_C, Acetyl CoA Carboxylase 1, ACTB, ADAR1,
    AIM1, AKT1S1, alpha-tublin, AMPK alpha, AMPK alpha 2,
    AMPK-a2_pS345, AMPKa, AMPKa_C, AMPKa_pT172, AMPK_pT172_V,
    Androgen Receptor, Annexin I, Annexin VII, Annexin-I, Annexin-VII,
    ANXA1, ANXA7, AR, ARAF, ARID1A,
    AR_V, ATG3, Atg3, ATG7, Atg7,
    ATM, ATM_pS1981, ATM_pS1981_mouse, ATR, ATRX,
    ATR_pS428, Aurora B/AIM1, Aurora-B, AXL, Axl,
    b-Actin, b-Catenin, b-Catenin_pT41_S45, b-Catenin_V, B-Raf are override GGPs.
  """

  val ggp1_ids = Seq(
    "P31946", "P62258", "P63104", "P31946", "P31946",
    "P62258", "P63104", "Q13541", "Q13541", "Q13541",
    "Q13541", "Q12888", "P10398", "P00519", "Q13085",
    "Q13085", "Q13085", "Q13085", "P60709", "Q13085",
    "Q96GD4", "Q96B36", "Q71U36", "Q13131", "P54646",
    "P54646", "Q13131", "Q13131", "Q13131", "Q13131",
    "P10275", "P04083", "P20073", "P04083", "P20073",
    "P04083", "P20073", "P10275", "P10398", "O14497",
    "P10275", "Q9NT62", "Q9NT62", "O95352", "O95352",
    "Q13315", "Q13315", "Q13315", "P20848", "P46100",
    "P20848", "Q96GD4", "Q96GD4", "P30530", "P30530",
    "P60709", "P35222", "P35222", "P35222", "P15056"
  )


  val ggpN = """
    Transferrin Receptor, Transglutaminase, Transglutaminase II, TRIM25, TSC1,
    TSC1/Hamartin, TSC2, TSC2/Tuberin, TSC2_C, TSC2_pT1462,
    TTF1, TUBA1A, Tuberin, Tuberin_pT1462, TUFM,
    TWIST, Twist, TWIST2, TYRO3, Tyro3,
    UBAC1, Ubiquityl Histone H2B, Ubq-Histone-H2B, UGT1A, UGT1A1,
    ULK1, ULK1_pS757, VASP, VDAC1, VDAC1/Porin,
    VEGF Receptor 2, VEGFR-2, VHL-EPPK1, VHL/EPPK1**, VIM,
    Vimentin, VTCN1, WEE1, Wee1, Wee1_pS642,
    WIPI1, WIPI2, WWTR1, XBP1, XPA,
    XPF, XRCC1, YAP, YAP1, YAP_pS127,
    YAP_V, YB1, YB1_pS102, YBI_pS102, YBX1,
    YWHAB, YWHAE, YWHAZ, ZAP-70, ZAP70 are override GGPs.
  """

  val ggpN_ids = Seq(
    "P02786", "P21980", "P21980", "Q14258", "Q92574",
    "Q92574", "P49815", "P49815", "P49815", "P49815",
    "P43699", "Q71U36", "P49815", "P49815", "P49411",
    "Q8WVJ9", "Q8WVJ9", "Q8WVJ9", "Q06418", "Q06418",
    "Q9BSL1", "P0C1H6", "P0C1H6", "P22309", "P22309",
    "O75385", "O75385", "P50552", "P21796", "P21796",
    "P35968", "P35968", "P58107", "P40337", "P58107", "P08670",
    "P08670", "Q7Z7D3", "P30291", "P30291", "P30291",
    "Q5MNZ9", "Q9Y4P8", "Q9GZV5", "P17861", "P23025",
    "Q92889", "P18887", "P46937", "P46937", "P46937",
    "P46937", "P67809", "P67809", "P67809", "P67809",
    "P31946", "P62258", "P63104", "P43403", "P43403"
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
