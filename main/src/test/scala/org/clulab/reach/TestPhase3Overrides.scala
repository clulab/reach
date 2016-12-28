package org.clulab.reach

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.Speciated._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._

/**
  * Test that our override KB works properly for NER and grounding.
  *   Written by: Tom Hicks. 12/26/2016.
  *   Last Modified: Add 132 Mouse tests which will not pass until NER KB loader is updated.
  */
class TestPhase3Overrides extends FlatSpec with Matchers {
  val GGP = "Gene_or_gene_product"
  val Protein = "Protein"
  val Goat = SpeciesNameSet("goat")
  val Mouse = SpeciesNameSet("mouse")
  val Rabbit = SpeciesNameSet("rabbit")
  val Rat = SpeciesNameSet("rat")

  // test data sets
  val ggp1 = "RPA32 is an override GGP."
  val ggp1_ids = Seq("P15927")

  val ggpG = """Beclin, BECN1, CD274, Pdcd-1L1, and XBP1 are override GGPs."""
  val ggpG_ids = Seq("Q14457", "Q14457", "Q9NZQ7", "Q9NZQ7", "P17861")

  // TODO: following test will not pass until the NER KB loader
  // is updated to read the Phase3 Override file
  val ggpM = """
    14-3-3 epsilon, 14-3-3-epsilon, ADAR1, AKT1S1, Annexin I,
    Annexin VII, Annexin-I, Annexin-VII, ANXA1, ANXA7,
    BAP1, BCL2, Bcl2, BiP-GRP78, BiP/GRP78,
    c-Met, CASP8, Caspase-8, Caspase_8_mouse, CCND3,
    CCNE1, CD171, CD171 (L1), CD29, CD31,
    CD44, CD49b, CHEK1, CHEK2, CHK1,
    Chk1, Chk2, CHK2_mouse, Cyclin D3, Cyclin E1,
    Cyclin-D3, Cyclin-E1, CyclinE1_V_mouse, Cyclophilin F, Cyclophilin-F,
    DIABLO, E2F-1, E2F1, EMA, ENY2,
    Epithelial Membrane Antigen, EPPK1, ERBB2, ErbB2/HER2, ERCC1,
    ERRFI1, ERRFI1/MIG6, GAPDH, GATA3, GATA3_mouse,
    H2AX, H2AX (phospho S140), H2AX_pS140, H2BFM, HER2,
    Hif-1 alpha, Hif-1-alpha, HIF1A, HSP27, HSP27_C_mouse,
    HSPA5, ITGA2, ITGB1, L1CAM, MET,
    MIG6, N-Ras, NDUFB4, NRAS, PAI-1,
    PAI-1_mouse, PCNA, PCNA_mouse, PECAM1, PI3K p110 beta,
    PI3K-p110-b, PIK3CB, Porin, PPIF, PRAS40,
    RAD50, Rad50, Rb, RB1, Rb_V_mouse,
    RHEB, Rheb, S6, S6 Ribosomal Protein, SCD,
    SERPINE1, SF2, SF2/ASF, Smac, Smac/Diablo,
    SMAD4, Smad4, SNAI1, Snail, SOD1,
    SRC, Src, SRC_V_mouse, SRSF1, SYK,
    Syk, TAU, Tau, TGM2, Transglutaminase,
    Transglutaminase II, TWIST, Twist, TWIST2, Ubiquityl Histone H2B,
    Ubq-Histone-H2B, UGT1A, UGT1A1, VDAC1, VDAC1/Porin,
    VHL-EPPK1, VHL/EPPK1**, VIM, Vimentin, XPA,
    XPF, and YWHAE are override GGPs.
  """

  val ggpM_ids = Seq(
    "P62258", "P62258", "Q13085", "Q96B36", "P04083",
    "P20073", "P04083", "P20073", "P04083", "P20073",
    "Q92560", "P10415", "P10415", "P11021", "P11021",
    "P08581", "Q14790", "Q14790", "Q14790", "P30281",
    "P24864", "P19320", "P19320", "P05556", "P16284",
    "P16070", "P17301", "O14757", "O96017", "O14757",
    "O14757", "O96017", "O96017", "P30281", "P24864",
    "P30281", "P24864", "P24864", "P30405", "P30405",
    "Q9NR28", "Q01094", "Q01094", "P15941", "Q9NPA8",
    "P15941", "P58107", "P04626", "P04626", "P07992",
    "Q9UJM3", "Q9UJM3", "P04406", "P23771", "P23771",
    "P16104", "P16104", "P16104", "P0C1H6", "P04626",
    "Q16665", "Q16665", "Q16665", "P04792", "P04792",
    "P11021", "P17301", "P05556", "P19320", "P08581",
    "Q9UJM3", "P01111", "O95168", "P01111", "P05121",
    "P05121", "P12004", "P12004", "P16284", "P42338",
    "P42338", "P42338", "P21796", "P30405", "Q96B36",
    "Q92878", "Q92878", "P06400", "P06400", "P06400",
    "Q15382", "Q15382", "P62753", "P62753", "O00767",
    "P05121", "Q07955", "Q07955", "Q9NR28", "Q9NR28",
    "Q13485", "Q13485", "O95863", "O95863", "P00441",
    "P12931", "P12931", "P12931", "Q07955", "P43405",
    "P43405", "P10636", "P10636", "P21980", "P21980",
    "P21980", "Q8WVJ9", "Q8WVJ9", "Q8WVJ9", "P0C1H6",
    "P0C1H6", "P22309", "P22309", "P21796", "P21796",
    "P58107", "P58107", "P08670", "P08670", "P23025",
    "Q92889", "P62258"
  )

  /** Override test driver method. */
  def testMentions (
    text: String,
    ids: Seq[String],
    label: String,
    displayLabel: Option[String] = None,
    groundedSpecies: Option[SpeciesNameSet] = None,
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

    if (groundedSpecies.isDefined) {
      val species = groundedSpecies.get
      it should "have grounded all mentions as ${species}" in {
        mentions.forall(m => m.grounding.isDefined &&
          Speciated.isMemberOf(m.grounding.get.species, species)) should be (true)
      }
    }

    it should "match expected grounding IDs" in {
      for ((m, ndx) <- mentions.zipWithIndex) {
        m.grounding.isDefined && (m.grounding.get.id == ids(ndx)) should be (true)
      }
    }
  }

  // Run the actual tests:
  testMentions(ggp1, ggp1_ids, GGP, Some(Protein), Some(Rat))
  testMentions(ggpG, ggpG_ids, GGP, Some(Protein), Some(Goat))
  // TODO: following test will not pass until the NER KB loader is updated
  // testMentions(ggpM, ggpM_ids, GGP, Some(Protein), Some(Mouse), true)

}
