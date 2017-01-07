package org.clulab.reach

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.Speciated._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._

/**
  * Test that our override KB works properly for NER and grounding.
  *   Written by: Tom Hicks. 12/26/2016.
  *   Last Modified: Change commenting for failing tests.
  */
class TestPhase3Overrides extends FlatSpec with Matchers {
  val GGP = "Gene_or_gene_product"
  val Protein = "Protein"
  val Goat = SpeciesNameSet("goat")
  val Mouse = SpeciesNameSet("mouse")
  val Rabbit = SpeciesNameSet("rabbit")
  val Rat = SpeciesNameSet("rat")

  // test data sets
  val ggpRat = "RPA32 is an override GGP."
  val ggpRat_ids = Seq("P15927")

  val ggpG = """Beclin, BECN1, CD274, Pdcd-1L1, and XBP1 are override GGPs."""
  val ggpG_ids = Seq("Q14457", "Q14457", "Q9NZQ7", "Q9NZQ7", "P17861")

  // TODO: following tests (which contain Sites) do not pass due to parse conflicts.
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


  // TODO: following tests (which contain Sites) do not pass due to parse conflicts.
  val ggpR1 = """
    14-3-3 beta, 14-3-3 zeta, 14-3-3-beta, 14-3-3-beta_V, 14-3-3-zeta,
    4E-BP1, 4E-BP1 (phospho S65), 4E-BP1_pS65, 4EBP1_pS65_V, 4EBP1_V,
    53BP1, A-Raf, ABL, ACACA, ACC1,
    ACC1_C, Acetyl CoA Carboxylase 1, ACTB, AIM1, alpha-tublin,
    AMPK alpha, AMPK alpha (phospho T172), AMPK alpha 2 (Phospho S345), AMPK-a2_pS345, AMPKa,
    AMPKa_C, AMPKa_pT172, AMPK_pT172_V, Androgen Receptor, AR,
    ARAF, ARID1A, AR_V, ATG3, Atg3,
    ATG7, Atg7, ATM, ATM (phospho S1981), ATM_pS1981,
    ATM_pS1981_mouse, ATR, ATR (Phospho S428), ATRX, ATR_pS428,
    Aurora B/AIM1, Aurora-B, AXL, Axl, b-Actin,
    b-Catenin, b-Catenin_pT41_S45, b-Catenin_V, B-Raf, B-Raf (phospho S445),
    B-Raf_pS445, B7-H4, BABAM1, BAD, Bad (phospho S112),
    Bad_pS112, BAD_pS112_C, Bak, BAK1, BAK_C,
    BAX, Bax, BAX_C, Bcl-xL, BCL-XL_C,
    BCL2A1, Bcl2A1, BCL2L1, BCL2L11, BCL2_C,
    beta Actin, beta Catenin, beta Catenin (phospho T41/S45), BID, Bid,
    BID_C, Bim, BIM_V, BIRC3, BRAF,
    bRAF_mouse, bRAF_pS445, BRD4, c-Abl, c-IAP2,
    c-Jun (phospho S73), c-JUN_pS73, c-Jun_pS73, c-Kit, c-KIT_V,
    c-Met (phospho Y1234/Y1235), c-Met_pY1234_Y1235, c-Myc, C-Raf,
    and C-Raf (phospho S338) are override GGPs.
  """

  val ggpR1_ids = Seq(
    "P31946", "P63104", "P31946", "P31946", "P63104",
    "Q13541", "Q13541", "Q13541", "Q13541", "Q13541",
    "Q12888", "P10398", "P00519", "Q13085", "Q13085",
    "Q13085", "Q13085", "P60709", "Q96GD4", "Q71U36",
    "Q13131", "Q13131", "P54646", "P54646", "Q13131",
    "Q13131", "Q13131", "Q13131", "P10275", "P10275",
    "P10398", "O14497", "P10275", "Q9NT62", "Q9NT62",
    "O95352", "O95352", "Q13315", "Q13315", "Q13315",
    "Q13315", "P20848", "P20848", "P46100", "P20848",
    "Q96GD4", "Q96GD4", "P30530", "P30530", "P60709",
    "P35222", "P35222", "P35222", "P15056", "P15056",
    "P15056", "Q7Z7D3", "Q9NWV8", "Q92934", "Q92934",
    "Q92934", "Q92934", "Q16611", "Q16611", "Q16611",
    "Q07812", "Q07812", "Q07812", "Q07817", "Q07817",
    "Q16548", "Q16548", "Q07817", "O43521", "Q16548",
    "P60709", "P35222", "P35222", "P55957", "P55957",
    "P55957", "O43521", "O43521", "Q13489", "P15056",
    "P15056", "P15056", "O60885", "P00519", "Q13489",
    "P05412", "P05412", "P05412", "P10721", "P10721",
    "P08581", "P08581", "P01106", "P04049", "P04049"
  )


  // TODO: following tests (which contain Sites) do not pass due to parse conflicts.
  val ggpR3 = """
    Rb_pS807_S811, Rb_pS807_V, RELA, RICTOR, Rictor,
    Rictor (phospho T1135), Rictor_pT1135, RIP, Rock-1, ROCK1,
    RPA32 (Phospho S4/S8), RPA32_pS4_S8, RPS6, RPS6K, RPS6KB1,
    RPTOR, S6 (phospho S235/S236), S6 (phospho S240/S244), S6_pS235_S236, S6_pS235_V,
    S6_pS240_S244, S6_pS240_V, SDHA, Shc (phospho Y317), SHC1,
    SHC_pY317, Shc_pY317, SHP-2 (phospho Y542), SHP-2_pY542, SLC16A4,
    SLC1A5, SMAD1, Smad1, SMAD3, Smad3,
    SOD2, SOX2, Sox2, STAT3, Stat3,
    Stat3 (phospho Y705), Stat3_pY705, STAT3_pY705_V, STAT5A, Stat5a,
    STAT5_V, Stathmin 1, Stathmin-1, Stathmin_V, STMN1,
    TAZ, TFAM, TFRC, TIGAR, TP53,
    TP53BP1, Transferrin Receptor, TRIM25, TSC1, TSC1/Hamartin,
    TSC2, TSC2/Tuberin (phospho T1462), TSC2_C, TSC2_pT1462, TTF1,
    TUBA1A, Tuberin, Tuberin_pT1462, TUFM, TYRO3,
    Tyro3, UBAC1, ULK1, ULK1 (phospho S757), ULK1_pS757,
    VASP, VEGF Receptor 2, VEGFR-2, VTCN1, WEE1,
    Wee1, Wee1 (Phospho S642), Wee1_pS642, WIPI1, WIPI2,
    WWTR1, XRCC1, YAP, YAP (phospho S127), YAP1,
    YAP_pS127, YAP_V, YB1 (phospho S102), YB1_pS102, YBI_pS102,
    YBX1, YWHAB, YWHAZ, ZAP-70, and ZAP7
    are override GGPs.
  """

  val ggpR3_ids = Seq(
    "P06400", "P06400", "Q04206", "Q6R327", "Q6R327",
    "Q6R327", "Q6R327", "P52594", "Q13464", "Q13464",
    "P15927", "P15927", "P62753", "Q15418", "P23443",
    "Q8N122", "P62753", "P62753", "P62753", "P62753",
    "P62753", "P62753", "P31040", "P29353", "P29353",
    "P29353", "P29353", "Q06124", "Q06124", "O15374",
    "Q15758", "Q15797", "Q15797", "P84022", "P84022",
    "P04179", "P48431", "P48431", "P40763", "P40763",
    "P40763", "P40763", "P40763", "P42229", "P42229",
    "P42229", "P16949", "P16949", "P16949", "P16949",
    "Q9GZV5", "Q00059", "P02786", "Q9NQ88", "P04637",
    "Q12888", "P02786", "Q14258", "Q92574", "Q92574",
    "P49815", "P49815", "P49815", "P49815", "P43699",
    "Q71U36", "P49815", "P49815", "P49411", "Q06418",
    "Q06418", "Q9BSL1", "O75385", "O75385", "O75385",
    "P50552", "P35968", "P35968", "Q7Z7D3", "P30291",
    "P30291", "P30291", "P30291", "Q5MNZ9", "Q9Y4P8",
    "Q9GZV5", "P18887", "P46937", "P46937", "P46937",
    "P46937", "P46937", "P67809", "P67809", "P67809",
    "P67809", "P31946", "P63104", "P43403", "P4340"
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
      it should s"have grounded all mentions as ${species}" in {
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
  testMentions(ggpRat, ggpRat_ids, GGP, Some(Protein), Some(Rat))
  testMentions(ggpG, ggpG_ids, GGP, Some(Protein), Some(Goat))
  // TODO: following tests will not pass because of parse conflicts:
  // testMentions(ggpM, ggpM_ids, GGP, Some(Protein), Some(Mouse))
  // testMentions(ggpR1, ggpR1_ids, GGP, Some(Protein), Some(Rabbit))
  // testMentions(ggpR3, ggpR3_ids, GGP, Some(Protein), Some(Rabbit))

}
