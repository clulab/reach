package org.clulab.reach

import org.clulab.reach.grounding._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._


/**
  * Test that our override KB works properly for NER and grounding.
  *   Written by: Tom Hicks. 7/8/2016.
  *   Last Modified: Update for updated BE families and complexes of 8/22/2017.
  */
class TestOverrides extends FlatSpec with Matchers {

  val Chemical = "Simple_chemical"
  val Family = "Family"
  val GGP = "Gene_or_gene_product"
  val Protein = "Protein"
  val Site = "Site"

  val ggp1 = "ADAM17, AKT1, AKT2, ASPP1, ASPP2 are GGPs."
  val ggp1_ids = Seq("P78536", "P31749", "P31751", "Q96KQ4", "Q13625")

  val ggp2 = "Casp8, EGF, EGFR, ERK5, GSK3beta are GGPs."
  val ggp2_ids = Seq("Q14790", "P01133", "P00533", "Q13164", "P49841")

  val ggp3 = "HRAS, IGF-1, JNK1, GSK3alpha, KRAS, MAP2K1 are GGPs."
  val ggp3_ids = Seq("P01112", "P05019", "P45983", "P49840", "P01116", "Q02750")

  val ggp4 = "MAP2K2, MAZ, MEK1, MEK2, MEK3, MEK4 are GGPs."
  val ggp4_ids = Seq("P36507", "P56270", "Q02750", "P36507", "P46734", "P45985")

  val ggp5 = "MEK5, MEK6, MEK7, and NRAS are GGPs."
  val ggp5_ids = Seq("Q13163", "P52564", "O14733", "P01111")

  val ggp6 = "p53, RAC1, RhoA, ROCK1, and SAF-1 GGPs. "
  val ggp6_ids = Seq("P04637", "P63000", "P61586", "Q13464", "P56270")

  val ggp7 = "HRAS, H-RAS, KRAS, K-RAS, NRAS, N-RAS are GGPs. "
  val ggp7_ids = Seq("P01112", "P01112", "P01116", "P01116", "P01111", "P01111")

  val fam1 = "ERK, ERK1/2, ERK 1/2, Neuregulin, Neuroregulin, and PI3K are Families. "
  val fam1_ids = Seq("ERK", "ERK", "ERK", "NRG", "NRG", "PI3K") 

  val fam2 = "SMAD, SMAD2/3, SMAD 2/3, and TGFB are important Families. "
  val fam2_ids = Seq("SMAD", "SMAD2_3", "SMAD2_3", "TGFB")

  // Check that these families are based on overrides
  val be1f = """ACOX, BMP, Cadherin, CRISP,
                COX4, COX6a, COX6b, COX7a, COX7b,
                COX8, DVL, ETS, FGF,
                GATA, HSP90, IGFBP, IL1, IRS,
                NOTCH, PKI, RAS, SAA,
                and TGFB are unchanged Families."""
  val be1f_ids = Seq(
    "ACOX", "BMP", "Cadherin", "CRISP",
    "COX4", "COX6A", "COX6B", "COX7A", "COX7B",
    "COX8", "DVL", "ETS", "FGF",
    "GATA", "HSP90", "IGFBP", "IL1", "IRS",
    "Notch", "PKI", "RAS", "SAA",
    "TGFB")

  // Override entries added as synonyms for BE complexes:
  val be2c = """Activin A, Activin AB, Inhibin A, Inhibin B,
                AMPK alpha1beta1gamma1, AMPK alpha1beta1gamma2, AMPK alpha1beta1gamma3,
                AMPK alpha1beta2gamma1, AMPK alpha1beta2gamma2, AMPK alpha2beta1gamma1,
                AMPK alpha2beta2gamma1, AMPK alpha2beta2gamma2, AMPK alpha2beta2gamma3,
                AMPK a1b1g1, AMPK a1b1g2, AMPK a1b1g3,
                AMPK a1b2g1, AMPK a1b2g2, AMPK a2b1g1,
                AMPK a2b2g1, AMPK a2b2g2, AMPK a2b2g3,
                alpha1beta1gamma1, alpha1beta1gamma2, alpha1beta1gamma3,
                alpha1beta2gamma1, alpha1beta2gamma2, alpha2beta1gamma1,
                alpha2beta2gamma1, alpha2beta2gamma2, and alpha2beta2gamma3
                are important complexes."""
  val be2c_ids = Seq(
    "Activin_A", "Activin_AB", "IPR002405", "Inhibin_B",
    "AMPK_A1B1G1", "AMPK_A1B1G2", "AMPK_A1B1G3",
    "AMPK_A1B2G1", "AMPK_A1B2G2", "AMPK_A2B1G1",
    "AMPK_A2B2G1", "AMPK_A2B2G2", "AMPK_A2B2G3",
    "AMPK_A1B1G1", "AMPK_A1B1G2", "AMPK_A1B1G3",
    "AMPK_A1B2G1", "AMPK_A1B2G2", "AMPK_A2B1G1",
    "AMPK_A2B2G1", "AMPK_A2B2G2", "AMPK_A2B2G3",
    "AMPK_A1B1G1", "AMPK_A1B1G2", "AMPK_A1B1G3",
    "AMPK_A1B2G1", "AMPK_A1B2G2", "AMPK_A2B1G1",
    "AMPK_A2B2G1", "AMPK_A2B2G2", "AMPK_A2B2G3" )

  // Override entries added as synonyms for BE families:
  val be2f = """BMP Receptor Type I, BMP Receptor Type II, CAMK2,
                MAPK p38, p38 MAPKs, MEK 1/2,
                NRG1/2, NRG3/4, SMAD 2/3
                are important families."""
  val be2f_ids = Seq(
    "BMP_receptor_type_I", "BMP_receptor_type_II", "CAMK2_complex",
    "p38", "p38", "MEK",
    "NRG_1_2", "NRG_3_4", "SMAD2_3" )

  // Previous Override GGP entries, replaced by BE complexes KB:
  val be3c = """AMPK, AP1, and PI3K are important complexes."""
  val be3c_ids = Seq("AMPK", "AP1", "PI3K")

  // Previous Override GGP entries, replaced by BE family KB:
  val be3f = """AKT, LDH, NFAT, SOD,
                and VEGFR are important families."""
  val be3f_ids = Seq(
    "AKT", "LDH", "NFAT", "SOD", "VEGFR" )

  // Override entries added to give BE families priority over GGP:
  val be4f = """Fos, GST, p38, PDGF, PDGFR, RAF, STAT5, and TSC are important families."""
  val be4f_ids = Seq(
    "FOS_family", "GST", "p38", "PDGF",
    "PDGFR", "RAF", "STAT5", "TSC" )

  // TODO: some of these are Family, some are GGP. Re-enable once Family is folded into GGP
  /*
  // A few sample previous Override Family entries, replaced by BE family KB.
  val be5f = """ACAD, ACSL, ACTN, ADCY, ADH,
                ADRA, ADRA2, ADRB, ADRBK, ALDH,
                ALDO, APOA, ARRB, ATP5G,
                BIRC, CAPN, Carboxylesterase, CD16, CD64,
                MAPK, MEK, MEK1/2, MEK 1/2, ERK,
                CSNK1, CSNK2, CTNNA, CUL, Cyclophilin are sample families."""
  val be5f_ids = Seq(
    "ACAD", "ACSL", "ACTN", "ADCY", "ADH",
    "ADRA", "ADRA2", "ADRB", "ADRBK", "ALDH",
    "ALDO", "APOA", "ARRB", "ATP5G",
    "BIRC", "CAPN", "Carboxylesterase", "CD16", "CD64",
    "ERK", "MEK", "MEK", "MEK", "ERK",
    "CSNK1", "CSNK2", "CTNNA", "CUL", "Cyclophilin"
  )
  */

  val chem = "GTP, GDP, cyclododecane, TAK-165, and estrone are important molecules. "
  val chem_ids = Seq("6830", "8977", "18529", "644692", "5870")

  val estros = "Estrone E1, estradiol E2, and estriol E3 do not cause cancer."

  val aminos = "Alanine, arginine, asparagine, aspartic acid, aspartate, cysteine, glutamic acid, glutamate, glutamine, glycine, histidine, isoleucine, leucine, lysine, methionine, phenylalanine, proline, serine, threonine, tryptophan, tyrosine, and valine are amino acids"

  val aa_short ="Ala, Arg, Asn, Asp, Cys, Gln, Glu, Gly, His, Ile, Leu, Lys, Met, Phe, Pro, Ser, Thr, Trp, Tyr, and Val are now labeled as Sites."

  val aa_ids = Seq(
    "UAZ-S-001", "UAZ-S-002", "UAZ-S-003", "UAZ-S-004", "UAZ-S-004",
    "UAZ-S-005", "UAZ-S-006", "UAZ-S-006", "UAZ-S-007", "UAZ-S-008",
    "UAZ-S-009", "UAZ-S-010", "UAZ-S-011", "UAZ-S-012", "UAZ-S-013",
    "UAZ-S-014", "UAZ-S-015", "UAZ-S-016", "UAZ-S-017", "UAZ-S-018",
    "UAZ-S-019", "UAZ-S-020"
  )

  /** Override test driver method. */
  def testMentions (
    text: String,
    ids: Seq[String],
    label: String,
    displayLabel: Option[String] = None,
    groundedHuman: Boolean = false,
    debug: Boolean = false
  ): Unit = {
    val mentions = getBioMentions(text)

    if (debug)                              // allow debugging on group-by-group basis
      printMentions(Try(mentions), debug)

    text should "have expected number of results" in {
      mentions should not be (empty)
      mentions should have size (ids.size)
    }

    it should s"have labeled all mentions as $label in the text $text" in {
      mentions.count(_ matches label) should be (ids.size)
    }

    if (displayLabel.isDefined) {
      it should s"have display labeled all mentions as $displayLabel" in {
        mentions.count(_.displayLabel == displayLabel.get) should be (ids.size)
      }
    }

    if (groundedHuman) {
      it should s"have grounded all mentions as Human in the text $text" in {
        mentions.forall(m => m.grounding.isDefined &&
          Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
      }
    }

    it should s"match expected grounding IDs in the text $text" in {
      for ((m, ndx) <- mentions.zipWithIndex) {
        m.grounding.isDefined && (m.grounding.get.id == ids(ndx)) should be (true)
      }
    }
  }

  // Run the actual tests:
  testMentions(ggp1,     ggp1_ids, GGP,      Some(Protein), true)
  testMentions(ggp2,     ggp2_ids, GGP,      Some(Protein), true)
  testMentions(ggp3,     ggp3_ids, GGP,      Some(Protein), true)
  testMentions(ggp4,     ggp4_ids, GGP,      Some(Protein), true)
  testMentions(ggp5,     ggp5_ids, GGP,      Some(Protein), false)
  testMentions(ggp6,     ggp6_ids, GGP,      Some(Protein), true)
  testMentions(ggp7,     ggp7_ids, GGP,      Some(Protein), true)
  testMentions(fam1,     fam1_ids, Family,   Some(Family),  false)
  testMentions(fam2,     fam2_ids, Family,   Some(Family),  false)
  testMentions(be1f,     be1f_ids, Family,   Some(Family),  false)
  testMentions(be2c,     be2c_ids, Family,   Some(Family), false)
  testMentions(be2f,     be2f_ids, Family,   Some(Family),  false)
  testMentions(be3c,     be3c_ids, Family,   Some(Family), false)
  testMentions(be3f,     be3f_ids, Family,   Some(Family),  false)
  testMentions(be4f,     be4f_ids, Family,   Some(Family),  false)
  //testMentions(be5f,     be5f_ids, Family,   Some(Family),  false)
  testMentions(chem,     chem_ids, Chemical, Some(Chemical), true)
  testMentions(aminos,   aa_ids,   Site,     Some(Site),    false)

  // special test for Estrogens and their nicknames:
  estros should "have Simple_chemical labels" in {
    val mentions = getBioMentions(estros)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 6
    mentions.count(_ matches "Simple_chemical") should be (6)
  }

  // Amino Acid abbreviations relabeled as Sites (but only 20/22 because of protein conflicts)
  // Should all be GROUNDED correctly as UAZ-S-* via the NER override file.
  aa_short should "have Site labels" in {
    val aas_mentions =  getBioMentions(aa_short)
    aas_mentions should not be (empty)
    // printMentions(Try(aas_mentions), true)      // DEBUGGING
    aas_mentions should have size (20)
    aas_mentions.count(_ matches "Site") should be (20)
  }

}
