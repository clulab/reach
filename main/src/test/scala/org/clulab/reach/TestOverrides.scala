package org.clulab.reach

import org.clulab.reach.grounding._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._


/**
  * Test that our override KB works properly for NER and grounding.
  *   Written by: Tom Hicks. 7/8/2016.
  *   Last Modified: Add override test for entries kept despite clash with BE KBs.
  */
class TestOverrides extends FlatSpec with Matchers {
  val Chemical = "Simple_chemical"
  val Family = "Family"
  val GGP = "Gene_or_gene_product"
  val Protein = "Protein"
  val Site = "Site"

  val ggp1 = "Ack1, AKT1, AKT2, ASPP1, ASPP2 are GGPs."
  val ggp1_ids = Seq("Q07912", "P31749", "P31751", "Q96KQ4", "Q13625")

  val ggp2 = "Cdc42, EGF, EGFR, ErbB, ERK5, GSK3beta are GGPs."
  val ggp2_ids = Seq("P60953", "P01133", "P00533", "P00533", "Q13164", "P49841")

  val ggp3 = "HRAS, IGF-1, JNK1, GSK3alpha, KRAS, MAP2K1 are GGPs."
  val ggp3_ids = Seq("P01112", "P05019", "P45983", "P49840", "P01116", "Q02750")

  val ggp4 = "MAP2K2, MAZ, MEK1, MEK2, MEK3, MEK4 are GGPs."
  val ggp4_ids = Seq("P36507", "P56270", "Q02750", "P36507", "P46734", "P45985")

  val ggp5 = "MEK5, MEK6, MEK7, NRAS, and PI3K are GGPs."
  val ggp5_ids = Seq("Q13163", "P52564", "O14733", "P01111", "PI3K")

  val ggp6 = "p53, RAC1, RhoA, ROCK1, SAF-1, VEGF are GGPs. "
  val ggp6_ids = Seq("P04637", "P63000", "P61586", "Q13464", "P56270", "P15692")

  val ggp7 = "HRAS, H-RAS, KRAS, K-RAS, NRAS, N-RAS are GGPs. "
  val ggp7_ids = Seq("P01112", "P01112", "P01116", "P01116", "P01111", "P01111")

  val fam1 = "ERK1/2, ERK 1/2, Neuregulin, Neuroregulin are Families. "
  val fam1_ids = Seq("ERK", "ERK", "PF02158", "PF02158")

  val fam2 = "SMAD, SMAD2/3, SMAD 2/3, and TGFB are important Families. "
  val fam2_ids = Seq("SMAD", "SMAD", "SMAD", "IPR015615")

  val fam3 = "AKT and Cadherin are important Families. "
  val fam3_ids = Seq("AKT", "PF00028")

  // Override entries which were kept despite clash with BE KBs:
  val fam4 = """ACOX, BMP, Cadherin, CRISP, DDR,
                COX4, COX6a, COX6b, COX7a, COX7b,
                COX8, DVL, ETS, FGF, FLOT,
                GATA, HSP90, IGFBP, IL1, IRS,
                MAF, NOTCH, PKI, RAS, SAA,
                and TGFB are unchanged Families."""
  val fam4_ids = Seq(
    "PF01756", "PF02608", "PF00028", "PF08562", "PF08841",
    "PF02936", "PF02046", "PF02297", "PF02238", "PF05392",
    "PF02285", "PF08137", "PF00178", "PF00167", "PF15975",
    "PF00320", "PF00183", "PF00219", "PF00340", "PF02174",
    "PF02545", "PF00066", "PF02827", "PF00071", "PF00277",
    "IPR015615")

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

    it should s"have labeled all mentions as ${label}" in {
      mentions.count(_ matches label) should be (ids.size)
    }

    if (displayLabel.isDefined) {
      it should s"have display labeled all mentions as ${displayLabel}" in {
        mentions.count(_.displayLabel == displayLabel.get) should be (ids.size)
      }
    }

    if (groundedHuman) {
      it should "have grounded all mentions as Human" in {
        mentions.forall(m => m.grounding.isDefined &&
          Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
      }
    }

    it should "match expected grounding IDs" in {
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
  testMentions(fam1,     fam1_ids, Family,   Some(Family), false)
  testMentions(fam2,     fam2_ids, Family,   Some(Family), false)
  testMentions(fam3,     fam3_ids, Family,   Some(Family), false)
  testMentions(fam4,     fam4_ids, Family,   Some(Family), false)
  testMentions(chem,     chem_ids, Chemical, Some(Chemical), true)
  testMentions(aminos,   aa_ids,   Site,     Some(Site), false)

  // special test for Estrogens and their nicknames:
  estros should "have Simple_chemical labels" in {
    val mentions = getBioMentions(estros)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 6
    mentions.count(_ matches "Simple_chemical") should be (6)
  }

  // Amino Acid abbreviations relabeled as Sites (but only 20/22 because of conflicts):
  aa_short should "have Site labels" in {
    val aas_mentions =  getBioMentions(aa_short)
    aas_mentions should not be (empty)
    // printMentions(Try(aas_mentions), true)      // DEBUGGING
    aas_mentions should have size (20)
    aas_mentions.count(_ matches "Site") should be (20)
  }

}
