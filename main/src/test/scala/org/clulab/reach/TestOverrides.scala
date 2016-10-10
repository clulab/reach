package org.clulab.reach

import org.clulab.reach.grounding._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._


/**
  * Test that our override KB works properly for NER and grounding.
  *   Written by: Tom Hicks. 7/8/2016.
  *   Last Modified: Add tests for overrides as a result of Hans error analysis.
  */
class TestOverrides extends FlatSpec with Matchers {

  val dr2a = "Ack1, AKT, AKT1, AKT2, ASPP1, ASPP2 are GGPs."
  val dr2a_ids = Seq("Q07912", "P31749", "P31749", "P31751", "Q96KQ4", "Q13625")

  val dr2b = "Cdc42, EGF, EGFR, ErbB, ERK5, GSK3beta are GGPs."
  val dr2b_ids = Seq("P60953", "P01133", "P00533", "P00533", "Q13164", "P49841")

  val dr2c = "HRAS, IGF-1, JNK1, GSK3alpha, KRAS, MAP2K1 are GGPs."
  val dr2c_ids = Seq("P01112", "P05019", "P45983", "P49840", "P01116", "Q02750")

  val dr2d = "MAP2K2, MAZ, MEK1, MEK2, MEK3, MEK4 are GGPs."
  val dr2d_ids = Seq("P36507", "P56270", "Q02750", "P36507", "P46734", "P45985")

  val dr2e = "MEK5, MEK6, MEK7, NRAS, PI3K, p38 are GGPs."
  val dr2e_ids = Seq("Q13163", "P52564", "O14733", "P01111", "P42336", "Q16539")

  val dr2f = "p53, RAC1, RhoA, ROCK1, SAF-1, VEGF are GGPs. "
  val dr2f_ids = Seq("P04637", "P63000", "P61586", "Q13464", "P56270", "P15692")

  val dr2g = "HRAS, H-RAS, KRAS, K-RAS, NRAS, N-RAS are GGPs. "
  val dr2g_ids = Seq("P01112", "P01112", "P01116", "P01116", "P01111", "P01111")

  val fam1 = "ERK1/2, ERK 1/2, Neuregulin, Neuroregulin are Families. "
  val fam1_ids = Seq("UAZ-PF-244", "UAZ-PF-244", "PF02158", "PF02158")

  val fam2 = "SMAD2/3, SMAD 2/3, TGFB and Cadherin are important Families. "
  val fam2_ids = Seq("UAZ-PF-243", "UAZ-PF-243", "IPR015615", "PF00028")

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

  // Dry Run 2 - group A
  val dr2a_mentions = getBioMentions(dr2a)
  dr2a should "have expected number of results" in {
    dr2a_mentions should not be (empty)
    // printMentions(Try(dr2a_mentions), true)      // DEBUGGING
    dr2a_mentions should have size (dr2a_ids.size)
  }

  it should "have labeled all mentions as GGP" in {
    dr2a_mentions.count(_ matches "Gene_or_gene_product") should be (dr2a_ids.size)
  }

  it should "have display labeled all mentions as Proteins" in {
    dr2a_mentions.count(_.displayLabel == "Protein") should be (dr2a_ids.size)
  }

  it should "have grounded all mentions as Human" in {
    dr2a_mentions.forall(m => m.grounding.isDefined &&
                         Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- dr2a_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == dr2a_ids(ndx)) should be (true)
    }
  }


  // Dry Run 2 - group B
  val dr2b_mentions = getBioMentions(dr2b)
  dr2b should "have expected number of results" in {
    dr2b_mentions should not be (empty)
    // printMentions(Try(dr2b_mentions), true)      // DEBUGGING
    dr2b_mentions should have size (dr2b_ids.size)
  }

  it should "have labeled all mentions as GGP" in {
    dr2b_mentions.count(_ matches "Gene_or_gene_product") should be (dr2b_ids.size)
  }

  it should "have display labeled all mentions as Proteins" in {
    dr2b_mentions.count(_.displayLabel == "Protein") should be (dr2b_ids.size)
  }

  it should "have grounded all mentions as Human" in {
    dr2b_mentions.forall(m => m.grounding.isDefined &&
                         Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- dr2b_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == dr2b_ids(ndx)) should be (true)
    }
  }


  // Dry Run 2 - group C
  val dr2c_mentions = getBioMentions(dr2c)
  dr2c should "have expected number of results" in {
    dr2c_mentions should not be (empty)
    // printMentions(Try(dr2c_mentions), true)      // DEBUGGING
    dr2c_mentions should have size (dr2c_ids.size)
  }

  it should "have labeled all mentions as GGP" in {
    dr2c_mentions.count(_ matches "Gene_or_gene_product") should be (dr2c_ids.size)
  }

  it should "have display labeled all mentions as Proteins" in {
    dr2c_mentions.count(_.displayLabel == "Protein") should be (dr2c_ids.size)
  }

  it should "have grounded all mentions as Human" in {
    dr2c_mentions.forall(m => m.grounding.isDefined &&
                         Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- dr2c_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == dr2c_ids(ndx)) should be (true)
    }
  }


  // Dry Run 2 - group D
  val dr2d_mentions = getBioMentions(dr2d)
  dr2d should "have expected number of results" in {
    dr2d_mentions should not be (empty)
    // printMentions(Try(dr2d_mentions), true)      // DEBUGGING
    dr2d_mentions should have size (dr2d_ids.size)
  }

  it should "have labeled all mentions as GGP" in {
    dr2d_mentions.count(_ matches "Gene_or_gene_product") should be (dr2d_ids.size)
  }

  it should "have display labeled all mentions as Proteins" in {
    dr2d_mentions.count(_.displayLabel == "Protein") should be (dr2d_ids.size)
  }

  it should "have grounded all mentions as Human" in {
    dr2d_mentions.forall(m => m.grounding.isDefined &&
                         Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- dr2d_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == dr2d_ids(ndx)) should be (true)
    }
  }


  // Dry Run 2 - group E
  val dr2e_mentions = getBioMentions(dr2e)
  dr2e should "have expected number of results" in {
    dr2e_mentions should not be (empty)
    // printMentions(Try(dr2e_mentions), true)      // DEBUGGING
    dr2e_mentions should have size (dr2e_ids.size)
  }

  it should "have labeled all mentions as GGP" in {
    dr2e_mentions.count(_ matches "Gene_or_gene_product") should be (dr2e_ids.size)
  }

  it should "have display labeled all mentions as Proteins" in {
    dr2e_mentions.count(_.displayLabel == "Protein") should be (dr2e_ids.size)
  }

  it should "have grounded all mentions as Human" in {
    dr2e_mentions.forall(m => m.grounding.isDefined &&
                         Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- dr2e_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == dr2e_ids(ndx)) should be (true)
    }
  }


  // Dry Run 2 - group F
  val dr2f_mentions = getBioMentions(dr2f)
  dr2f should "have expected number of results" in {
    dr2f_mentions should not be (empty)
    // printMentions(Try(dr2f_mentions), true)      // DEBUGGING
    dr2f_mentions should have size (dr2f_ids.size)
  }

  it should "have labeled all mentions as GGP" in {
    dr2f_mentions.count(_ matches "Gene_or_gene_product") should be (dr2f_ids.size)
  }

  it should "have display labeled all mentions as Proteins" in {
    dr2f_mentions.count(_.displayLabel == "Protein") should be (dr2f_ids.size)
  }

  it should "have grounded all mentions as Human" in {
    dr2f_mentions.forall(m => m.grounding.isDefined &&
                         Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- dr2f_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == dr2f_ids(ndx)) should be (true)
    }
  }


  // Dry Run 2 - group G
  val dr2g_mentions = getBioMentions(dr2g)
  dr2g should "have expected number of results" in {
    dr2g_mentions should not be (empty)
    // printMentions(Try(dr2g_mentions), true)      // DEBUGGING
    dr2g_mentions should have size (dr2f_ids.size)
  }

  it should "have labeled all mentions as GGP" in {
    dr2g_mentions.count(_ matches "Gene_or_gene_product") should be (dr2g_ids.size)
  }

  it should "have display labeled all mentions as Proteins" in {
    dr2g_mentions.count(_.displayLabel == "Protein") should be (dr2g_ids.size)
  }

  it should "have grounded all mentions as Human" in {
    dr2g_mentions.forall(m => m.grounding.isDefined &&
                         Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- dr2g_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == dr2g_ids(ndx)) should be (true)
    }
  }


  // Family Tests 1
  val fam1_mentions = getBioMentions(fam1)
  fam1 should "have expected number of results" in {
    fam1_mentions should not be (empty)
    // printMentions(Try(fam1_mentions), true)      // DEBUGGING
    fam1_mentions should have size (fam1_ids.size)
  }

  it should "have labeled all mentions as Family" in {
    fam1_mentions.count(_ matches "Family") should be (fam1_ids.size)
  }

  it should "have display labeled all mentions as Family" in {
    fam1_mentions.count(_.displayLabel == "Family") should be (fam1_ids.size)
  }

  it should "have grounded all mentions as Human" in {
    fam1_mentions.forall(m => m.grounding.isDefined &&
                         Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- fam1_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == fam1_ids(ndx)) should be (true)
    }
  }


  // Family Tests 2
  val fam2_mentions = getBioMentions(fam2)
  fam2 should "have expected number of results" in {
    fam2_mentions should not be (empty)
    // printMentions(Try(fam2_mentions), true)      // DEBUGGING
    fam2_mentions should have size (fam2_ids.size)
  }

  it should "have labeled all mentions as Family" in {
    fam2_mentions.count(_ matches "Family") should be (fam2_ids.size)
  }

  it should "have display labeled all mentions as Family" in {
    fam2_mentions.count(_.displayLabel == "Family") should be (fam2_ids.size)
  }

  it should "have grounded all mentions as Human" in {
    fam2_mentions.forall(m => m.grounding.isDefined &&
                         Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- fam2_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == fam2_ids(ndx)) should be (true)
    }
  }


  // Simple_chemical Tests
  val chem_mentions = getBioMentions(chem)
  chem should "have expected number of results" in {
    chem_mentions should not be (empty)
    // printMentions(Try(chem_mentions), true)      // DEBUGGING
    chem_mentions should have size (chem_ids.size)
  }

  it should "have labeled all mentions as Simple_chemical" in {
    chem_mentions.count(_ matches "Simple_chemical") should be (chem_ids.size)
  }

  it should "have display labeled all mentions as Simple_chemical" in {
    chem_mentions.count(_.displayLabel == "Simple_chemical") should be (chem_ids.size)
  }

  it should "have grounded all mentions as Human" in {
    chem_mentions.forall(m => m.grounding.isDefined &&
                         Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- chem_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == chem_ids(ndx)) should be (true)
    }
  }


  estros should "have Simple_chemical labels" in {
    val mentions = getBioMentions(estros)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 6
    mentions.count(_ matches "Simple_chemical") should be (6)
  }



  // Amino Acids relabeled as Sites:
  val aa_mentions = getBioMentions(aminos)
  aminos should "have expected number of results" in {
    aa_mentions should not be (empty)
    // printMentions(Try(aa_mentions), true)      // DEBUGGING
    aa_mentions should have size (aa_ids.size)
  }

  it should "have labeled all mentions as Site" in {
    aa_mentions.count(_ matches "Site") should be (aa_ids.size)
  }

  it should "have display labeled all mentions as Proteins" in {
    aa_mentions.count(_.displayLabel == "Site") should be (aa_ids.size)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- aa_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == aa_ids(ndx)) should be (true)
    }
  }


  // Amino Acid abbreviations relabeled as Sites:
  val aas_mentions =  getBioMentions(aa_short)

  aa_short should "have Site labels" in {
    aas_mentions should not be (empty)
    // printMentions(Try(aas_mentions), true)      // DEBUGGING
    aas_mentions should have size (20)
    aas_mentions.count(_ matches "Site") should be (20)
  }

}
