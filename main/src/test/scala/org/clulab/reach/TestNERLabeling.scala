package org.clulab.reach

import org.clulab.reach.grounding._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._


/**
  * Test the labeling of various types of mentions identified by the NER.
  *   Written by: Tom Hicks. 4/21/2016.
  *   Last Modified: Update for use of BE KBs.
  */
class TestNERLabeling extends FlatSpec with Matchers {

  val bioProcess = "apoptosis, autophagic cell death, quiescence, hematopoiesis, or complex assembly cause cancer."
  val cellLine = "MPanc-96, mast, Hyssop, CEM/TART, and ZR75-1 cause cancer."
  val cellType = "apud cell, AV nodal myocyte, An1 B Cell, xanthoblast, and zygote cause cancer"
  val cellTypes = "apud cells, AV nodal myocytes, An1 B Cells, xanthoblasts, and zygotes cause cancer"
  // this tests from Uniprot subcellular location AND GO subcellular location KBs:
  val cellular_comp = "A bands, C zones, F bouton, H zones, I bands, Z lines, CVT vesicles, telomeres, Symplasts, and Host periplasms cause cancer."
  // this tests from PFAM AND InterPro protein family KBs:
  val families = "CDC73_N, RcsD-ABL domain, zinc-ribbon domain, Rho_RNA_bind, RasGAP_C, zwf, PTHR10856:SF10, GLHYDRLASE27, Ras guanyl-releasing protein 1, and Jiraiya cause cancer."
  val ggp = "CK-40, ZZANK2, MCH-1R, RAS1, and hemAT cause cancer."
  // this tests overrides simple chemical identifications:
  val manual_chemicals = "Estrone E1, estradiol E2, and estriol E3 do not cause cancer."
  val organ = "Acetabulum, Visceral Pericardium, malleolar bone, Vena cava sinus, and zygopodium cause cancer"
  val chemical = "endoxifen sulfate, Juvamine, Adenosine-phosphate, Xitix, and Monic acid cause cancer"
  val sites = "ALOG domain, AMIN domain, KIP1-like, KEN domain, and HAS subgroup cause cancer"
  val species = "Potato, wheat, Yerba-mate, Danio rerio, zebrafish, Rats, Gallus gallus, and chickens cause cancer"

  val drug = "Alvocidib, Anacardic acid, L-779450, Masitinib, and  Withaferin A are known drugs. "
  val drug_ids = Seq("CHEBI:47344", "167551", "9950176", "CHEBI:63450", "CHEBI:10040")


  bioProcess should "have BioProcess label" in {
    val mentions = getBioMentions(bioProcess)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 5
    mentions.count(_ matches "BioProcess") should be (5)
  }

  cellLine should "have CellLine label" in {
    val mentions = getBioMentions(cellLine)
    mentions should not be (empty)
    //printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 5
    mentions.count(_ matches "CellLine") should be (4) // "mast" is a protein
  }

  cellType should "have CellType label" in {
    val mentions = getBioMentions(cellType)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 5
    mentions.count(_ matches "CellType") should be (5)
  }

  cellTypes should "have CellType label" in {
    val mentions = getBioMentions(cellTypes)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 5
    mentions.count(_ matches "CellType") should be (5)
  }

  // this tests from Uniprot subcellular location AND GO subcellular location KBs:
  cellular_comp should "have Cellular_component label" in {
    val mentions = getBioMentions(cellular_comp)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 10
    mentions.count(_ matches "Cellular_component") should be (10)
  }

  // this tests from PFAM AND InterPro protein family KBs:
  families should "have Family label" in {
    val mentions = getBioMentions(families)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 10

    // "Ras guanyl-releasing protein 1" is both a protein and a protein family
    // this test is robust to this: it allows either label for this entity
    (mentions.count(_ matches "Family") >= 9) should be (true)
  }

  ggp should "have Gene_or_gene_product label" in {
    val mentions = getBioMentions(ggp)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 5
    mentions.count(_ matches "Gene_or_gene_product") should be (5)
  }

  ggp should "have Protein displayLabel" in {
    val mentions = getBioMentions(ggp)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 5
    mentions.count(_.displayLabel == "Protein") should be (5)
  }

  organ should "have Organ label" in {
    val mentions = getBioMentions(organ)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 5
    mentions.count(_ matches "Organ") should be (5)
  }

  chemical should "have Simple_chemical label" in {
    val mentions = getBioMentions(chemical)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 5
    mentions.count(_ matches "Simple_chemical") should be (5)
  }

  sites should "have Site label" in {
    val mentions = getBioMentions(sites)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 5
    mentions.count(_ matches "Site") should be (5)
  }

  species should "have Species label" in {
    val mentions = getBioMentions(species)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 8
    mentions.count(_ matches "Species") should be (8)
  }

  manual_chemicals should "have override labels" in {
    val mentions = getBioMentions(manual_chemicals)
    mentions should not be (empty)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions should have size 6
    mentions.count(_ matches "Simple_chemical") should be (6)
  }


  // Tests from Reach issue #274
  val mentions274a = getBioMentions("Smad 2 is doing something.")
  "Smad 2 is doing something" should "have expected number of results" in {
    mentions274a should not be (empty)
    // printMentions(Try(mentions274a), verbose = true)      // DEBUGGING
    mentions274a should have size 1
  }

  it should "have labeled all mentions as GGP" in {
    mentions274a.count(_ matches "Gene_or_gene_product") should be (1)
  }

  it should "have Protein displayLabel" in {
    mentions274a.count(_.displayLabel == "Protein") should be (1)
  }

  it should "have grounded all mentions as Human" in {
    mentions274a.forall(m => m.grounding.isDefined &&
                        Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  // The following test fails (Reach issue #274).
  // So far this appears to be because the main grounding routine in ReachEntityLookup
  // is being called with 'smad' as text (not 'smad 2') even though label is correctly GGP.
  it should "match expected grounding IDs" in {
    val m = mentions274a(0)
    (m.grounding.isDefined && (m.grounding.get.id == "Q15796")) should be (true)
  }


  val mentions274f = getBioMentions("Smad is doing something.")
  "Smad is doing something" should "have expected number of results" in {
    mentions274f should not be (empty)
    // printMentions(Try(mentions274f), true)      // DEBUGGING
    mentions274f should have size 1
  }

  it should "have labeled all mentions as Family" in {
    mentions274f.count(_ matches "Family") should be (1)
  }

  it should "have Protein displayLabel" in {
    mentions274f.count(_.displayLabel == "Family") should be (1)
  }

  it should "match expected grounding IDs" in {
    val m = mentions274f(0)
    (m.grounding.isDefined && (m.grounding.get.id == "SMAD")) should be (true)
  }


  // Drug KB Simple_chemical Tests
  val drug_mentions = getBioMentions(drug)
  drug should "have expected number of results" in {
    drug_mentions should not be (empty)
    // printMentions(Try(drug_mentions), true)      // DEBUGGING
    drug_mentions should have size (drug_ids.size)
  }

  it should "have labeled all mentions as Simple_chemical" in {
    drug_mentions.count(_ matches "Simple_chemical") should be (drug_ids.size)
  }

  it should "have display labeled all mentions as Simple_chemical" in {
    drug_mentions.count(_.displayLabel == "Simple_chemical") should be (drug_ids.size)
  }

  it should "match expected grounding IDs" in {
    for ((m, ndx) <- drug_mentions.zipWithIndex) {
      m.grounding.isDefined && (m.grounding.get.id == drug_ids(ndx)) should be (true)
    }
  }

}
