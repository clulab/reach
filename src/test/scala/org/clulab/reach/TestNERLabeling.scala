package org.clulab.reach

import org.clulab.odin._
import org.clulab.reach.mentions._
import org.clulab.utils.Serializer

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try
import TestUtils._

/**
  * Test the labeling of various types of mentions identified by the NER.
  *   Written by: Tom Hicks. 4/21/2016.
  *   Last Modified: Update tests for override KB.
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

  bioProcess should "have BioProcess label" in {
    val mentions = getBioMentions(bioProcess)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "BioProcess") should be (5)
  }

  cellLine should "have CellLine label" in {
    val mentions = getBioMentions(cellLine)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "CellLine") should be (5)
  }

  cellType should "have CellType label" in {
    val mentions = getBioMentions(cellType)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "CellType") should be (5)
  }

  cellTypes should "have CellType label" in {
    val mentions = getBioMentions(cellTypes)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "CellType") should be (5)
  }

  // this tests from Uniprot subcellular location AND GO subcellular location KBs:
  cellular_comp should "have Cellular_component label" in {
    val mentions = getBioMentions(cellular_comp)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (10)
    mentions.count(_ matches "Cellular_component") should be (10)
  }

  // this tests from PFAM AND InterPro protein family KBs:
  families should "have Family label" in {
    val mentions = getBioMentions(families)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (10)
    mentions.count(_ matches "Family") should be (10)
  }

  ggp should "have Gene_or_gene_product label" in {
    val mentions = getBioMentions(ggp)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "Gene_or_gene_product") should be (5)
  }

  ggp should "have Protein displayLabel" in {
    val mentions = getBioMentions(ggp)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_.displayLabel == "Protein") should be (5)
  }

  organ should "have Organ label" in {
    val mentions = getBioMentions(organ)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "Organ") should be (5)
  }

  chemical should "have Simple_chemical label" in {
    val mentions = getBioMentions(chemical)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "Simple_chemical") should be (5)
  }

  sites should "have Site label" in {
    val mentions = getBioMentions(sites)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "Site") should be (5)
  }

  species should "have Species label" in {
    val mentions = getBioMentions(species)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (8)
    mentions.count(_ matches "Species") should be (8)
  }

  manual_chemicals should "have override labels" in {
    val mentions = getBioMentions(manual_chemicals)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (6)
    mentions.count(_ matches "Simple_chemical") should be (6)
  }

}
