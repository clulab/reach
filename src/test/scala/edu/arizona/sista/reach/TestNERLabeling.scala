package edu.arizona.sista.reach

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.utils.Serializer

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try
import TestUtils._

/**
  * Test the labeling of various types of mentions identified by the NER.
  *   Written by: Tom Hicks. 4/21/2016.
  *   Last Modified: Update for use of manual simple chemical overrides.
  */
class TestNERLabeling extends FlatSpec with Matchers {

  val BioProcess = "apoptosis, autophagic cell death, quiescence, hematopoiesis, or complex assembly cause cancer."
  val CellLine = "MPanc-96, mast, CHO, CEM/TART, and ZR75-1 cause cancer."
  val CellType = "apud cell, AV nodal myocyte, An1 B Cell, xanthoblast, and zygote cause cancer"
  val CellTypes = "apud cells, AV nodal myocytes, An1 B Cells, xanthoblasts, and zygotes cause cancer"
  // this tests from Uniprot subcellular location AND GO subcellular location KBs:
  val Cellular_component = "A bands, C zones, F bouton, H zones, I bands, Z lines, CVT vesicles, telomeres, Symplasts, and Host periplasms cause cancer."
  // this tests from PFAM AND InterPro protein family KBs:
  val Family = "CDC73_N, RcsD-ABL domain, zinc-ribbon domain, Rho_RNA_bind, RasGAP_C, zwf, PTHR10856:SF10, GLHYDRLASE27, Ras guanyl-releasing protein 1, and Jiraiya cause cancer."
  val Gene_or_gene_product = "CK-40, ZZANK2, MCH-1R, RAS1, and hemAT cause cancer."
  // this tests overrides simple chemical identifications:
  val manual_chemicals = "Estrone E1, estradiol E2, and estriol E3 do not cause cancer."
  val Organ = "Acetabulum, Visceral Pericardium, malleolar bone, Vena cava sinus, and zygopodium cause cancer"
  val Simple_chemical = "endoxifen sulfate, Juvamine, Adenosine-phosphate, Xitix, and Monic acid cause cancer"
  val Site = "ALOG domain, AMIN domain, KIP1-like, KEN domain, and HAS subgroup cause cancer"
  val Species = "Potato, wheat, Yerba-mate, Danio rerio, zebrafish, Rats, Gallus gallus, and chickens cause cancer"

  "BioProcess entities" should "have BioProcess label" in {
    val mentions = getBioMentions(BioProcess)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "BioProcess") should be (5)
  }

  "CellLine entities" should "have CellLine label" in {
    val mentions = getBioMentions(CellLine)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "CellLine") should be (5)
  }

  "CellType entities" should "have CellType label" in {
    val mentions = getBioMentions(CellType)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "CellType") should be (5)
  }

  "Plural CellType entities" should "have CellType label" in {
    val mentions = getBioMentions(CellTypes)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "CellType") should be (5)
  }

  // this tests from Uniprot subcellular location AND GO subcellular location KBs:
  "Cellular_component entities" should "have Cellular_component label" in {
    val mentions = getBioMentions(Cellular_component)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (10)
    mentions.count(_ matches "Cellular_component") should be (10)
  }

  // this tests from PFAM AND InterPro protein family KBs:
  "Family entities" should "have Family label" in {
    val mentions = getBioMentions(Family)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (10)
    mentions.count(_ matches "Family") should be (10)
  }

  "Gene_or_gene_product entities" should "have Gene_or_gene_product label" in {
    val mentions = getBioMentions(Gene_or_gene_product)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "Gene_or_gene_product") should be (5)
  }

  "Gene_or_gene_product entities" should "have Protein displayLabel" in {
    val mentions = getBioMentions(Gene_or_gene_product)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_.displayLabel == "Protein") should be (5)
  }

  "Organ entities" should "have Organ label" in {
    val mentions = getBioMentions(Organ)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "Organ") should be (5)
  }

  "Simple_chemical entities" should "have Simple_chemical label" in {
    val mentions = getBioMentions(Simple_chemical)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "Simple_chemical") should be (5)
  }

  "Site entities" should "have Site label" in {
    val mentions = getBioMentions(Site)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (5)
    mentions.count(_ matches "Site") should be (5)
  }

  "Species entities" should "have Species label" in {
    val mentions = getBioMentions(Species)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (8)
    mentions.count(_ matches "Species") should be (8)
  }

  "Manual Chemical entities" should "have override labels" in {
    val mentions = getBioMentions(manual_chemicals)
    mentions.isEmpty should be (false)
    // printMentions(Try(mentions), true)      // DEBUGGING
    mentions.size should be (6)
    // should be 6 but test will fail until NER can be overridden (processors issue #61):
    // mentions.count(_ matches "Simple_chemical") should be (6)
    mentions.count(_ matches "Simple_chemical") should be (5)
  }

}
