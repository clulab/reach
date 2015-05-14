package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest._
import TestResources._
import DarpaEvalUtils._

/**
 * Unit tests to ensure PTM rules are matching correctly
 */
class TestPTMrules extends FlatSpec with Matchers {
  val docId = "testdoc"
  val chunkId = "1"

  // Ubiquitinated
  val ubiqJJ = "The ubiquitinated Ras binds AKT and ASPP2."
  val ubiqJJWithSite = "The ubiquitinated Ras at **SITEHERE** binds AKT and ASPP2."
  val ubiqJJIntercedingWIthSite = "The ubiquitinated Ras protein at **SITEHERE** binds AKT and ASPP2."
  val ubiqVBN = ""
  val ubiqVBNWithSite = ""
  val ubiqVBNIntercedingWIthSite = ""
  // Phosphorylated
  val phosJJ = ""
  val phosJJWithSite = ""
  val phosJJIntercedingWIthSite = ""
  val phosVBN = ""
  val phosVBNWithSite = ""
  val phosVBNIntercedingWIthSite = ""
  // Farnesylated
  val farneJJ = ""
  val farneJJWithSite = ""
  val farneJJIntercedingWIthSite = ""
  val farneVBN = ""
  val farneVBNWithSite = ""
  val farneVBNIntercedingWIthSite = ""
  // Ribosylated
  val riboJJ = ""
  val riboJJWithSite = ""
  val riboJJIntercedingWIthSite = ""
  val riboVBN = ""
  val riboVBNWithSite = ""
  val riboVBNIntercedingWIthSite = ""
  // Hydroxylated
  val hydroxJJ = ""
  val hydroxJJWithSite = ""
  val hydroxJJIntercedingWIthSite = ""
  val hydroxVBN = ""
  val hydroxVBNWithSite = ""
  val hydroxVBNIntercedingWIthSite = ""
  // Acetylated
  val aceJJ = ""
  val aceJJWithSite = ""
  val aceJJIntercedingWIthSite = ""
  val aceVBN = ""
  val aceVBNWithSite = ""
  val aceVBNIntercedingWIthSite = ""
  // Glycosylated
  val glycoJJ = ""
  val glycoJJWithSite = ""
  val glycoJJIntercedingWIthSite = ""
  val glycoVBN = ""
  val glycoVBNWithSite = ""
  val glycoVBNIntercedingWIthSite = ""
  // Methylated
  val methJJ = ""
  val methJJWithSite = ""
  val methJJIntercedingWIthSite = ""
  val methVBN = ""
  val methVBNWithSite = ""
  val methVBNIntercedingWIthSite = ""
  // Sumosylated
  val sumoJJ = ""
  val sumoJJWithSite = ""
  val sumoJJIntercedingWIthSite = ""
  val sumoVBN = ""
  val sumoVBNWithSite = ""
  val sumoVBNIntercedingWIthSite = ""

  ubiqJJ should "contain a ubiq PTM" in {
    val doc = reach.mkDoc(ubiqJJ, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ubiquitinated" should be (true)
  }
  ubiqVBN should "contain a ubiq PTM" in {
    // Test here
  }
  ubiqJJWithSite should "contain a ubiq PTM with a site" in {
    // Test here
  }
  ubiqJJIntercedingWIthSite should "contain a ubiq PTM with a site" in {
    // Test here
  }
  ubiqVBNWithSite should "contain a ubiq PTM with a site" in {
    // Test here
  }
  ubiqVBNIntercedingWIthSite should "contain a ubiq PTM with a site" in {
    // Test here
  }
}
