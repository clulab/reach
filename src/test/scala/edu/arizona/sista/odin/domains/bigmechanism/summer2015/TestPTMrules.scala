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
  val ubiqJJWithSite = "The ubiquitinated Ras at Ser16 binds AKT and ASPP2."
  val ubiqJJIntercedingWithSite = "The ubiquitinated Ras protein at Ser16 binds AKT and ASPP2."
  val ubiqVBN = "Ubiquitinated Ras binds AKT and ASPP2."
  val ubiqVBNWithSite = "Ubiquitinated Ras at Ser16 binds AKT and ASPP2."
  val ubiqVBNIntercedingWithSite = "Ubiquitinated Ras proteins at Ser16 binds AKT and ASPP2."
  // Phosphorylated
  val phosJJ = "The phosphorylated Ras binds AKT and ASPP2."
  val phosJJWithSite = "The phosphorylated Ras at Ser16 binds AKT and ASPP2."
  val phosJJIntercedingWithSite = "The phosphorylated Ras protein at Ser16 binds AKT and ASPP2."
  val phosVBN = "Phosphorylated Ras binds AKT and ASPP2."
  val phosVBNWithSite = "Phosphorylated Ras at Ser16 binds AKT and ASPP2."
  val phosVBNIntercedingWithSite = "Phosphorylated Ras proteins at Ser16 binds AKT and ASPP2."
  // Farnesylated
  val farneJJ = "The farnesylated Ras binds AKT and ASPP2."
  val farneJJWithSite = "The farnesylated Ras at Ser16 binds AKT and ASPP2."
  val farneJJIntercedingWithSite = "The farnesylated Ras protein at Ser16 binds AKT and ASPP2."
  val farneVBN = "Farnesylated Ras binds AKT and ASPP2."
  val farneVBNWithSite = "Farnesylated Ras at Ser16 binds AKT and ASPP2."
  val farneVBNIntercedingWithSite = "Farnesylated Ras proteins at Ser16 binds AKT and ASPP2."
  // Ribosylated
  val riboJJ = "The ribosylated Ras binds AKT and ASPP2."
  val riboJJWithSite = "The ribosylated Ras at Ser16 binds AKT and ASPP2."
  val riboJJIntercedingWithSite = "The ribosylated Ras protein at Ser16 binds AKT and ASPP2."
  val riboVBN = "Ribosylated Ras binds AKT and ASPP2."
  val riboVBNWithSite = "Ribosylated Ras at Ser16 binds AKT and ASPP2."
  val riboVBNIntercedingWithSite = "Ribosylated Ras proteins at Ser16 binds AKT and ASPP2."
  // Hydroxylated
  val hydroxJJ = "The hydroxylated Ras binds AKT and ASPP2."
  val hydroxJJWithSite = "The hydroxylated Ras at Ser16 binds AKT and ASPP2."
  val hydroxJJIntercedingWithSite = "The hydroxylated Ras protein at Ser16 binds AKT and ASPP2."
  val hydroxVBN = "Hydroxylated Ras binds AKT and ASPP2."
  val hydroxVBNWithSite = "Hydroxylated Ras at Ser16 binds AKT and ASPP2."
  val hydroxVBNIntercedingWithSite = "Hydroxylated Ras proteins at Ser16 binds AKT and ASPP2."
  // Acetylated
  val aceJJ = "The acetylated Ras binds AKT and ASPP2."
  val aceJJWithSite = "The acetylated Ras at Ser16 binds AKT and ASPP2."
  val aceJJIntercedingWithSite = "The acetylated Ras protein at Ser16 binds AKT and ASPP2."
  val aceVBN = "Acetylated Ras binds AKT and ASPP2."
  val aceVBNWithSite = "Acetylated Ras at Ser16 binds AKT and ASPP2."
  val aceVBNIntercedingWithSite = "Acetylated Ras proteins at Ser16 binds AKT and ASPP2."
  // Glycosylated
  val glycoJJ = "The glycosylated Ras binds AKT and ASPP2."
  val glycoJJWithSite = "The glycosylated Ras at Ser16 binds AKT and ASPP2."
  val glycoJJIntercedingWithSite = "The glycosylated Ras protein at Ser16 binds AKT and ASPP2."
  val glycoVBN = "Glycosylated Ras binds AKT and ASPP2."
  val glycoVBNWithSite = "Glycosylated Ras at Ser16 binds AKT and ASPP2."
  val glycoVBNIntercedingWithSite = "Glycosylated Ras proteins at Ser16 binds AKT and ASPP2."
  // Methylated
  val methJJ = "The methylated Ras binds AKT and ASPP2."
  val methJJWithSite = "The methylated Ras at Ser16 binds AKT and ASPP2."
  val methJJIntercedingWithSite = "The methylated Ras protein at Ser16 binds AKT and ASPP2."
  val methVBN = "Methylated Ras binds AKT and ASPP2."
  val methVBNWithSite = "Methylated Ras at Ser16 binds AKT and ASPP2."
  val methVBNIntercedingWithSite = "Methylated Ras proteins at Ser16 binds AKT and ASPP2."
  // Sumoylated
  val sumoJJ = "The sumoylated Ras binds AKT and ASPP2."
  val sumoJJWithSite = "The sumoylated Ras at Ser16 binds AKT and ASPP2."
  val sumoJJIntercedingWithSite = "The sumoylated Ras protein at Ser16 binds AKT and ASPP2."
  val sumoVBN = "Sumoylated Ras binds AKT and ASPP2."
  val sumoVBNWithSite = "Sumoylated Ras at Ser16 binds AKT and ASPP2."
  val sumoVBNIntercedingWithSite = "Sumoylated Ras proteins at Ser16 binds AKT and ASPP2."

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
    val doc = reach.mkDoc(ubiqVBN, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ubiquitinated" should be (true)
  }
  ubiqJJWithSite should "contain a ubiq PTM with a site" in {
    val doc = reach.mkDoc(ubiqJJWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ubiquitinated" should be (true)
  }
  ubiqJJIntercedingWithSite should "contain a ubiq PTM with a site" in {
    val doc = reach.mkDoc(ubiqJJIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ubiquitinated" should be (true)
  }
  ubiqVBNWithSite should "contain a ubiq PTM with a site" in {
    val doc = reach.mkDoc(ubiqVBNWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ubiquitinated" should be (true)
  }
  ubiqVBNIntercedingWithSite should "contain a ubiq PTM with a site" in {
    val doc = reach.mkDoc(ubiqVBNIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ubiquitinated" should be (true)
  }

  phosJJ should "contain a phos PTM" in {
    val doc = reach.mkDoc(phosJJ, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "phosphorylated" should be (true)
  }
  phosVBN should "contain a phos PTM" in {
    val doc = reach.mkDoc(phosVBN, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "phosphorylated" should be (true)
  }
  phosJJWithSite should "contain a phos PTM with a site" in {
    val doc = reach.mkDoc(phosJJWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "phosphorylated" should be (true)
  }
  phosJJIntercedingWithSite should "contain a phos PTM with a site" in {
    val doc = reach.mkDoc(phosJJIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "phosphorylated" should be (true)
  }
  phosVBNWithSite should "contain a phos PTM with a site" in {
    val doc = reach.mkDoc(phosVBNWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "phosphorylated" should be (true)
  }
  phosVBNIntercedingWithSite should "contain a phos PTM with a site" in {
    val doc = reach.mkDoc(phosVBNIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "phosphorylated" should be (true)
  }

  farneJJ should "contain a farne PTM" in {
    val doc = reach.mkDoc(farneJJ, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "farnesylated" should be (true)
  }
  farneVBN should "contain a farne PTM" in {
    val doc = reach.mkDoc(farneVBN, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "farnesylated" should be (true)
  }
  farneJJWithSite should "contain a farne PTM with a site" in {
    val doc = reach.mkDoc(farneJJWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "farnesylated" should be (true)
  }
  farneJJIntercedingWithSite should "contain a farne PTM with a site" in {
    val doc = reach.mkDoc(farneJJIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "farnesylated" should be (true)
  }
  farneVBNWithSite should "contain a farne PTM with a site" in {
    val doc = reach.mkDoc(farneVBNWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "farnesylated" should be (true)
  }
  farneVBNIntercedingWithSite should "contain a farne PTM with a site" in {
    val doc = reach.mkDoc(farneVBNIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "farnesylated" should be (true)
  }

  riboJJ should "contain a ribo PTM" in {
    val doc = reach.mkDoc(riboJJ, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ribosylated" should be (true)
  }
  riboVBN should "contain a ribo PTM" in {
    val doc = reach.mkDoc(riboVBN, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ribosylated" should be (true)
  }
  riboJJWithSite should "contain a ribo PTM with a site" in {
    val doc = reach.mkDoc(riboJJWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ribosylated" should be (true)
  }
  riboJJIntercedingWithSite should "contain a ribo PTM with a site" in {
    val doc = reach.mkDoc(riboJJIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ribosylated" should be (true)
  }
  riboVBNWithSite should "contain a ribo PTM with a site" in {
    val doc = reach.mkDoc(riboVBNWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ribosylated" should be (true)
  }
  riboVBNIntercedingWithSite should "contain a ribo PTM with a site" in {
    val doc = reach.mkDoc(riboVBNIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "ribosylated" should be (true)
  }

  hydroxJJ should "contain a hydrox PTM" in {
    val doc = reach.mkDoc(hydroxJJ, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "hydroxylated" should be (true)
  }
  hydroxVBN should "contain a hydrox PTM" in {
    val doc = reach.mkDoc(hydroxVBN, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "hydroxylated" should be (true)
  }
  hydroxJJWithSite should "contain a hydrox PTM with a site" in {
    val doc = reach.mkDoc(hydroxJJWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "hydroxylated" should be (true)
  }
  hydroxJJIntercedingWithSite should "contain a hydrox PTM with a site" in {
    val doc = reach.mkDoc(hydroxJJIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "hydroxylated" should be (true)
  }
  hydroxVBNWithSite should "contain a hydrox PTM with a site" in {
    val doc = reach.mkDoc(hydroxVBNWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "hydroxylated" should be (true)
  }
  hydroxVBNIntercedingWithSite should "contain a hydrox PTM with a site" in {
    val doc = reach.mkDoc(hydroxVBNIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "hydroxylated" should be (true)
  }

  aceJJ should "contain a ace PTM" in {
    val doc = reach.mkDoc(aceJJ, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "acetylated" should be (true)
  }
  aceVBN should "contain a ace PTM" in {
    val doc = reach.mkDoc(aceVBN, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "acetylated" should be (true)
  }
  aceJJWithSite should "contain a ace PTM with a site" in {
    val doc = reach.mkDoc(aceJJWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "acetylated" should be (true)
  }
  aceJJIntercedingWithSite should "contain a ace PTM with a site" in {
    val doc = reach.mkDoc(aceJJIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "acetylated" should be (true)
  }
  aceVBNWithSite should "contain a ace PTM with a site" in {
    val doc = reach.mkDoc(aceVBNWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "acetylated" should be (true)
  }
  aceVBNIntercedingWithSite should "contain a ace PTM with a site" in {
    val doc = reach.mkDoc(aceVBNIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "acetylated" should be (true)
  }

  glycoJJ should "contain a glyco PTM" in {
    val doc = reach.mkDoc(glycoJJ, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "glycosylated" should be (true)
  }
  glycoVBN should "contain a glyco PTM" in {
    val doc = reach.mkDoc(glycoVBN, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "glycosylated" should be (true)
  }
  glycoJJWithSite should "contain a glyco PTM with a site" in {
    val doc = reach.mkDoc(glycoJJWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "glycosylated" should be (true)
  }
  glycoJJIntercedingWithSite should "contain a glyco PTM with a site" in {
    val doc = reach.mkDoc(glycoJJIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "glycosylated" should be (true)
  }
  glycoVBNWithSite should "contain a glyco PTM with a site" in {
    val doc = reach.mkDoc(glycoVBNWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "glycosylated" should be (true)
  }
  glycoVBNIntercedingWithSite should "contain a glyco PTM with a site" in {
    val doc = reach.mkDoc(glycoVBNIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "glycosylated" should be (true)
  }

  methJJ should "contain a meth PTM" in {
    val doc = reach.mkDoc(methJJ, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "methylated" should be (true)
  }
  methVBN should "contain a meth PTM" in {
    val doc = reach.mkDoc(methVBN, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "methylated" should be (true)
  }
  methJJWithSite should "contain a meth PTM with a site" in {
    val doc = reach.mkDoc(methJJWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "methylated" should be (true)
  }
  methJJIntercedingWithSite should "contain a meth PTM with a site" in {
    val doc = reach.mkDoc(methJJIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "methylated" should be (true)
  }
  methVBNWithSite should "contain a meth PTM with a site" in {
    val doc = reach.mkDoc(methVBNWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "methylated" should be (true)
  }
  methVBNIntercedingWithSite should "contain a meth PTM with a site" in {
    val doc = reach.mkDoc(methVBNIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "methylated" should be (true)
  }

  sumoJJ should "contain a sumo PTM" in {
    val doc = reach.mkDoc(sumoJJ, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "sumoylated" should be (true)
  }
  sumoVBN should "contain a sumo PTM" in {
    val doc = reach.mkDoc(sumoVBN, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "sumoylated" should be (true)
  }
  sumoJJWithSite should "contain a sumo PTM with a site" in {
    val doc = reach.mkDoc(sumoJJWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "sumoylated" should be (true)
  }
  sumoJJIntercedingWithSite should "contain a sumo PTM with a site" in {
    val doc = reach.mkDoc(sumoJJIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "sumoylated" should be (true)
  }
  sumoVBNWithSite should "contain a sumo PTM with a site" in {
    val doc = reach.mkDoc(sumoVBNWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "sumoylated" should be (true)
  }
  sumoVBNIntercedingWithSite should "contain a sumo PTM with a site" in {
    val doc = reach.mkDoc(sumoVBNIntercedingWithSite, docId, chunkId)
    val mentions = reach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == "sumoylated" should be (true)
  }

}
