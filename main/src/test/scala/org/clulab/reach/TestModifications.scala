package org.clulab.reach

import org.clulab.reach.mentions._
import org.scalatest._
import TestUtils._
import org.clulab.reach.mentions.serialization.json.JSONSerializer
import org.clulab.reach.mentions.serialization.json.prettify

/**
 * Unit tests to ensure PTM rules are matching correctly
 */
class TestModifications extends FlatSpec with Matchers {
  val docId = "testdoc"
  val chunkId = "1"

  // Ubiquitinated
  val ubiquitinationLabel = "Ubiquitination"
  val ubiqJJ = "The ubiquitinated Ras binds AKT and ASPP2."
  val ubiqJJWithSite = "The ubiquitinated Ras at Ser16 binds AKT and ASPP2."
  val ubiqJJIntercedingWithSite = "The ubiquitinated Ras protein at Ser16 binds AKT and ASPP2."
  val ubiqVBN = "Ubiquitinated Ras binds AKT and ASPP2."
  val ubiqVBNWithSite = "Ubiquitinated Ras at Ser16 binds AKT and ASPP2."
  val ubiqVBNIntercedingWithSite = "Ubiquitinated Ras proteins at Ser16 binds AKT and ASPP2."
  val ubiqVBNThatIs = "Src tyrosyl binds Ras that is ubiquitinated at serine 286."
  // Phosphorylated
  val phosphorylationLabel = "Phosphorylation"
  val phosJJ = "The phosphorylated Ras binds AKT and ASPP2."
  val phosJJWithSite = "The phosphorylated Ras at Ser16 binds AKT and ASPP2."
  val phosJJIntercedingWithSite = "The phosphorylated Ras protein at Ser16 binds AKT and ASPP2."
  val phosVBN = "Phosphorylated Ras binds AKT and ASPP2."
  val phosVBNWithSite = "Phosphorylated Ras at Ser16 binds AKT and ASPP2."
  val phosVBNIntercedingWithSite = "Phosphorylated Ras proteins at Ser16 binds AKT and ASPP2."
  val phosVBNThatIs = "Src tyrosyl binds Ras that is phosphorylated at serine 286."
  // Farnesylated
  val farnesylationLabel= "Farnesylation"
  val farneJJ = "The farnesylated Ras binds AKT and ASPP2."
  val farneJJWithSite = "The farnesylated Ras at Ser16 binds AKT and ASPP2."
  val farneJJIntercedingWithSite = "The farnesylated Ras protein at Ser16 binds AKT and ASPP2."
  val farneVBN = "Farnesylated Ras binds AKT and ASPP2."
  val farneVBNWithSite = "Farnesylated Ras at Ser16 binds AKT and ASPP2."
  val farneVBNIntercedingWithSite = "Farnesylated Ras proteins at Ser16 binds AKT and ASPP2."
  val farneVBNThatIs = "Src tyrosyl binds Ras that is farnesylated at serine 286."
  // Ribosylated
  val ribosylationLabel = "Ribosylation"
  val riboJJ = "The ribosylated Ras binds AKT and ASPP2."
  val riboJJWithSite = "The ribosylated Ras at Ser16 binds AKT and ASPP2."
  val riboJJIntercedingWithSite = "The ribosylated Ras protein at Ser16 binds AKT and ASPP2."
  val riboVBN = "Ribosylated Ras binds AKT and ASPP2."
  val riboVBNWithSite = "Ribosylated Ras at Ser16 binds AKT and ASPP2."
  val riboVBNIntercedingWithSite = "Ribosylated Ras proteins at Ser16 binds AKT and ASPP2."
  val riboVBNThatIs = "Src tyrosyl binds Ras that is ribosylated at serine 286."
  // Hydroxylated
  val hydroxylationLabel = "Hydroxylation"
  val hydroxJJ = "The hydroxylated Ras binds AKT and ASPP2."
  val hydroxJJWithSite = "The hydroxylated Ras at Ser16 binds AKT and ASPP2."
  val hydroxJJIntercedingWithSite = "The hydroxylated Ras protein at Ser16 binds AKT and ASPP2."
  val hydroxVBN = "Hydroxylated Ras binds AKT and ASPP2."
  val hydroxVBNWithSite = "Hydroxylated Ras at Ser16 binds AKT and ASPP2."
  val hydroxVBNIntercedingWithSite = "Hydroxylated Ras proteins at Ser16 binds AKT and ASPP2."
  val hydroxVBNThatIs = "Src tyrosyl binds Ras that is hydroxylated at serine 286."
  // Acetylated
  val acetylationLabel = "Acetylation"
  val aceJJ = "The acetylated Ras binds AKT and ASPP2."
  val aceJJWithSite = "The acetylated Ras at Ser16 binds AKT and ASPP2."
  val aceJJIntercedingWithSite = "The acetylated Ras protein at Ser16 binds AKT and ASPP2."
  val aceVBN = "Acetylated Ras binds AKT and ASPP2."
  val aceVBNWithSite = "Acetylated Ras at Ser16 binds AKT and ASPP2."
  val aceVBNIntercedingWithSite = "Acetylated Ras proteins at Ser16 binds AKT and ASPP2."
  val aceVBNThatIs = "Src tyrosyl binds Ras that is acetylated at serine 286."
  // Glycosylated
  val glycosylationLabel = "Glycosylation"
  val glycoJJ = "The glycosylated Ras binds AKT and ASPP2."
  val glycoJJWithSite = "The glycosylated Ras at Ser16 binds AKT and ASPP2."
  val glycoJJIntercedingWithSite = "The glycosylated Ras protein at Ser16 binds AKT and ASPP2."
  val glycoVBN = "Glycosylated Ras binds AKT and ASPP2."
  val glycoVBNWithSite = "Glycosylated Ras at Ser16 binds AKT and ASPP2."
  val glycoVBNIntercedingWithSite = "Glycosylated Ras proteins at Ser16 binds AKT and ASPP2."
  val glycoVBNThatIs = "Src tyrosyl binds Ras that is glycosylated at serine 286."
  // Methylated
  val methylationLabel = "Methylation"
  val methJJ = "The methylated Ras binds AKT and ASPP2."
  val methJJWithSite = "The methylated Ras at Ser16 binds AKT and ASPP2."
  val methJJIntercedingWithSite = "The methylated Ras protein at Ser16 binds AKT and ASPP2."
  val methVBN = "Methylated Ras binds AKT and ASPP2."
  val methVBNWithSite = "Methylated Ras at Ser16 binds AKT and ASPP2."
  val methVBNIntercedingWithSite = "Methylated Ras proteins at Ser16 binds AKT and ASPP2."
  val methVBNThatIs = "Src tyrosyl binds Ras that is methylated at serine 286."
  // Sumoylated
  val sumoylationLabel = "Sumoylation"
  val sumoJJ = "The sumoylated Ras binds AKT and ASPP2."
  val sumoJJWithSite = "The sumoylated Ras at Ser16 binds AKT and ASPP2."
  val sumoJJIntercedingWithSite = "The sumoylated Ras protein at Ser16 binds AKT and ASPP2."
  val sumoVBN = "Sumoylated Ras binds AKT and ASPP2."
  val sumoVBNWithSite = "Sumoylated Ras at Ser16 binds AKT and ASPP2."
  val sumoVBNIntercedingWithSite = "Sumoylated Ras proteins at Ser16 binds AKT and ASPP2."
  val sumoVBNThatIs = "Src tyrosyl binds Ras that is sumoylated at serine 286."


  ubiqJJ should "contain a ubiq PTM" in {
    val doc = testReach.mkDoc(ubiqJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ubiquitinationLabel should be (true)
  }
  ubiqVBN should "contain a ubiq PTM" in {
    val doc = testReach.mkDoc(ubiqVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ubiquitinationLabel should be (true)
  }
  ubiqJJWithSite should "contain a ubiq PTM with a site" in {
    val doc = testReach.mkDoc(ubiqJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ubiquitinationLabel should be (true)
  }
  ubiqJJIntercedingWithSite should "contain a ubiq PTM with a site" in {
    val doc = testReach.mkDoc(ubiqJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ubiquitinationLabel should be (true)
  }
  ubiqVBNWithSite should "contain a ubiq PTM with a site" in {
    val doc = testReach.mkDoc(ubiqVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ubiquitinationLabel should be (true)
  }
  ubiqVBNIntercedingWithSite should "contain a ubiq PTM with a site" in {
    val doc = testReach.mkDoc(ubiqVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ubiquitinationLabel should be (true)
  }
  ubiqVBNThatIs should "contain a ubiq PTM with a site" in {
    val doc = testReach.mkDoc(ubiqVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ubiquitinationLabel should be (true)
  }

  phosJJ should "contain a phos PTM" in {
    val doc = testReach.mkDoc(phosJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == phosphorylationLabel should be (true)
  }
  phosVBN should "contain a phos PTM" in {
    val doc = testReach.mkDoc(phosVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == phosphorylationLabel should be (true)
  }
  phosJJWithSite should "contain a phos PTM with a site" in {
    val doc = testReach.mkDoc(phosJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == phosphorylationLabel should be (true)
  }
  phosJJIntercedingWithSite should "contain a phos PTM with a site" in {
    val doc = testReach.mkDoc(phosJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == phosphorylationLabel should be (true)
  }
  phosVBNWithSite should "contain a phos PTM with a site" in {
    val doc = testReach.mkDoc(phosVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == phosphorylationLabel should be (true)
  }
  phosVBNIntercedingWithSite should "contain a phos PTM with a site" in {
    val doc = testReach.mkDoc(phosVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == phosphorylationLabel should be (true)
  }
  phosVBNThatIs should "contain a phos PTM with a site" in {
    val doc = testReach.mkDoc(phosVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == phosphorylationLabel should be (true)
  }

  farneJJ should "contain a farne PTM" in {
    val doc = testReach.mkDoc(farneJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == farnesylationLabel should be (true)
  }
  farneVBN should "contain a farne PTM" in {
    val doc = testReach.mkDoc(farneVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == farnesylationLabel should be (true)
  }
  farneJJWithSite should "contain a farne PTM with a site" in {
    val doc = testReach.mkDoc(farneJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == farnesylationLabel should be (true)
  }
  farneJJIntercedingWithSite should "contain a farne PTM with a site" in {
    val doc = testReach.mkDoc(farneJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == farnesylationLabel should be (true)
  }
  farneVBNWithSite should "contain a farne PTM with a site" in {
    val doc = testReach.mkDoc(farneVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == farnesylationLabel should be (true)
  }
  farneVBNIntercedingWithSite should "contain a farne PTM with a site" in {
    val doc = testReach.mkDoc(farneVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == farnesylationLabel should be (true)
  }
  farneVBNThatIs should "contain a farne PTM with a site" in {
    val doc = testReach.mkDoc(farneVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == farnesylationLabel should be (true)
  }

  riboJJ should "contain a ribo PTM" in {
    val doc = testReach.mkDoc(riboJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ribosylationLabel should be (true)
  }
  riboVBN should "contain a ribo PTM" in {
    val doc = testReach.mkDoc(riboVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ribosylationLabel should be (true)
  }
  riboJJWithSite should "contain a ribo PTM with a site" in {
    val doc = testReach.mkDoc(riboJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ribosylationLabel should be (true)
  }
  riboJJIntercedingWithSite should "contain a ribo PTM with a site" in {
    val doc = testReach.mkDoc(riboJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ribosylationLabel should be (true)
  }
  riboVBNWithSite should "contain a ribo PTM with a site" in {
    val doc = testReach.mkDoc(riboVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ribosylationLabel should be (true)
  }
  riboVBNIntercedingWithSite should "contain a ribo PTM with a site" in {
    val doc = testReach.mkDoc(riboVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ribosylationLabel should be (true)
  }
  riboVBNThatIs should "contain a ribo PTM with a site" in {
    val doc = testReach.mkDoc(riboVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ribosylationLabel should be (true)
  }

  hydroxJJ should "contain a hydrox PTM" in {
    val doc = testReach.mkDoc(hydroxJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == hydroxylationLabel should be (true)
  }
  hydroxVBN should "contain a hydrox PTM" in {
    val doc = testReach.mkDoc(hydroxVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == hydroxylationLabel should be (true)
  }
  hydroxJJWithSite should "contain a hydrox PTM with a site" in {
    val doc = testReach.mkDoc(hydroxJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == hydroxylationLabel should be (true)
  }
  hydroxJJIntercedingWithSite should "contain a hydrox PTM with a site" in {
    val doc = testReach.mkDoc(hydroxJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == hydroxylationLabel should be (true)
  }
  hydroxVBNWithSite should "contain a hydrox PTM with a site" in {
    val doc = testReach.mkDoc(hydroxVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == hydroxylationLabel should be (true)
  }
  hydroxVBNIntercedingWithSite should "contain a hydrox PTM with a site" in {
    val doc = testReach.mkDoc(hydroxVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == hydroxylationLabel should be (true)
  }
  hydroxVBNThatIs should "contain a hydrox PTM with a site" in {
    val doc = testReach.mkDoc(hydroxVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == hydroxylationLabel should be (true)
  }

  aceJJ should "contain a ace PTM" in {
    val doc = testReach.mkDoc(aceJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == acetylationLabel should be (true)
  }
  aceVBN should "contain a ace PTM" in {
    val doc = testReach.mkDoc(aceVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == acetylationLabel should be (true)
  }
  aceJJWithSite should "contain a ace PTM with a site" in {
    val doc = testReach.mkDoc(aceJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == acetylationLabel should be (true)
  }
  aceJJIntercedingWithSite should "contain a ace PTM with a site" in {
    val doc = testReach.mkDoc(aceJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == acetylationLabel should be (true)
  }
  aceVBNWithSite should "contain a ace PTM with a site" in {
    val doc = testReach.mkDoc(aceVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == acetylationLabel should be (true)
  }
  aceVBNIntercedingWithSite should "contain a ace PTM with a site" in {
    val doc = testReach.mkDoc(aceVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == acetylationLabel should be (true)
  }
  aceVBNThatIs should "contain a ace PTM with a site" in {
    val doc = testReach.mkDoc(aceVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == acetylationLabel should be (true)
  }

  glycoJJ should "contain a glyco PTM" in {
    val doc = testReach.mkDoc(glycoJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == glycosylationLabel should be (true)
  }
  glycoVBN should "contain a glyco PTM" in {
    val doc = testReach.mkDoc(glycoVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == glycosylationLabel should be (true)
  }
  glycoJJWithSite should "contain a glyco PTM with a site" in {
    val doc = testReach.mkDoc(glycoJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == glycosylationLabel should be (true)
  }
  glycoJJIntercedingWithSite should "contain a glyco PTM with a site" in {
    val doc = testReach.mkDoc(glycoJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == glycosylationLabel should be (true)
  }
  glycoVBNWithSite should "contain a glyco PTM with a site" in {
    val doc = testReach.mkDoc(glycoVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == glycosylationLabel should be (true)
  }
  glycoVBNIntercedingWithSite should "contain a glyco PTM with a site" in {
    val doc = testReach.mkDoc(glycoVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == glycosylationLabel should be (true)
  }
  glycoVBNThatIs should "contain a glyco PTM with a site" in {
    val doc = testReach.mkDoc(glycoVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == glycosylationLabel should be (true)
  }

  methJJ should "contain a meth PTM" in {
    val doc = testReach.mkDoc(methJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == methylationLabel should be (true)
  }
  methVBN should "contain a meth PTM" in {
    val doc = testReach.mkDoc(methVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == methylationLabel should be (true)
  }
  methJJWithSite should "contain a meth PTM with a site" in {
    val doc = testReach.mkDoc(methJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == methylationLabel should be (true)
  }
  methJJIntercedingWithSite should "contain a meth PTM with a site" in {
    val doc = testReach.mkDoc(methJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == methylationLabel should be (true)
  }
  methVBNWithSite should "contain a meth PTM with a site" in {
    val doc = testReach.mkDoc(methVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == methylationLabel should be (true)
  }
  methVBNIntercedingWithSite should "contain a meth PTM with a site" in {
    val doc = testReach.mkDoc(methVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == methylationLabel should be (true)
  }
  methVBNThatIs should "contain a meth PTM with a site" in {
    val doc = testReach.mkDoc(methVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == methylationLabel should be (true)
  }

  sumoJJ should "contain a sumo PTM" in {
    val doc = testReach.mkDoc(sumoJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == sumoylationLabel should be (true)
  }
  sumoVBN should "contain a sumo PTM" in {
    val doc = testReach.mkDoc(sumoVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == sumoylationLabel should be (true)
  }
  sumoJJWithSite should "contain a sumo PTM with a site" in {
    val doc = testReach.mkDoc(sumoJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == sumoylationLabel should be (true)
  }
  sumoJJIntercedingWithSite should "contain a sumo PTM with a site" in {
    val doc = testReach.mkDoc(sumoJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == sumoylationLabel should be (true)
  }
  sumoVBNWithSite should "contain a sumo PTM with a site" in {
    val doc = testReach.mkDoc(sumoVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == sumoylationLabel should be (true)
  }
  sumoVBNIntercedingWithSite should "contain a sumo PTM with a site" in {
    val doc = testReach.mkDoc(sumoVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == sumoylationLabel should be (true)
  }
  sumoVBNThatIs should "contain a sumo PTM with a site" in {
    val doc = testReach.mkDoc(sumoVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == sumoylationLabel should be (true)
  }

  val sent1 = "The phosphorylated AKT binds to ASPP2."
  val sent1b = "The ubiquitinated AKT binds to ASPP2."
  "ReachSystem" should "not find a PTMs as events" in {
    var mentions = getBioMentions(sent1)
    val p = mentions.find(_ matches "Phosphorylation")
    p.isDefined should be (false) // Dane
    var b = mentions.find(_ matches "Binding")
    b.isDefined should be (true) // Marco

    mentions = getBioMentions(sent1b)
    val u = mentions.find(_ matches "Ubiquitination")
    u.isDefined should be (false)
    b = mentions.find(_ matches "Binding")
    b.isDefined should be (true)
  }

  val sent2 = "We demonstrate that the RBD of PI3KC2β binds nucleotide-free Ras in vitro."
  s"""PI3KC2β in "$sent2"""" should "have 1 \"site of protein\" EventSite modification" in {
    // Also: if the entity modification has no type, it should be propagated up in the event using the entity
    val mentions = getEntities(sent2)
    val p = mentions.filter(_ matches "Gene_or_gene_product")
    p should have size (1)
    // This tests whether the modification is present
    getEventSites(p.head) should have size (1)
  }

  // Test EventSite modifications
  val sent3a = "Experiments revealed ubiquitination at Lys residues 104 and 147 of K-Ras"
  s"""K-Ras in "$sent3a"""" should "have 2 EventSites after the modificationEngine" in {
    val mentions =  getEntities(sent3a)
    val p = mentions.filter(_ matches "Gene_or_gene_product")
    p should have size (1)
    // This tests whether the modification is present
    getEventSites(p.head) should have size (2)
  }

  // Test EventSite modifications
  val sent3b = "Experiments revealed ubiquitination at Lys residues 117, 147, and 170 for H-Ras."
  s""""H-Ras in "$sent3b"s""" should "have 3 EventSites after the modificationEngine" in {
    val mentions =  getEntities(sent3b)
    val p = mentions.filter(_ matches "Gene_or_gene_product")
    p should have size (1)
    // This tests whether the modification is present
    getEventSites(p.head) should have size (3)
  }

  val sent4 = "Phosphorylated Mek binds to GTP."
  sent4 should "not contain a phosphorylation event (this is a PTM)" in {
    val doc = testReach.mkDoc(sent4, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_ matches "Phosphorylation") should be (false)
  }

  val sent5 = "Ligation of ASPP2 to hydroxylated RAS-GTP promotes apoptosis."
  sent5 should "not contain a hydroxylation event (this is a PTM)" in {
    val doc = testReach.mkDoc(sent5, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_ matches "Hydroxylation") should be (false)
  }

  val sent6 = "Optineurin regulates NF-kappaB activation by mediating interaction of CYLD with ubiquitinated RIP."
  sent6 should "not contain a ubiquitination event (this is a PTM)" in {
    val doc = testReach.mkDoc(sent6, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_ matches "Ubiquitination") should be (false)
  }

  val sent7 = "The ubiquitinated Ras protein phosphorylates AKT."
  // the example text says that Ras is ubiquitinated
  // that should be reflected as a PTM in ras.modifications
  sent7 should "contain a Ras with a PTM" in {
    val doc = testReach.mkDoc(sent7, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    // there is only one PTM in the example text
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == ubiquitinationLabel should be (true)
  }

  val sent8 = "Ras does not phosphorylate Mek"
  sent8 should "have a negated positive regulation" in {
    val doc = testReach.mkDoc(sent8, docId, chunkId)
    val mentions = testReach extractFrom doc
    mentions filter (_ matches "Event") should have size (2)
    val phospho = mentions.find(_ matches "Phosphorylation")
    phospho should be ('defined)
    phospho.get.arguments.keySet should not contain ("cause")
    phospho.get.modifications.filter(_.isInstanceOf[Negation]) should be ('empty)
    val reg = mentions.find(_ matches "Positive_regulation")
    reg should be ('defined)
    reg.get.modifications.filter(_.isInstanceOf[Negation]) should have size (1)
  }

  val sent9 = "The phosphorylated p53 by ASPP2 is doing something..."
  // this is not a PTM! It is an event with a cause
  sent9 should "contain 1 phosphorylation and 1 regulation event" in {
    val mentions = getBioMentions(sent9)
    hasEventWithArguments("Phosphorylation", List("p53"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP2", "Phosphorylation", List("p53"), mentions) should be (true)
  }

  //
  // a few tests for mutation modifications
  //
  val sent10 = "Note that only K650M and K650E-FGFR3 mutants cause STAT1 phosphorylation"
  sent10 should "have 2 mutations for FGFR3" in {
    val mentions = getBioMentions(sent10)
    val fgfr = mentions.filter(m => (m.text contains "FGFR3") && m.isInstanceOf[BioTextBoundMention])
    fgfr should have size (2)
    fgfr(0).countMutations should be (1)
    fgfr(1).countMutations should be (1)
    val mutations = fgfr.flatMap(_.modifications.filter(_.isInstanceOf[Mutant]).map(_.asInstanceOf[Mutant].evidence.text))
    mutations should contain ("K650M")
    mutations should contain ("K650E")
  }

  val sent11 = "Note that only FGFR3 K650M causes STAT1 phosphorylation"
  sent11 should "have 1 mutation for FGFR3" in {
    val mentions = getBioMentions(sent11)
    val fgfr = mentions.filter(m => (m.text contains "FGFR3") && m.isInstanceOf[BioTextBoundMention])
    fgfr should have size (1)
    fgfr.head.countMutations should be (1)
  }

  val sent12 = "Note that only the K650M-FGFR3 mutant causes STAT1 phosphorylation"
  sent12 should "have 1 mutation for FGFR3" in {
    val mentions = getBioMentions(sent12)
    val fgfr = mentions.filter(m => (m.text contains "FGFR3") && m.isInstanceOf[BioTextBoundMention])
    fgfr should have size (1)
    fgfr.head.hasMutation("K650M") should be (true)
  }

  val sent13 = "monoubiquitinated K-Ras is less sensitive than the unmodified protein to GAP-mediated GTP hydrolysis"
  sent13 should "not contain a ubiquitination event (this is a PTM)" in {
    val mentions = getBioMentions(sent13)
    mentions.count(_ matches "Ubiquitination") should be (0)
    hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions) should be (false)
    val kras = mentions.find(_.text contains "K-Ras")
    kras.isDefined should be (true)
    // there is only one PTM in the example text
    kras.get.modifications.size == 1 should be (true)
    val ptm = kras.get.modifications.head
    ptm.label == ubiquitinationLabel should be (true)
  }

  //
  // a few tests for modifications in parens
  //
  val sent14 = "all six FGFR3 mutants induced activatory ERK(T202/Y204) phosphorylation (Fig. 2)."
  sent14 should "contain 2 phosphorylations (one for each ERK mutation) and 2 Positive Regulations" in {
    val mentions = getBioMentions(sent14)

    // We have one phosphorylation per Site
    val phosphos = mentions.filter(_ matches "Phosphorylation")
    phosphos should have size (2)
    val s1 = phosphos.head.arguments.getOrElse("site", Nil)
    val s2 = phosphos.last.arguments.getOrElse("site", Nil)
    s1 should have size (1)
    s2 should have size (1)
    val ss = Seq(s1.head.text, s2.head.text)
    ss should contain ("T202")
    ss should contain ("Y204")

    // There should be 2 Positive Regulations (one for each phospho)
    mentions.count(_ matches "Positive_regulation") should be (2)
  }

  val sent15 = "all six FGFR3 mutants induced activatory ERK(K156M/H204M) phosphorylation (Fig. 2)."
  sent15 should "contain 2 Positive Regulations NOT Activations (1 for each ERK mutation)" in {
    val mentions = getBioMentions(sent15)

    // We have 1 Reg per ERK mutant
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (2)
  }

  val sent16 = "all six FGFR3 mutants induced activatory ERK(K156M, H204M) phosphorylation (Fig. 2)."
  sent16 should "contain 2 Positive Regulations NOT Activations (1 for each ERK mutation)" in {
    val mentions = getBioMentions(sent16)

    // We have 1 Reg per ERK mutant
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (2)

  }

  val siteTest1 = "activatory ERK(T202/Y204) phosphorylation (Fig. 2)."
  siteTest1 should "contain 2 sites (distinct phosphorylations)" in {
    val mentions = getBioMentions(siteTest1)

    // We have one phosphorylation per Site
    val phosphos = mentions.filter(_ matches "Phosphorylation")
    phosphos should have size (2)
    val s1 = phosphos.head.arguments.getOrElse("site", Nil)
    val s2 = phosphos.last.arguments.getOrElse("site", Nil)
    s1 should have size (1)
    s2 should have size (1)
    val ss = Seq(s1.head.text, s2.head.text)
    ss should contain ("T202")
    ss should contain ("Y204")
  }

  val siteTest2 = "Ser56 RAS"
  siteTest2 should "contain 2 entities (1 Site)" in {
    val mentions = getBioMentions(siteTest2)
    mentions should have size (2)
    mentions.count(_ matches "Site") should be (1)
  }

  val mutantTest1 = "all six FGFR3 mutants induced activatory ERK(K156M/H204M) phosphorylation (Fig. 2)."
  mutantTest1 should "contain 2 mutations for ERK and 1 for FGFR3" in {
    val mentions = getBioMentions(mutantTest1)

    val fgfr = mentions filter(_.text == "FGFR3")
    fgfr should have size (1)
    fgfr.head.countMutations should be (1)
    fgfr.head hasMutation "mutants" should be (true)

    val erk = mentions filter(_.text == "ERK")
    erk should have size (2)
    erk(0).countMutations should be (1)
    erk(1).countMutations should be (1)
    val mutations = erk.flatMap(_.modifications.filter(_.isInstanceOf[Mutant]).map(_.asInstanceOf[Mutant].evidence.text))
    mutations should contain ("K156M")
    mutations should contain ("H204M")
  }

  val mutantTest2 = "all six FGFR3 mutants induced activatory ERK(K156M, H204M) phosphorylation (Fig. 2)."
  mutantTest2 should "contain 2 mutations for ERK and 1 for FGFR3" in {
    val mentions = getBioMentions(mutantTest2)

    val fgfr = mentions filter(_.text == "FGFR3")
    fgfr should have size (1)
    fgfr.head.countMutations should be (1)
    fgfr.head hasMutation "mutants" should be (true)

    val erk = mentions filter(_.text == "ERK")
    erk should have size (2)
    erk(0).countMutations should be (1)
    erk(1).countMutations should be (1)
    val mutations = erk.flatMap(_.modifications.filter(_.isInstanceOf[Mutant]).map(_.asInstanceOf[Mutant].evidence.text))
    mutations should contain ("K156M")
    mutations should contain ("H204M")
  }

  val mutantTest3 = "MEK R567Q"
  mutantTest3 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest3)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation "R567Q" should be (true)
  }

  val mutantTest4 = "MEK mutant R567Q"
  mutantTest4 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest4)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation "R567Q" should be (true)
  }

  val mutantTest5 = "MEK (R678Q, G890K)"
  mutantTest5 should "contain 1 entity with 2 Mutant modification" in {
    val mentions = getBioMentions(mutantTest5)
    mentions should have size (2)
    mentions(0).countMutations should be (1)
    mentions(1).countMutations should be (1)
    val mutations = mentions.flatMap(_.modifications.filter(_.isInstanceOf[Mutant]).map(_.asInstanceOf[Mutant].evidence.text))
    mutations should contain ("R678Q")
    mutations should contain ("G890K")
  }

  val mutantTest6 = "K111M and K112M ASPP1 mutants and ASPP2"
  mutantTest6 should "countain 2 ASPP1 with 1 Mutant each and ASPP2 with 0 Mutant mods" in {
    val mentions = getBioMentions(mutantTest6)
    mentions should have size (3)
    val asppOne = mentions filter (_.text == "ASPP1")
    asppOne should have size (2)
    asppOne(0).countMutations should be (1)
    asppOne(1).countMutations should be (1)
    val mutations = asppOne.flatMap(_.modifications.filter(_.isInstanceOf[Mutant]).map(_.asInstanceOf[Mutant].evidence.text))
    mutations should contain ("K111M")
    mutations should contain ("K112M")

    val asppTwo = mentions filter (_.text == "ASPP2")
    asppTwo should have size (1)
    asppTwo.head.countMutations should be (0)
  }

  val mutantTest7 = "K111M, K112M, and K113M ASPP1 mutants and ASPP2"
  mutantTest7 should "countain 3 ASPP1 with 1 Mutant each and ASPP2 with 0 Mutant mods" in {
    val mentions = getBioMentions(mutantTest7)
    mentions should have size (4)
    val asppOne = mentions filter (_.text == "ASPP1")
    asppOne should have size (3)
    asppOne(0).countMutations should be (1)
    asppOne(1).countMutations should be (1)
    asppOne(2).countMutations should be (1)
    val mutations = asppOne.flatMap(_.modifications.filter(_.isInstanceOf[Mutant]).map(_.asInstanceOf[Mutant].evidence.text))
    mutations should contain ("K111M")
    mutations should contain ("K112M")
    mutations should contain ("K113M")

    val asppTwo = mentions filter (_.text == "ASPP2")
    asppTwo should have size (1)
    asppTwo.head.countMutations should be (0)
  }

  val mutantTest8 = "ASPP1 mutants K111M, K112M, and K113M and ASPP2"
  mutantTest8 should "countain 3 ASPP1 with 1 Mutant each and ASPP2 with 0 Mutant mods" in {
    val mentions = getBioMentions(mutantTest8)
    mentions should have size (4)
    val asppOne = mentions filter (_.text == "ASPP1")
    asppOne should have size (3)
    asppOne(0).countMutations should be (1)
    asppOne(1).countMutations should be (1)
    asppOne(2).countMutations should be (1)
    val mutations = asppOne.flatMap(_.modifications.filter(_.isInstanceOf[Mutant]).map(_.asInstanceOf[Mutant].evidence.text))
    mutations should contain ("K111M")
    mutations should contain ("K112M")
    mutations should contain ("K113M")

    val asppTwo = mentions filter (_.text == "ASPP2")
    asppTwo should have size (1)
    asppTwo.head.countMutations should be (0)
  }

  val mutantTest9 = "Ser785His mutant RAS"
  mutantTest9 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest9)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation "Ser785His" should be (true)
  }

  val mutantTest10 = "Ser785His RAS"
  mutantTest10 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest10)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation "Ser785His" should be (true)
  }

  val mutantTest11 = "Ser785His mutant of RAS"
  mutantTest11 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest11)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation "Ser785His" should be (true)
  }

  val mutantTest12 = "K111M, K112M, and K113M mutants of ASPP1 and the RAS complex are phosphorylated."
  mutantTest12 should "countain 3 ASPP1 with 1 Mutant each and ASPP2 with 0 Mutant mods" in {
    val mentions = getBioMentions(mutantTest12)
    mentions should have size (8)
    val asppOne = mentions filter (_.text == "ASPP1")
    asppOne should have size (3)
    asppOne(0).countMutations should be (1)
    asppOne(1).countMutations should be (1)
    asppOne(2).countMutations should be (1)
    val mutations = asppOne.flatMap(_.modifications.filter(_.isInstanceOf[Mutant]).map(_.asInstanceOf[Mutant].evidence.text))
    mutations should contain ("K111M")
    mutations should contain ("K112M")
    mutations should contain ("K113M")

    val ras = mentions filter (_.text == "RAS")
    ras should have size (1)
    ras.head.countMutations should be (0)
  }

  val mutantTest13 = "Ser785His mutation of RAS"
  mutantTest13 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest13)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation "Ser785His" should be (true)
  }

  val mutantTest14 = "K111M, K112M, and K113M mutations of ASPP1 and the RAS complex are phosphorylated."
  mutantTest14 should "countain 3 ASPP1 with 1 Mutant each and ASPP2 with 0 Mutant mods" in {
    val mentions = getBioMentions(mutantTest14)
    mentions should have size (8)
    val asppOne = mentions filter (_.text == "ASPP1")
    asppOne should have size (3)
    asppOne(0).countMutations should be (1)
    asppOne(1).countMutations should be (1)
    asppOne(2).countMutations should be (1)
    val mutations = asppOne.flatMap(_.modifications.filter(_.isInstanceOf[Mutant]).map(_.asInstanceOf[Mutant].evidence.text))
    mutations should contain ("K111M")
    mutations should contain ("K112M")
    mutations should contain ("K113M")

    val ras = mentions filter (_.text == "RAS")
    ras should have size (1)
    ras.head.countMutations should be (0)
  }

  val mutantTest15 = "Mutation of the PIK3CA gene"
  mutantTest15 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest15)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation "Mutation" should be (true)
  }

  val mutantTest16 = "We used a substitution mutant of Raf (76A>T)"
  mutantTest16 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest16)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation ("76A>T", "SubstitutionMutant") should be (true)
  }

  val mutantTest17 = "We used a deletion mutant of Raf (ΔF508)"
  mutantTest17 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest17)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation ("ΔF508", "DeletionMutant") should be (true)
  }

  val mutantTest18a = "We used a deletion mutant of Raf (K29del)"
  val mutantTest18b = "We used a deletion mutant of Raf (29delK)"
  val mutantTest18c = "We used a deletion mutant of Raf (M27_K29del)"
  val mutantTest18d = "We used a deletion mutant of Raf (27_29del)"
  mutantTest18a should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest18a)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation ("K29del", "DeletionMutant") should be (true)
  }
  mutantTest18b should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest18b)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation ("29delK", "DeletionMutant") should be (true)
  }
  mutantTest18c should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest18c)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation ("M27_K29del", "DeletionMutant") should be (true)
  }
  mutantTest18d should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest18d)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation ("27_29del", "DeletionMutant") should be (true)
  }

  val mutantTest19 = "We used an insertion mutant of Raf (K29_M30insQSK)"
  mutantTest19 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest19)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation ("K29_M30insQSK", "InsertionMutant") should be (true)
  }

  val mutantTest20 = "We used a duplication mutant of Raf (G31_Q33dup)"
  mutantTest20 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest20)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation ("G31_Q33dup", "DuplicationMutant") should be (true)
  }

  val mutantTest21 = "We used a frameshift mutant of Raf (Arg83fs)"
  mutantTest21 should "contain 1 entity with 1 Mutant modification" in {
    val mentions = getBioMentions(mutantTest21)
    mentions should have size (1)
    mentions.head.countMutations should be (1)
    mentions.head hasMutation ("Arg83fs", "FrameshiftMutant") should be (true)
  }

  val siteTest3 = "Phosphorylation (p) of Akt (Ser-473), mTOR (Ser 2448) and Rictor (Ser 792) was quantified."
  siteTest2 should "contain 3 Sites" in {
    val mentions = getBioMentions(siteTest3)
    val sites = mentions.filter(_ matches "Site")
    sites should have size (3)
    sites.exists(_.text contains "Ser-473") should be (true)
  }

  val siteTest4 = "Phosphorylation of Akt (S473M) was attenuated."
  siteTest4 should "not contain any sites" in {
    val mentions = getBioMentions(siteTest4)
    val sites = mentions.filter(_ matches "Site")
    sites should have size (0)
    val akt = mentions filter (_.text == "Akt")
    akt should have size (1)
    akt.head.countMutations should be (1)
  }

  val modSiteExcludeTest1 = "The PTEN protein contains a CK2 phosphorylation site."
  modSiteExcludeTest1 should "not contain any phosphorylation of CK2" in {
    val mentions = getBioMentions(modSiteExcludeTest1)
    hasEventWithArguments("Phosphorylation", List("CK2"), mentions) should be (false)
  }

  val modSiteExcludeTest2 = "Mutations of the AKT1 phosphorylation site of RUNX3 decreased RUNX3 sumoylation."
  modSiteExcludeTest2 should "not contain any phosphorylation of AKT1" in {
    val mentions = getBioMentions(modSiteExcludeTest2)
    hasEventWithArguments("Phosphorylation", List("AKT1"), mentions) should be (false)
  }

  val modSiteExcludeTest3 = "We studied the MEK phosphorlyation site of ERK."
  modSiteExcludeTest3 should "not contain any phosphorylation of MEK" in {
    val mentions = getBioMentions(modSiteExcludeTest3)
    hasEventWithArguments("Phosphorylation", List("MEK"), mentions) should be (false)
  }

  // This originated in the wild in case you are wondering.
  val koTriggerTest1 = "Tbet Rag2 mice (Garrett et al., 2010) as well as Bacteroides spp. (Bloom et al., 2011), Helicobacter spp. (Fox et al., 2011), and Bilophila wadsworthia (Devkota et al., 2012) in Il10 have been shown to enhance intestinal inflammation.The acute dextran sulfate sodium"
  koTriggerTest1 should "contain KOtriggers and be able to (de)serialize them" in {

    def getKOtriggers(bioMentions: Seq[BioMention]) = bioMentions.flatMap { mention => mention.modifications.collect { case koTrigger: KOtrigger => koTrigger } }

    val bioMentions1 = getBioMentions(koTriggerTest1)
    val koTriggers1 = getKOtriggers(bioMentions1)

    koTriggers1 should not be ('empty)

    val jValue = JSONSerializer.jsonAST(bioMentions1)
    val json = prettify(jValue)

    json should include ("KOtrigger")

    val bioMentions2 = JSONSerializer.toBioMentions(jValue)
    val koTriggers2 = getKOtriggers(bioMentions2)

    koTriggers1 should contain theSameElementsInOrderAs (koTriggers2)
  }
}
