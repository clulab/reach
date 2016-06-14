package edu.arizona.sista.reach

import edu.arizona.sista.reach.mentions._
import org.scalatest._
import scala.util.Try
import TestUtils._

/**
 * Unit tests to ensure PTM rules are matching correctly for "reverse" PTMs.
 */
class TestDeModifications extends FlatSpec with Matchers {
  val docId = "testdocR"
  val chunkId = "2"

  // Deubiquitinated
  val deubiquitinationLabel = "Deubiquitination"
  val deubiqJJ = "The deubiquitinated Ras binds AKT and ASPP2."
  val deubiqJJWithSite = "The deubiquitinated Ras at Ser16 binds AKT and ASPP2."
  val deubiqJJIntercedingWithSite = "The deubiquitinated Ras protein at Ser16 binds AKT and ASPP2."
  val deubiqVBN = "Deubiquitinated Ras binds AKT and ASPP2."
  val deubiqVBNWithSite = "Deubiquitinated Ras at Ser16 binds AKT and ASPP2."
  val deubiqVBNIntercedingWithSite = "Deubiquitinated Ras proteins at Ser16 binds AKT and ASPP2."
  val deubiqVBNThatIs = "Src tyrosyl binds Ras that is deubiquitinated at serine 286."
  // Dephosphorylated
  val dephosphorylationLabel = "Dephosphorylation"
  val phosJJ = "The dephosphorylated Ras binds AKT and ASPP2."
  val phosJJWithSite = "The dephosphorylated Ras at Ser16 binds AKT and ASPP2."
  val phosJJIntercedingWithSite = "The dephosphorylated Ras protein at Ser16 binds AKT and ASPP2."
  val phosVBN = "Dephosphorylated Ras binds AKT and ASPP2."
  val phosVBNWithSite = "Dephosphorylated Ras at Ser16 binds AKT and ASPP2."
  val phosVBNIntercedingWithSite = "Dephosphorylated Ras proteins at Ser16 binds AKT and ASPP2."
  val phosVBNThatIs = "Src tyrosyl binds Ras that is dephosphorylated at serine 286."
  // Defarnesylated
  val defarnesylationLabel= "Defarnesylation"
  val defarneJJ = "The defarnesylated Ras binds AKT and ASPP2."
  val defarneJJWithSite = "The defarnesylated Ras at Ser16 binds AKT and ASPP2."
  val defarneJJIntercedingWithSite = "The defarnesylated Ras protein at Ser16 binds AKT and ASPP2."
  val defarneVBN = "Defarnesylated Ras binds AKT and ASPP2."
  val defarneVBNWithSite = "Defarnesylated Ras at Ser16 binds AKT and ASPP2."
  val defarneVBNIntercedingWithSite = "Defarnesylated Ras proteins at Ser16 binds AKT and ASPP2."
  val defarneVBNThatIs = "Src tyrosyl binds Ras that is defarnesylated at serine 286."
  // Deribosylated
  val deribosylationLabel = "Deribosylation"
  val deriboJJ = "The deribosylated Ras binds AKT and ASPP2."
  val deriboJJWithSite = "The deribosylated Ras at Ser16 binds AKT and ASPP2."
  val deriboJJIntercedingWithSite = "The deribosylated Ras protein at Ser16 binds AKT and ASPP2."
  val deriboVBN = "Deribosylated Ras binds AKT and ASPP2."
  val deriboVBNWithSite = "Deribosylated Ras at Ser16 binds AKT and ASPP2."
  val deriboVBNIntercedingWithSite = "Deribosylated Ras proteins at Ser16 binds AKT and ASPP2."
  val deriboVBNThatIs = "Src tyrosyl binds Ras that is deribosylated at serine 286."
  // Dehydroxylated
  val dehydroxylationLabel = "Dehydroxylation"
  val dehydroxJJ = "The dehydroxylated Ras binds AKT and ASPP2."
  val dehydroxJJWithSite = "The dehydroxylated Ras at Ser16 binds AKT and ASPP2."
  val dehydroxJJIntercedingWithSite = "The dehydroxylated Ras protein at Ser16 binds AKT and ASPP2."
  val dehydroxVBN = "Dehydroxylated Ras binds AKT and ASPP2."
  val dehydroxVBNWithSite = "Dehydroxylated Ras at Ser16 binds AKT and ASPP2."
  val dehydroxVBNIntercedingWithSite = "Dehydroxylated Ras proteins at Ser16 binds AKT and ASPP2."
  val dehydroxVBNThatIs = "Src tyrosyl binds Ras that is dehydroxylated at serine 286."
  // Deacetylated
  val deacetylationLabel = "Deacetylation"
  val aceJJ = "The deacetylated Ras binds AKT and ASPP2."
  val aceJJWithSite = "The deacetylated Ras at Ser16 binds AKT and ASPP2."
  val aceJJIntercedingWithSite = "The deacetylated Ras protein at Ser16 binds AKT and ASPP2."
  val aceVBN = "Deacetylated Ras binds AKT and ASPP2."
  val aceVBNWithSite = "Deacetylated Ras at Ser16 binds AKT and ASPP2."
  val aceVBNIntercedingWithSite = "Deacetylated Ras proteins at Ser16 binds AKT and ASPP2."
  val aceVBNThatIs = "Src tyrosyl binds Ras that is deacetylated at serine 286."
  // Deglycosylated
  val deglycosylationLabel = "Deglycosylation"
  val deglycoJJ = "The deglycosylated Ras binds AKT and ASPP2."
  val deglycoJJWithSite = "The deglycosylated Ras at Ser16 binds AKT and ASPP2."
  val deglycoJJIntercedingWithSite = "The deglycosylated Ras protein at Ser16 binds AKT and ASPP2."
  val deglycoVBN = "Deglycosylated Ras binds AKT and ASPP2."
  val deglycoVBNWithSite = "Deglycosylated Ras at Ser16 binds AKT and ASPP2."
  val deglycoVBNIntercedingWithSite = "Deglycosylated Ras proteins at Ser16 binds AKT and ASPP2."
  val deglycoVBNThatIs = "Src tyrosyl binds Ras that is deglycosylated at serine 286."
  // Demethylated
  val demethylationLabel = "Demethylation"
  val methJJ = "The demethylated Ras binds AKT and ASPP2."
  val methJJWithSite = "The demethylated Ras at Ser16 binds AKT and ASPP2."
  val methJJIntercedingWithSite = "The demethylated Ras protein at Ser16 binds AKT and ASPP2."
  val methVBN = "Demethylated Ras binds AKT and ASPP2."
  val methVBNWithSite = "Demethylated Ras at Ser16 binds AKT and ASPP2."
  val methVBNIntercedingWithSite = "Demethylated Ras proteins at Ser16 binds AKT and ASPP2."
  val methVBNThatIs = "Src tyrosyl binds Ras that is demethylated at serine 286."
  // Desumoylated
  val desumoylationLabel = "Desumoylation"
  val sumoJJ = "The desumoylated Ras binds AKT and ASPP2."
  val sumoJJWithSite = "The desumoylated Ras at Ser16 binds AKT and ASPP2."
  val sumoJJIntercedingWithSite = "The desumoylated Ras protein at Ser16 binds AKT and ASPP2."
  val sumoVBN = "Desumoylated Ras binds AKT and ASPP2."
  val sumoVBNWithSite = "Desumoylated Ras at Ser16 binds AKT and ASPP2."
  val sumoVBNIntercedingWithSite = "Desumoylated Ras proteins at Ser16 binds AKT and ASPP2."
  val sumoVBNThatIs = "Src tyrosyl binds Ras that is desumoylated at serine 286."


  deubiqJJ should "contain a deubiq PTM" in {
    val doc = testReach.mkDoc(deubiqJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deubiquitinationLabel should be (true)
  }
  deubiqVBN should "contain a deubiq PTM" in {
    val doc = testReach.mkDoc(deubiqVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deubiquitinationLabel should be (true)
  }
  deubiqJJWithSite should "contain a deubiq PTM with a site" in {
    val doc = testReach.mkDoc(deubiqJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deubiquitinationLabel should be (true)
  }
  deubiqJJIntercedingWithSite should "contain a deubiq PTM with a site" in {
    val doc = testReach.mkDoc(deubiqJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deubiquitinationLabel should be (true)
  }
  deubiqVBNWithSite should "contain a deubiq PTM with a site" in {
    val doc = testReach.mkDoc(deubiqVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deubiquitinationLabel should be (true)
  }
  deubiqVBNIntercedingWithSite should "contain a deubiq PTM with a site" in {
    val doc = testReach.mkDoc(deubiqVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deubiquitinationLabel should be (true)
  }
  deubiqVBNThatIs should "contain a deubiq PTM with a site" in {
    val doc = testReach.mkDoc(deubiqVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deubiquitinationLabel should be (true)
  }

  phosJJ should "contain a phos PTM" in {
    val doc = testReach.mkDoc(phosJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dephosphorylationLabel should be (true)
  }
  phosVBN should "contain a phos PTM" in {
    val doc = testReach.mkDoc(phosVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dephosphorylationLabel should be (true)
  }
  phosJJWithSite should "contain a phos PTM with a site" in {
    val doc = testReach.mkDoc(phosJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dephosphorylationLabel should be (true)
  }
  phosJJIntercedingWithSite should "contain a phos PTM with a site" in {
    val doc = testReach.mkDoc(phosJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dephosphorylationLabel should be (true)
  }
  phosVBNWithSite should "contain a phos PTM with a site" in {
    val doc = testReach.mkDoc(phosVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dephosphorylationLabel should be (true)
  }
  phosVBNIntercedingWithSite should "contain a phos PTM with a site" in {
    val doc = testReach.mkDoc(phosVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dephosphorylationLabel should be (true)
  }
  phosVBNThatIs should "contain a phos PTM with a site" in {
    val doc = testReach.mkDoc(phosVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dephosphorylationLabel should be (true)
  }

  defarneJJ should "contain a defarne PTM" in {
    val doc = testReach.mkDoc(defarneJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == defarnesylationLabel should be (true)
  }
  defarneVBN should "contain a defarne PTM" in {
    val doc = testReach.mkDoc(defarneVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == defarnesylationLabel should be (true)
  }
  defarneJJWithSite should "contain a defarne PTM with a site" in {
    val doc = testReach.mkDoc(defarneJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == defarnesylationLabel should be (true)
  }
  defarneJJIntercedingWithSite should "contain a defarne PTM with a site" in {
    val doc = testReach.mkDoc(defarneJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == defarnesylationLabel should be (true)
  }
  defarneVBNWithSite should "contain a defarne PTM with a site" in {
    val doc = testReach.mkDoc(defarneVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == defarnesylationLabel should be (true)
  }
  defarneVBNIntercedingWithSite should "contain a defarne PTM with a site" in {
    val doc = testReach.mkDoc(defarneVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == defarnesylationLabel should be (true)
  }
  defarneVBNThatIs should "contain a defarne PTM with a site" in {
    val doc = testReach.mkDoc(defarneVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == defarnesylationLabel should be (true)
  }

  deriboJJ should "contain a deribo PTM" in {
    val doc = testReach.mkDoc(deriboJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deribosylationLabel should be (true)
  }
  deriboVBN should "contain a deribo PTM" in {
    val doc = testReach.mkDoc(deriboVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deribosylationLabel should be (true)
  }
  deriboJJWithSite should "contain a deribo PTM with a site" in {
    val doc = testReach.mkDoc(deriboJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deribosylationLabel should be (true)
  }
  deriboJJIntercedingWithSite should "contain a deribo PTM with a site" in {
    val doc = testReach.mkDoc(deriboJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deribosylationLabel should be (true)
  }
  deriboVBNWithSite should "contain a deribo PTM with a site" in {
    val doc = testReach.mkDoc(deriboVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deribosylationLabel should be (true)
  }
  deriboVBNIntercedingWithSite should "contain a deribo PTM with a site" in {
    val doc = testReach.mkDoc(deriboVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deribosylationLabel should be (true)
  }
  deriboVBNThatIs should "contain a deribo PTM with a site" in {
    val doc = testReach.mkDoc(deriboVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deribosylationLabel should be (true)
  }

  dehydroxJJ should "contain a dehydrox PTM" in {
    val doc = testReach.mkDoc(dehydroxJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dehydroxylationLabel should be (true)
  }
  dehydroxVBN should "contain a dehydrox PTM" in {
    val doc = testReach.mkDoc(dehydroxVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dehydroxylationLabel should be (true)
  }
  dehydroxJJWithSite should "contain a dehydrox PTM with a site" in {
    val doc = testReach.mkDoc(dehydroxJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dehydroxylationLabel should be (true)
  }
  dehydroxJJIntercedingWithSite should "contain a dehydrox PTM with a site" in {
    val doc = testReach.mkDoc(dehydroxJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dehydroxylationLabel should be (true)
  }
  dehydroxVBNWithSite should "contain a dehydrox PTM with a site" in {
    val doc = testReach.mkDoc(dehydroxVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dehydroxylationLabel should be (true)
  }
  dehydroxVBNIntercedingWithSite should "contain a dehydrox PTM with a site" in {
    val doc = testReach.mkDoc(dehydroxVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dehydroxylationLabel should be (true)
  }
  dehydroxVBNThatIs should "contain a dehydrox PTM with a site" in {
    val doc = testReach.mkDoc(dehydroxVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == dehydroxylationLabel should be (true)
  }

  aceJJ should "contain a ace PTM" in {
    val doc = testReach.mkDoc(aceJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deacetylationLabel should be (true)
  }
  aceVBN should "contain a ace PTM" in {
    val doc = testReach.mkDoc(aceVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deacetylationLabel should be (true)
  }
  aceJJWithSite should "contain a ace PTM with a site" in {
    val doc = testReach.mkDoc(aceJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deacetylationLabel should be (true)
  }
  aceJJIntercedingWithSite should "contain a ace PTM with a site" in {
    val doc = testReach.mkDoc(aceJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deacetylationLabel should be (true)
  }
  aceVBNWithSite should "contain a ace PTM with a site" in {
    val doc = testReach.mkDoc(aceVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deacetylationLabel should be (true)
  }
  aceVBNIntercedingWithSite should "contain a ace PTM with a site" in {
    val doc = testReach.mkDoc(aceVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deacetylationLabel should be (true)
  }
  aceVBNThatIs should "contain a ace PTM with a site" in {
    val doc = testReach.mkDoc(aceVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deacetylationLabel should be (true)
  }

  deglycoJJ should "contain a deglyco PTM" in {
    val doc = testReach.mkDoc(deglycoJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deglycosylationLabel should be (true)
  }
  deglycoVBN should "contain a deglyco PTM" in {
    val doc = testReach.mkDoc(deglycoVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deglycosylationLabel should be (true)
  }
  deglycoJJWithSite should "contain a deglyco PTM with a site" in {
    val doc = testReach.mkDoc(deglycoJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deglycosylationLabel should be (true)
  }
  deglycoJJIntercedingWithSite should "contain a deglyco PTM with a site" in {
    val doc = testReach.mkDoc(deglycoJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deglycosylationLabel should be (true)
  }
  deglycoVBNWithSite should "contain a deglyco PTM with a site" in {
    val doc = testReach.mkDoc(deglycoVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deglycosylationLabel should be (true)
  }
  deglycoVBNIntercedingWithSite should "contain a deglyco PTM with a site" in {
    val doc = testReach.mkDoc(deglycoVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deglycosylationLabel should be (true)
  }
  deglycoVBNThatIs should "contain a deglyco PTM with a site" in {
    val doc = testReach.mkDoc(deglycoVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deglycosylationLabel should be (true)
  }

  methJJ should "contain a meth PTM" in {
    val doc = testReach.mkDoc(methJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == demethylationLabel should be (true)
  }
  methVBN should "contain a meth PTM" in {
    val doc = testReach.mkDoc(methVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == demethylationLabel should be (true)
  }
  methJJWithSite should "contain a meth PTM with a site" in {
    val doc = testReach.mkDoc(methJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == demethylationLabel should be (true)
  }
  methJJIntercedingWithSite should "contain a meth PTM with a site" in {
    val doc = testReach.mkDoc(methJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == demethylationLabel should be (true)
  }
  methVBNWithSite should "contain a meth PTM with a site" in {
    val doc = testReach.mkDoc(methVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == demethylationLabel should be (true)
  }
  methVBNIntercedingWithSite should "contain a meth PTM with a site" in {
    val doc = testReach.mkDoc(methVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == demethylationLabel should be (true)
  }
  methVBNThatIs should "contain a meth PTM with a site" in {
    val doc = testReach.mkDoc(methVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == demethylationLabel should be (true)
  }

  sumoJJ should "contain a sumo PTM" in {
    val doc = testReach.mkDoc(sumoJJ, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == desumoylationLabel should be (true)
  }
  sumoVBN should "contain a sumo PTM" in {
    val doc = testReach.mkDoc(sumoVBN, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == desumoylationLabel should be (true)
  }
  sumoJJWithSite should "contain a sumo PTM with a site" in {
    val doc = testReach.mkDoc(sumoJJWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == desumoylationLabel should be (true)
  }
  sumoJJIntercedingWithSite should "contain a sumo PTM with a site" in {
    val doc = testReach.mkDoc(sumoJJIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == desumoylationLabel should be (true)
  }
  sumoVBNWithSite should "contain a sumo PTM with a site" in {
    val doc = testReach.mkDoc(sumoVBNWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == desumoylationLabel should be (true)
  }
  sumoVBNIntercedingWithSite should "contain a sumo PTM with a site" in {
    val doc = testReach.mkDoc(sumoVBNIntercedingWithSite, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == desumoylationLabel should be (true)
  }
  sumoVBNThatIs should "contain a sumo PTM with a site" in {
    val doc = testReach.mkDoc(sumoVBNThatIs, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == desumoylationLabel should be (true)
  }

  val sent1 = "The dephosphorylated AKT binds to ASPP2."
  val sent1b = "The deubiquitinated AKT binds to ASPP2."
  "ReachSystem" should "not find a PTMs as events" in {
    var mentions = getBioMentions(sent1)
    val p = mentions.find(_ matches "Dephosphorylation")
    p.isDefined should be (false) // Dane
    var b = mentions.find(_ matches "Binding")
    b.isDefined should be (true) // Marco

    mentions = getBioMentions(sent1b)
    val u = mentions.find(_ matches "Deubiquitination")
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
  val sent3a = "Experiments revealed deubiquitination at Lys residues 104 and 147 of K-Ras"
  s"""K-Ras in "$sent3a"""" should "have 2 EventSites after the modificationEngine" in {
    val mentions =  getEntities(sent3a)
    val p = mentions.filter(_ matches "Gene_or_gene_product")
    p should have size (1)
    // This tests whether the modification is present
    getEventSites(p.head) should have size (2)
  }

  // Test EventSite modifications
  val sent3b = "Experiments revealed deubiquitination at Lys residues 117, 147, and 170 for H-Ras."
  s""""H-Ras in "$sent3b"s""" should "have 3 EventSites after the modificationEngine" in {
    val mentions =  getEntities(sent3b)
    val p = mentions.filter(_ matches "Gene_or_gene_product")
    p should have size (1)
    // This tests whether the modification is present
    getEventSites(p.head) should have size (3)
  }

  val sent4 = "Dephosphorylated Mek binds to GTP."
  sent4 should "not contain a dephosphorylation event (this is a PTM)" in {
    val doc = testReach.mkDoc(sent4, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_ matches "Dephosphorylation") should be (false)
  }

  val sent5 = "Ligation of ASPP2 to dehydroxylated RAS-GTP promotes apoptosis."
  sent5 should "not contain a dehydroxylation event (this is a PTM)" in {
    val doc = testReach.mkDoc(sent5, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_ matches "Dehydroxylation") should be (false)
  }

  val sent6 = "Optineurin regulates NF-kappaB activation by mediating interaction of CYLD with deubiquitinated RIP."
  sent6 should "not contain a deubiquitination event (this is a PTM)" in {
    val doc = testReach.mkDoc(sent6, "testdoc")
    val mentions = testReach extractFrom doc
    mentions.exists(_ matches "Deubiquitination") should be (false)
  }

  val sent7 = "The deubiquitinated Ras protein dephosphorylates AKT."
  // the example text says that Ras is deubiquitinated
  // that should be reflected as a PTM in ras.modifications
  sent7 should "contain a Ras with a PTM" in {
    val doc = testReach.mkDoc(sent7, docId, chunkId)
    val mentions = testReach.extractEntitiesFrom(doc)
    val ras = mentions.find(_.text contains "Ras")
    ras.isDefined should be (true)
    // there is only one PTM in the example text
    ras.get.modifications.size == 1 should be (true)
    val ptm = ras.get.modifications.head
    ptm.label == deubiquitinationLabel should be (true)
  }

  val sent8 = "Ras does not dephosphorylate Mek"
  sent8 should "have a negated positive regulation" in {
    val doc = testReach.mkDoc(sent8, docId, chunkId)
    val mentions = testReach extractFrom doc
    mentions filter (_ matches "Event") should have size (2)
    val phospho = mentions.find(_ matches "Dephosphorylation")
    phospho should be ('defined)
    phospho.get.arguments.keySet should not contain ("cause")
    phospho.get.modifications.filter(_.isInstanceOf[Negation]) should be ('empty)
    val reg = mentions.find(_ matches "Positive_regulation")
    reg should be ('defined)
    reg.get.modifications.filter(_.isInstanceOf[Negation]) should have size (1)
  }

  val sent9 = "The dephosphorylated p53 by ASPP2 is doing something..."
  // this is not a PTM! It is an event with a cause
  sent9 should "contain 1 dephosphorylation and 1 regulation event" in {
    val mentions = getBioMentions(sent9)
    hasEventWithArguments("Dephosphorylation", List("p53"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP2", "Dephosphorylation", List("p53"), mentions) should be (true)
  }

  //
  // a few tests for mutation modifications
  //
  val sent10 = "Note that only K650M and K650E-FGFR3 mutants cause STAT1 dephosphorylation"
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

  val sent11 = "Note that only FGFR3 K650M causes STAT1 dephosphorylation"
  sent11 should "have 1 mutation for FGFR3" in {
    val mentions = getBioMentions(sent11)
    val fgfr = mentions.filter(m => (m.text contains "FGFR3") && m.isInstanceOf[BioTextBoundMention])
    fgfr should have size (1)
    fgfr.head.countMutations should be (1)
  }

  val sent12 = "Note that only the K650M-FGFR3 mutant causes STAT1 dephosphorylation"
  sent12 should "have 1 mutation for FGFR3" in {
    val mentions = getBioMentions(sent12)
    val fgfr = mentions.filter(m => (m.text contains "FGFR3") && m.isInstanceOf[BioTextBoundMention])
    fgfr should have size (1)
    fgfr.head.hasMutation("K650M") should be (true)
  }

  //
  // a few tests for modifications in parens
  //
  val sent14 = "all six FGFR3 mutants induced activatory ERK(T202/Y204) dephosphorylation (Fig. 2)."
  sent14 should "contain 2 dephosphorylations (one for each ERK mutation) and 2 Positive Regulations" in {
    val mentions = getBioMentions(sent14)

    // We have one dephosphorylation per Site
    val phosphos = mentions.filter(_ matches "Dephosphorylation")
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

  val sent15 = "all six FGFR3 mutants induced activatory ERK(K156M/H204M) dephosphorylation (Fig. 2)."
  sent15 should "contain 2 Positive Regulations NOT Activations (1 for each ERK mutation)" in {
    val mentions = getBioMentions(sent15)

    // We have 1 Reg per ERK mutant
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (2)
  }

  val sent16 = "all six FGFR3 mutants induced activatory ERK(K156M, H204M) dephosphorylation (Fig. 2)."
  sent16 should "contain 2 Positive Regulations NOT Activations (1 for each ERK mutation)" in {
    val mentions = getBioMentions(sent16)

    // We have 1 Reg per ERK mutant
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (2)

  }

  val siteTest1 = "activatory ERK(T202/Y204) dephosphorylation (Fig. 2)."
  siteTest1 should "contain 2 sites (distinct dephosphorylations)" in {
    val mentions = getBioMentions(siteTest1)

    // We have one dephosphorylation per Site
    val phosphos = mentions.filter(_ matches "Dephosphorylation")
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

  val mutantTest1 = "all six FGFR3 mutants induced activatory ERK(K156M/H204M) dephosphorylation (Fig. 2)."
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

  val mutantTest2 = "all six FGFR3 mutants induced activatory ERK(K156M, H204M) dephosphorylation (Fig. 2)."
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

  val mutantTest12 = "K111M, K112M, and K113M mutants of ASPP1 and the RAS complex are dephosphorylated."
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

  val mutantTest14 = "K111M, K112M, and K113M mutations of ASPP1 and the RAS complex are dephosphorylated."
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
    mentions.head hasMutation ("DeltaF508", "DeletionMutant") should be (true)
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

  val siteTest3 = "Dephosphorylation (p) of Akt (Ser-473), mTOR (Ser 2448) and Rictor (Ser 792) was quantified."
  siteTest2 should "contain 3 Sites" in {
    val mentions = getBioMentions(siteTest3)
    val sites = mentions.filter(_ matches "Site")
    sites should have size (3)
    sites.exists(_.text contains "Ser-473") should be (true)
  }

  val siteTest4 = "Dephosphorylation of Akt (S473M) was attenuated."
  siteTest4 should "not contain any sites" in {
    val mentions = getBioMentions(siteTest4)
    val sites = mentions.filter(_ matches "Site")
    sites should have size (0)
    val akt = mentions filter (_.text == "Akt")
    akt should have size (1)
    akt.head.countMutations should be (1)
  }
}
