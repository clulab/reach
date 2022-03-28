package org.clulab.reach

import org.scalatest.{FlatSpec, Matchers}
import TestUtils._
import com.typesafe.config.ConfigFactory
import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBConstants._

/**
  * Unit tests to ensure the in-memory KB is working for grounding.
  *   Written by: Tom Hicks. 10/26/2015.
  *   Last Modified: Update for IMKB rewrite.
  */
class TestTsvKBs extends FlatSpec with Matchers {

  // Tests of non-speciated (2-column) knowledge base
  val imkb2 = (new CellLocKBL).memoryKB     // defined after this class (LOOK BELOW)
  // imkb2.dump                                // DEBUGGING

  "InMemoryKB COL-2" should "lookup on IMKB from COL-2 TSV file" in {
    (imkb2.lookup("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkb2.lookup("not-in-kb").isDefined) should be (false) // not in KB
    (imkb2.lookup("DENDRITE").isDefined) should be (true)
    (imkb2.lookup("dendrite").isDefined) should be (true)
    (imkb2.lookup("telomere").isDefined) should be (true)
  }

  "InMemoryKB COL-2" should "fail to lookupByASpecies on IMKB from COL-2 TSV file" in {
    (imkb2.lookupByASpecies("NOT-IN-KB", "human").isDefined) should be (false)
    (imkb2.lookupByASpecies("DENDRITE", "human").isDefined) should be (false)
    (imkb2.lookupByASpecies("dendrite", "human").isDefined) should be (false)
    (imkb2.lookupByASpecies("dendrite", "mouse").isDefined) should be (false)
    (imkb2.lookupByASpecies("telomere", "human").isDefined) should be (false)
  }

  "InMemoryKB COL-2" should "fail to lookupBySpecies on IMKB from COL-2 gzipped TSV file" in {
    (imkb2.lookupBySpecies("dendrite", Set("aardvark")).isDefined) should be (false)
    (imkb2.lookupBySpecies("dendrite", Set("human")).isDefined) should be (false)
    (imkb2.lookupBySpecies("dendrite", Set("human", "mouse")).isDefined) should be (false)
    (imkb2.lookupBySpecies("DENDRITE", Set("human","mouse","gorilla")).isDefined) should be (false)
    (imkb2.lookupBySpecies("telomere", Set("human", "mouse")).isDefined) should be (false)
  }

  "InMemoryKB COL-2" should "fail to lookupHuman on IMKB from COL-2 TSV file" in {
    (imkb2.lookupHuman("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkb2.lookupHuman("not-in-kb").isDefined) should be (false) // not in KB
    (imkb2.lookupHuman("DENDRITE").isDefined) should be (false)  // KB has no species
    (imkb2.lookupHuman("dendrite").isDefined) should be (false)  // KB has no species
    (imkb2.lookupHuman("telomere").isDefined) should be (false)  // KB has no species
  }

  "InMemoryKB COL-2" should "lookupNoSpecies on IMKB from COL-2 TSV file" in {
    (imkb2.lookupNoSpecies("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkb2.lookupNoSpecies("not-in-kb").isDefined) should be (false) // not in KB
    (imkb2.lookupNoSpecies("DENDRITE").isDefined) should be (true)
    (imkb2.lookupNoSpecies("dendrite").isDefined) should be (true)
    (imkb2.lookupNoSpecies("telomere").isDefined) should be (true)
  }


  // Tests of speciated (3-column) knowledge base
  val imkbPF = (new ProtFamKBL).memoryKB // defined after this class (LOOK BELOW)
  // imkbPF.dump                               // DEBUGGING

  // tests lookups directly in IMKB
  "InMemoryKB COL-3" should "lookup on IMKB from COL-3 gzipped TSV file" in {
    (imkbPF.lookup("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkbPF.lookup("PTHR21244").isDefined) should be (true)
    (imkbPF.lookup("pthr21244").isDefined) should be (true)
    (imkbPF.lookup("hk").isDefined) should be (true)
  }

  "InMemoryKB COL-3" should "lookupByASpecies on IMKB from COL-3 gzipped TSV file" in {
    (imkbPF.lookupByASpecies("NOT-IN-KB", "human").isDefined) should be (false) // not in KB
    (imkbPF.lookupByASpecies("pthr21244", "aardvark").isDefined) should be (false) // not in KB
    (imkbPF.lookupByASpecies("pthr21244", "HUMAN").isDefined) should be (false) // species uppercase
    (imkbPF.lookupByASpecies("PTHR21244", "human").isDefined) should be (true)
    (imkbPF.lookupByASpecies("pthr21244", "human").isDefined) should be (true)
    (imkbPF.lookupByASpecies("pthr21244", "mouse").isDefined) should be (true)
    (imkbPF.lookupByASpecies("hk", "saccharomyces cerevisiae").isDefined) should be (true)
  }

  "InMemoryKB COL-3" should "lookupBySpecies on IMKB from COL-3 gzipped TSV file" in {
    (imkbPF.lookupBySpecies("pthr21244", Set("aardvark")).isDefined) should be (false)
    (imkbPF.lookupBySpecies("pthr21244", Set("human")).isDefined) should be (true)
    val pt = imkbPF.lookupBySpecies("pthr21244", Set("human", "mouse"))
    (pt.isDefined) should be (true)
    (pt.get.size) should be (4)
    (imkbPF.lookupBySpecies("pthr21244", Set("human","mouse","gorilla")).isDefined) should be (true)
    val pt2 = (imkbPF.lookupBySpecies("pthr21244", Set("human", "mouse","gorilla")))
    (pt2.isDefined) should be (true)
    (pt2.get.size) should be (4)
    (imkbPF.lookupBySpecies("hk", Set("human", "mouse")).isDefined) should be (false)
    (imkbPF.lookupBySpecies("hk", Set("saccharomyces cerevisiae", "ant")).isDefined) should be (true)
    (imkbPF.lookupBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).isDefined) should be (true)
    (imkbPF.lookupBySpecies(
      "hk", Set("ant", "saccharomyces cerevisiae")).get.size) should be (5)
  }

  "InMemoryKB COL-3" should "lookupHuman on IMKB from COL-3 gzipped TSV file" in {
    (imkbPF.lookupHuman("hk").isDefined) should be (false) // yeast only
    (imkbPF.lookupHuman("PTHR21244").isDefined) should be (true)
    (imkbPF.lookupHuman("pthr21244").isDefined) should be (true)
  }

  "InMemoryKB COL-3" should "lookupNoSpecies on IMKB from COL-3 gzipped TSV file" in {
    (imkbPF.lookupNoSpecies("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkbPF.lookupNoSpecies("not-in-kb").isDefined) should be (false) // not in KB
    (imkbPF.lookupNoSpecies("PTHR21244").isDefined) should be (false) // uppercase
    (imkbPF.lookupNoSpecies("pthr21244").isDefined) should be (false) // has a species
    (imkbPF.lookupNoSpecies("hk").isDefined) should be (false)        // has a species
  }

}

class CellLocKBL extends IMKBLookup {

  private val conf = ConfigFactory.load()
  private val path = conf.getString("KnowledgeBases.StaticCellLocation2.path")

  val meta = new IMKBMetaInfo(
//    kbFilename = Some(StaticCellLocation2Filename),
    kbFilename = Some(path),
    namespace = "uniprot",
    baseURI = "http://identifiers.org/uazclu/",
    resourceId = "MIR:00000000"
  )
  memoryKB = (new TsvIMKBFactory).make(meta)
}

class ProtFamKBL extends IMKBLookup {

  private val conf = ConfigFactory.load()
  private val path = conf.getString("KnowledgeBases.StaticProteinFamily2.path")

  val meta = new IMKBMetaInfo(
    kbFilename = Some(path),
    namespace = "interpro",
    baseURI = "http://identifiers.org/uazclu/",
    resourceId = "MIR:00000000",
    hasSpeciesInfo = true,
    isFamilyKB = true
  )
  memoryKB = (new TsvIMKBFactory).make(meta)
}
