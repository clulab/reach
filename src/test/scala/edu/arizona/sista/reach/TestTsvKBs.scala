package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.grounding._

/**
  * Unit tests to ensure the in-memory KB is working for grounding.
  *   Written by: Tom Hicks. 10/26/2015.
  *   Last Modified: Update for returned resolution sequences. Fix test KB namespaces.
  */
class TestTsvKBs extends FlatSpec with Matchers {

  // Tests of non-speciated (2-column) knowledge base
  val imkb2 = new TsvIMKBFactory("uniprot", "uniprot-subcellular-locations.tsv",
    new IMKBMetaInfo("http://identifiers.org/uazclu/", "MIR:00000000")).make()

  "InMemoryKB COL-2" should "lookupAll on IMKB from COL-2 TSV file" in {
    (imkb2.lookupAll("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkb2.lookupAll("not-in-kb").isDefined) should be (false) // not in KB
    (imkb2.lookupAll("DENDRITE").isDefined) should be (false)  // uppercase
    (imkb2.lookupAll("dendrite").isDefined) should be (true)
    (imkb2.lookupAll("telomere").isDefined) should be (true)
  }

  "InMemoryKB COL-2" should "lookupAny on IMKB from COL-2 TSV file" in {
    (imkb2.lookupAny("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkb2.lookupAny("not-in-kb").isDefined) should be (false) // not in KB
    (imkb2.lookupAny("DENDRITE").isDefined) should be (false)  // uppercase
    (imkb2.lookupAny("dendrite").isDefined) should be (true)
    (imkb2.lookupAny("telomere").isDefined) should be (true)
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
    (imkb2.lookupHuman("DENDRITE").isDefined) should be (false)  // uppercase
    (imkb2.lookupHuman("dendrite").isDefined) should be (false)  // KB has no species
    (imkb2.lookupHuman("telomere").isDefined) should be (false)  // KB has no species
  }

  "InMemoryKB COL-2" should "lookupNoSpecies on IMKB from COL-2 TSV file" in {
    (imkb2.lookupNoSpecies("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkb2.lookupNoSpecies("not-in-kb").isDefined) should be (false) // not in KB
    (imkb2.lookupNoSpecies("DENDRITE").isDefined) should be (false)  // uppercase
    (imkb2.lookupNoSpecies("dendrite").isDefined) should be (true)
    (imkb2.lookupNoSpecies("telomere").isDefined) should be (true)
  }


  // Tests of speciated (3-column) knowledge base
  val imkbPF = new TsvIMKBFactory("interpro", "ProteinFamilies.tsv.gz", true,
    new IMKBMetaInfo("http://identifiers.org/uazclu/", "MIR:00000000")).make()

  // test lookups directly in IMKB (remember: all test keys must be lowercased to succeed!)

  "InMemoryKB COL-3" should "lookupAll on IMKB from COL-3 gzipped TSV file" in {
    (imkbPF.lookupAll("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkbPF.lookupAll("PTHR21244").isDefined) should be (false) // uppercase
    (imkbPF.lookupAll("pthr21244").isDefined) should be (true)
    (imkbPF.lookupAll("hk").isDefined) should be (true)
  }

  "InMemoryKB COL-3" should "lookupAny on IMKB from COL-3 gzipped TSV file" in {
    (imkbPF.lookupAny("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkbPF.lookupAny("PTHR21244").isDefined) should be (false) // uppercase
    (imkbPF.lookupAny("pthr21244").isDefined) should be (true)
    (imkbPF.lookupAny("hk").isDefined) should be (true)
  }

  "InMemoryKB COL-3" should "lookupByASpecies on IMKB from COL-3 gzipped TSV file" in {
    (imkbPF.lookupByASpecies("NOT-IN-KB", "human").isDefined) should be (false) // not in KB
    (imkbPF.lookupByASpecies("PTHR21244", "human").isDefined) should be (false) // uppercase
    (imkbPF.lookupByASpecies("pthr21244", "HUMAN").isDefined) should be (false) // species uppercase
    (imkbPF.lookupByASpecies("pthr21244", "human").isDefined) should be (true)
    (imkbPF.lookupByASpecies("pthr21244", "mouse").isDefined) should be (true)
    (imkbPF.lookupByASpecies("pthr21244", "aardvark").isDefined) should be (false) // not in KB
    (imkbPF.lookupByASpecies("hk", "saccharomyces cerevisiae").isDefined) should be (true)
  }

  "InMemoryKB COL-3" should "lookupBySpecies on IMKB from COL-3 gzipped TSV file" in {
    (imkbPF.lookupBySpecies("pthr21244", Set("aardvark")).isDefined) should be (false)
    (imkbPF.lookupBySpecies("pthr21244", Set("human")).isDefined) should be (true)
    val pt = imkbPF.lookupBySpecies("pthr21244", Set("human", "mouse"))
    (pt.isDefined) should be (true)
    (pt.get.size == 2) should be (true)
    (imkbPF.lookupBySpecies("pthr21244", Set("human","mouse","gorilla")).isDefined) should be (true)
    val pt2 = (imkbPF.lookupBySpecies("pthr21244", Set("human", "mouse","gorilla")))
    (pt2.isDefined) should be (true)
    (pt2.get.size == 2) should be (true)
    (imkbPF.lookupBySpecies("hk", Set("human", "mouse")).isDefined) should be (false)
    (imkbPF.lookupBySpecies("hk", Set("saccharomyces cerevisiae", "ant")).isDefined) should be (true)
    (imkbPF.lookupBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).isDefined) should be (true)
    (imkbPF.lookupBySpecies(
      "hk", Set("ant", "saccharomyces cerevisiae")).get.size == 1) should be (true)
  }

  "InMemoryKB COL-3" should "lookupHuman on IMKB from COL-3 gzipped TSV file" in {
    (imkbPF.lookupHuman("hk").isDefined) should be (false) // yeast only
    (imkbPF.lookupHuman("PTHR21244").isDefined) should be (false)
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
