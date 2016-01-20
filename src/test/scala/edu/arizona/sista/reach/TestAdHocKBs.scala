package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.grounding._

/**
  * Unit tests to ensure a mixed-namespace in-memory KB is working for grounding.
  *   Written by: Tom Hicks. 1/20/2016.
  *   Last Modified: Initial creation: not all tests pass yet.
  */
class TestAdHocKBs extends FlatSpec with Matchers {

  // Tests of 3-column KB without specific species, so human assumed:
  val ahkb3 = new AdHocIMKBFactory("adhoc.tsv").make()

  "AdHocKB COL-3" should "lookupAll on AHKB from COL-3 TSV file" in {
    (ahkb3.lookupAll("NOT-IN-KB").isDefined) should be (false) // not in KB
    (ahkb3.lookupAll("not-in-kb").isDefined) should be (false) // not in KB
    (ahkb3.lookupAll("TROP2").isDefined) should be (false)  // uppercase
    (ahkb3.lookupAll("Trop2").isDefined) should be (false)  // mixed case
    (ahkb3.lookupAll("trop2").isDefined) should be (true)
    (ahkb3.lookupAll("apoptosis").isDefined) should be (true)
    (ahkb3.lookupAll("nadph").isDefined) should be (true)
    (ahkb3.lookupAll("ros").isDefined) should be (true)
  }

  "AdHocKB COL-3" should "lookupAny on AHKB from COL-3 TSV file" in {
    (ahkb3.lookupAny("NOT-IN-KB").isDefined) should be (false) // not in KB
    (ahkb3.lookupAny("not-in-kb").isDefined) should be (false) // not in KB
    (ahkb3.lookupAny("TROP2").isDefined) should be (false)  // uppercase
    (ahkb3.lookupAny("Trop2").isDefined) should be (false)  // mixed case
    (ahkb3.lookupAny("trop2").isDefined) should be (true)
    (ahkb3.lookupAny("apoptosis").isDefined) should be (true)
    (ahkb3.lookupAny("nadph").isDefined) should be (true)
    (ahkb3.lookupAny("ros").isDefined) should be (true)
  }

  "AdHocKB COL-3" should "lookupByASpecies on AHKB from COL-3 TSV file" in {
    (ahkb3.lookupByASpecies("NOT-IN-KB", "human").isDefined) should be (false)
    (ahkb3.lookupByASpecies("not-in-kb", "human").isDefined) should be (false)
    (ahkb3.lookupByASpecies("TROP2", "human").isDefined) should be (false) // uppercase
    (ahkb3.lookupByASpecies("Trop2", "human").isDefined) should be (false) // mixed case
    (ahkb3.lookupByASpecies("trop2", "mouse").isDefined) should be (false)
    (ahkb3.lookupByASpecies("trop2", "human").isDefined) should be (true)
    (ahkb3.lookupByASpecies("apoptosis", "human").isDefined) should be (true)
    (ahkb3.lookupByASpecies("nadph", "human").isDefined) should be (true)
    (ahkb3.lookupByASpecies("ros", "human").isDefined) should be (true)
  }

  "AdHocKB COL-3" should "lookupBySpecies on AHKB from COL-3 gzipped TSV file" in {
    (ahkb3.lookupBySpecies("NOT-IN-KB", Set("human")).isDefined) should be (false)
    (ahkb3.lookupBySpecies("not-in-kb", Set("human")).isDefined) should be (false)
    (ahkb3.lookupBySpecies("trop2", Set("aardvark")).isDefined) should be (false)
    (ahkb3.lookupBySpecies("TROP2", Set("human","mouse","gorilla")).isDefined) should be (false)
    (ahkb3.lookupBySpecies("trop2", Set("human")).isDefined) should be (true)
    (ahkb3.lookupBySpecies("trop2", Set("human", "mouse")).isDefined) should be (true)
    (ahkb3.lookupBySpecies("apoptosis", Set("human", "mouse")).isDefined) should be (true)
    (ahkb3.lookupBySpecies("nadph", Set("human", "mouse")).isDefined) should be (true)
    (ahkb3.lookupBySpecies("ros", Set("human", "mouse")).isDefined) should be (true)
  }

  "AdHocKB COL-3" should "lookupHuman on AHKB from COL-3 TSV file" in {
    (ahkb3.lookupHuman("NOT-IN-KB").isDefined) should be (false) // not in KB
    (ahkb3.lookupHuman("not-in-kb").isDefined) should be (false) // not in KB
    (ahkb3.lookupHuman("TROP2").isDefined) should be (false)  // uppercase
    (ahkb3.lookupHuman("trop2").isDefined) should be (true)
    (ahkb3.lookupHuman("apoptosis").isDefined) should be (true)
    (ahkb3.lookupHuman("nadph").isDefined) should be (true)
    (ahkb3.lookupHuman("ros").isDefined) should be (true)
  }

  "AdHocKB COL-3" should "fail lookupNoSpecies on AHKB from COL-3 TSV file" in {
    (ahkb3.lookupNoSpecies("NOT-IN-KB").isDefined) should be (false) // not in KB
    (ahkb3.lookupNoSpecies("not-in-kb").isDefined) should be (false) // not in KB
    (ahkb3.lookupNoSpecies("TROP2").isDefined) should be (false)  // uppercase
    (ahkb3.lookupNoSpecies("trop2").isDefined) should be (false)  // assumed human
    (ahkb3.lookupNoSpecies("apoptosis").isDefined) should be (false) // assumed human
    (ahkb3.lookupNoSpecies("nadph").isDefined) should be (false)     // assumed human
    (ahkb3.lookupNoSpecies("ros").isDefined) should be (false)       // assumed human
  }


  // Tests of speciated (3-column) knowledge base
  val ahkb4 = new AdHocIMKBFactory("adhoc.tsv").make()

  // test lookups directly in AHKB (remember: all test keys must be lowercased to succeed!)

  "AdHocKB COL-4" should "lookupAll on AHKB from COL-4 gzipped TSV file" in {
    (ahkb4.lookupAll("NOT-IN-KB").isDefined) should be (false) // not in KB
    (ahkb4.lookupAll("PTHR21244").isDefined) should be (false) // uppercase
    (ahkb4.lookupAll("pthr21244").isDefined) should be (true)
    (ahkb4.lookupAll("hk").isDefined) should be (true)
  }

  "AdHocKB COL-4" should "lookupAny on AHKB from COL-4 gzipped TSV file" in {
    (ahkb4.lookupAny("NOT-IN-KB").isDefined) should be (false) // not in KB
    (ahkb4.lookupAny("PTHR21244").isDefined) should be (false) // uppercase
    (ahkb4.lookupAny("pthr21244").isDefined) should be (true)
    (ahkb4.lookupAny("hk").isDefined) should be (true)
  }

  "AdHocKB COL-4" should "lookupByASpecies on AHKB from COL-4 gzipped TSV file" in {
    (ahkb4.lookupByASpecies("NOT-IN-KB", "human").isDefined) should be (false) // not in KB
    (ahkb4.lookupByASpecies("PTHR21244", "human").isDefined) should be (false) // uppercase
    (ahkb4.lookupByASpecies("pthr21244", "human").isDefined) should be (true)
    (ahkb4.lookupByASpecies("pthr21244", "HUMAN").isDefined) should be (true)
    (ahkb4.lookupByASpecies("pthr21244", "mouse").isDefined) should be (true)
    (ahkb4.lookupByASpecies("pthr21244", "aardvark").isDefined) should be (false) // not in KB
    (ahkb4.lookupByASpecies("hk", "saccharomyces cerevisiae").isDefined) should be (true)
  }

  "AdHocKB COL-4" should "lookupBySpecies on AHKB from COL-4 gzipped TSV file" in {
    (ahkb4.lookupBySpecies("pthr21244", Set("aardvark")).isDefined) should be (false)
    (ahkb4.lookupBySpecies("pthr21244", Set("human")).isDefined) should be (true)
    val pt = ahkb4.lookupBySpecies("pthr21244", Set("human", "mouse"))
    (pt.isDefined) should be (true)
    (pt.get.size == 2) should be (true)
    (ahkb4.lookupBySpecies("pthr21244", Set("human","mouse","gorilla")).isDefined) should be (true)
    val pt2 = (ahkb4.lookupBySpecies("pthr21244", Set("human", "mouse","gorilla")))
    (pt2.isDefined) should be (true)
    (pt2.get.size == 2) should be (true)
    (ahkb4.lookupBySpecies("hk", Set("human", "mouse")).isDefined) should be (false)
    (ahkb4.lookupBySpecies("hk", Set("saccharomyces cerevisiae", "ant")).isDefined) should be (true)
    (ahkb4.lookupBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).isDefined) should be (true)
    (ahkb4.lookupBySpecies(
      "hk", Set("ant", "saccharomyces cerevisiae")).get.size == 1) should be (true)
  }

  "AdHocKB COL-4" should "lookupHuman on AHKB from COL-4 gzipped TSV file" in {
    (ahkb4.lookupHuman("hk").isDefined) should be (false) // yeast only
    (ahkb4.lookupHuman("PTHR21244").isDefined) should be (false)
    (ahkb4.lookupHuman("pthr21244").isDefined) should be (true)
  }

  "AdHocKB COL-4" should "lookupNoSpecies on AHKB from COL-4 gzipped TSV file" in {
    (ahkb4.lookupNoSpecies("NOT-IN-KB").isDefined) should be (false) // not in KB
    (ahkb4.lookupNoSpecies("not-in-kb").isDefined) should be (false) // not in KB
    (ahkb4.lookupNoSpecies("PTHR21244").isDefined) should be (false) // uppercase
    (ahkb4.lookupNoSpecies("pthr21244").isDefined) should be (false) // has a species
    (ahkb4.lookupNoSpecies("hk").isDefined) should be (false)        // has a species
  }

}
