package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import org.clulab.reach.grounding._

/**
  * Unit tests to ensure a mixed-namespace in-memory KB is working for grounding.
  *   Written by: Tom Hicks. 1/20/2016.
  *   Last Modified: Update for IMKB rewrite.
  */
class TestAdHocIMKBs extends FlatSpec with Matchers {

  // Tests of 3-column KB without specific species, so human assumed:
  val meta = new IMKBMetaInfo(kbFilename = Some("NER-Grounding-Override.tsv"))
  val ahkb3 = (new AdHocIMKBFactory).make(meta)

  "AdHocKB COL-3" should "lookup on AHKB from COL-3 TSV file" in {
    (ahkb3.lookup("NOT-IN-KB").isDefined) should be (false) // not in KB
    (ahkb3.lookup("not-in-kb").isDefined) should be (false) // not in KB
    (ahkb3.lookup("TROP2").isDefined) should be (true)
    (ahkb3.lookup("Trop2").isDefined) should be (true)
    (ahkb3.lookup("trop2").isDefined) should be (true)
    (ahkb3.lookup("nadph").isDefined) should be (true)
    (ahkb3.lookup("ros").isDefined) should be (true)
  }

  "AdHocKB COL-3" should "lookupByASpecies on AHKB from COL-3 TSV file" in {
    (ahkb3.lookupByASpecies("NOT-IN-KB", "human").isDefined) should be (false)
    (ahkb3.lookupByASpecies("not-in-kb", "human").isDefined) should be (false)
    (ahkb3.lookupByASpecies("trop2", "mouse").isDefined) should be (false)
    (ahkb3.lookupByASpecies("TROP2", "human").isDefined) should be (true)
    (ahkb3.lookupByASpecies("Trop2", "human").isDefined) should be (true)
    (ahkb3.lookupByASpecies("trop2", "human").isDefined) should be (true)
    (ahkb3.lookupByASpecies("nadph", "human").isDefined) should be (true)
    (ahkb3.lookupByASpecies("ros", "human").isDefined) should be (true)
  }

  "AdHocKB COL-3" should "lookupBySpecies on AHKB from COL-3 gzipped TSV file" in {
    (ahkb3.lookupBySpecies("NOT-IN-KB", Set("human")).isDefined) should be (false)
    (ahkb3.lookupBySpecies("not-in-kb", Set("human")).isDefined) should be (false)
    (ahkb3.lookupBySpecies("trop2", Set("aardvark")).isDefined) should be (false)
    (ahkb3.lookupBySpecies("trop2", Set("aardvark", "zebra")).isDefined) should be (false)
    (ahkb3.lookupBySpecies("trop2", Set("human")).isDefined) should be (true)
    (ahkb3.lookupBySpecies("trop2", Set("human", "mouse")).isDefined) should be (true)
    (ahkb3.lookupBySpecies("nadph", Set("human", "mouse")).isDefined) should be (true)
    (ahkb3.lookupBySpecies("ros", Set("human", "mouse")).isDefined) should be (true)
    (ahkb3.lookupBySpecies("TROP2", Set("human","mouse","gorilla")).isDefined) should be (true)
  }

  "AdHocKB COL-3" should "lookupHuman on AHKB from COL-3 TSV file" in {
    (ahkb3.lookupHuman("NOT-IN-KB").isDefined) should be (false) // not in KB
    (ahkb3.lookupHuman("not-in-kb").isDefined) should be (false) // not in KB
    (ahkb3.lookupHuman("TROP2").isDefined) should be (true)
    (ahkb3.lookupHuman("trop2").isDefined) should be (true)
    (ahkb3.lookupHuman("nadph").isDefined) should be (true)
    (ahkb3.lookupHuman("ros").isDefined) should be (true)
  }

  "AdHocKB COL-3" should "fail lookupNoSpecies on AHKB from COL-3 TSV file" in {
    (ahkb3.lookupNoSpecies("NOT-IN-KB").isDefined) should be (false) // not in KB
    (ahkb3.lookupNoSpecies("not-in-kb").isDefined) should be (false) // not in KB
    (ahkb3.lookupNoSpecies("TROP2").isDefined) should be (false)     // assumed human
    (ahkb3.lookupNoSpecies("trop2").isDefined) should be (false)     // assumed human
    (ahkb3.lookupNoSpecies("nadph").isDefined) should be (false)     // assumed human
    (ahkb3.lookupNoSpecies("ros").isDefined) should be (false)       // assumed human
  }

}
