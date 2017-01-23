package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests of InMemoryKBs.
  *   Written by: Tom Hicks. 1/22/2017.
  *   Last Modified: Initial creation.
  */
class TestIMKB extends FlatSpec with Matchers {
  val eFactory = new EmptyIMKBFactory

  // create empty IMKB with several types of storage key transforms:
  val multiKTs = Seq( identityKT _, lowercaseKT _, canonicalKT _)
  val ktConfig = new IMKBKeyTransforms(multiKTs, multiKTs)
  val eImkb = eFactory.makeIMKB(ktConfig)        // configure new IMKB (factory below)

  // create empty IMKB with one canonical key transform:
  val canonKTs = Seq( canonicalKT _)
  val ktConfig2 = new IMKBKeyTransforms(canonKTs, canonKTs)
  val eImkb2 = eFactory.makeIMKB(ktConfig2)       // configure new IMKB (factory below)

  "Empty KB" should "not be able to lookup anything while empty" in {
    (eImkb.lookup("")) should be (empty)
    (eImkb.lookup("abc")) should be (empty)
    (eImkb.lookup("ABC")) should be (empty)
    (eImkb.lookup("A/B-C DE")) should be (empty)
  }

  "Empty KB" should "add unique keys only" in {
    eImkb.addEntries("abc", "tst1", "abc1")
    (eImkb.keys.size) should be (1)
    (eImkb.nsIds.size) should be (1)
    eImkb.addEntries("abc", "tst1", "abc1")
    (eImkb.keys.size) should be (1)         // same add should be idempotent
    (eImkb.nsIds.size) should be (1)        // same nsId should not increment key count
    eImkb.addEntries("ABC", "tst1", "abc1")
    (eImkb.keys.size) should be (2)         // should have only added one more
    (eImkb.nsIds.size) should be (1)        // same nsId should not increment key count
    eImkb.addEntries("XYZ", "tst1", "abc1")
    (eImkb.keys.size) should be (4)         // should have added two more entries
    (eImkb.nsIds.size) should be (1)        // same nsId should not increment key count
    eImkb.addEntries("abc", "tst1", "abc2")
    (eImkb.keys.size) should be (4)         // same key should not increment key count
    (eImkb.nsIds.size) should be (2)        // new nsId should have added one more key
    eImkb.addEntries("ABC", "tst1", "abc2")
    (eImkb.keys.size) should be (4)         // same key should not increment key count
    (eImkb.nsIds.size) should be (2)        // same nsId should not increment key count
    eImkb.addEntries("EntryKey", "tst2", "abc2")
    (eImkb.keys.size) should be (6)         // new key should increment key count
    (eImkb.nsIds.size) should be (3)        // new nsId should increment key count
    // eImkb.dump                              // DEBUGGING
  }

  "Empty KB" should "be able to resolve lookups based on previous filling" in {
    // // eImkb.dump                              // DEBUGGING
    // (eImkb.lookup("")) should be (empty)
    // (eImkb.lookup("abc").size) should be (3)
    // (eImkb.lookup("ABC").size) should be (3)
    // (eImkb.lookup("xyz").size) should be (2)
    // (eImkb.lookup("XYZ").size) should be (2)
    // (eImkb.lookup("EntryKey").size) should be (1)
    // (eImkb.lookup("entrykey").size) should be (1)
    // (eImkb.lookup("Entry Key").size) should be (1)
    // (eImkb.lookup("entry key").size) should be (1)
    // (eImkb.lookup("Entry-Key").size) should be (1)
    // (eImkb.lookup("entry-key").size) should be (1)
    // (eImkb.lookup("A/B-C DE")) should be (empty)
  }

  "Empty KB 2" should "add unique keys only" in {
    eImkb2.addEntries("abc", "Key", "Value")
    (eImkb2.keys.size) should be (1)
    (eImkb2.nsIds.size) should be (1)
    eImkb2.addEntries("ABC", "Key", "Value")
    (eImkb2.keys.size) should be (1)        // key should not increment key count
    (eImkb2.nsIds.size) should be (1)       // same nsId should not increment key count
    eImkb2.addEntries("a b c", "Key", "Value")
    (eImkb2.keys.size) should be (1)        // key should not increment key count
    (eImkb2.nsIds.size) should be (1)       // same nsId should not increment key count
    eImkb2.addEntries("A B C", "Key", "Value")
    (eImkb2.keys.size) should be (1)        // key should not increment key count
    (eImkb2.nsIds.size) should be (1)       // same nsId should not increment key count
    eImkb2.addEntries("A-b-C", "Key", "Value")
    (eImkb2.keys.size) should be (1)        // key should not increment key count
    (eImkb2.nsIds.size) should be (1)       // same nsId should not increment key count
    eImkb2.addEntries("A/b-C", "Key", "Value")
    (eImkb2.keys.size) should be (1)        // key should not increment key count
    (eImkb2.nsIds.size) should be (1)       // same nsId should not increment key count
    eImkb2.addEntries("a/b/c", "Key", "Value")
    (eImkb2.keys.size) should be (1)        // key should not increment key count
    (eImkb2.nsIds.size) should be (1)       // same nsId should not increment key count
    // eImkb2.dump                             // DEBUGGING
  }

  "Empty KB 2" should "be able to resolve lookups based on previous filling" in {
    // // eImkb2.dump                              // DEBUGGING
    // (eImkb2.lookup("")) should be (empty)
    // (eImkb2.lookup("abc").size) should be (1)
    // (eImkb2.lookup("ABC").size) should be (1)
    // (eImkb2.lookup("a b c").size) should be (1)
    // (eImkb2.lookup("A B C").size) should be (1)
    // (eImkb2.lookup("A-b-C").size) should be (1)
    // (eImkb2.lookup("A/b-C").size) should be (1)
    // (eImkb2.lookup("a/b/c").size) should be (1)
    // (eImkb2.lookup("A/B-C DE")) should be (empty)
  }
}


class EmptyIMKBFactory {
  def makeIMKB(keyTransforms: IMKBKeyTransforms): InMemoryKB = {
    val metaInfo = new IMKBMetaInfo(kbFilename=None, namespace="tst", baseURI="http://test.com")
    new InMemoryKB(metaInfo, keyTransforms)
  }
}
