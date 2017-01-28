package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests of InMemoryKBs.
  *   Written by: Tom Hicks. 1/22/2017.
  *   Last Modified: Update for rename of KB key transforms group.
  */
class TestIMKB extends FlatSpec with Matchers {
  val eFactory = new EmptyIMKBFactory

  // create empty IMKB with one canonical key transform:
  val canonKTs = Seq( canonicalKT _)
  val ktConfig1 = new KBKeyTransformsGroup(canonKTs, canonKTs)
  val eImkb1 = eFactory.makeIMKB(ktConfig1) // configure new IMKB (factory below)

  // create empty IMKB with several types of storage key transforms:
  val multiKTs = Seq( identityKT _, lowercaseKT _, canonicalKT _)
  val ktConfig2 = new KBKeyTransformsGroup(multiKTs, multiKTs)
  val eImkb2 = eFactory.makeIMKB(ktConfig2) // configure new IMKB (factory below)

  "Empty KB" should "not be able to lookup anything while empty" in {
    (eImkb1.lookup("")) should be (empty)
    (eImkb1.lookup("abc")) should be (empty)
    (eImkb1.lookup("ABC")) should be (empty)
    (eImkb1.lookup("a b c")) should be (empty)
    (eImkb1.lookup("A B C")) should be (empty)
    (eImkb1.lookup("a/b/c")) should be (empty)
    (eImkb1.lookup("A/B-C DE")) should be (empty)
  }

  "Empty KB 1" should "add unique keys only" in {
    eImkb1.addEntries("abc", "ns1", "A-Value")
    (eImkb1.keys.size) should be (1)
    (eImkb1.nsIds.size) should be (1)
    (eImkb1.resolutions.head.size) should be (1)
    eImkb1.addEntries("ABC", "ns1", "A-Value")
    (eImkb1.keys.size) should be (1)        // key should not increment key count
    (eImkb1.nsIds.size) should be (1)       // same nsId should not increment key count
    (eImkb1.resolutions.head.size) should be (1) // key should not increment entry count
    eImkb1.addEntries("a b c", "ns1", "A-Value")
    (eImkb1.keys.size) should be (1)        // key should not increment key count
    (eImkb1.nsIds.size) should be (1)       // same nsId should not increment key count
    (eImkb1.resolutions.head.size) should be (2) // new key should increment entry count
    eImkb1.addEntries("A B C", "ns1", "A-Value")
    (eImkb1.keys.size) should be (1)        // key should not increment key count
    (eImkb1.nsIds.size) should be (1)       // same nsId should not increment key count
    (eImkb1.resolutions.head.size) should be (2) // key should not increment entry count
    eImkb1.addEntries("A-b-C", "ns1", "A-Value")
    (eImkb1.keys.size) should be (1)        // key should not increment key count
    (eImkb1.nsIds.size) should be (1)       // same nsId should not increment key count
    (eImkb1.resolutions.head.size) should be (3) // new key should increment entry count
    eImkb1.addEntries("A/b-C", "ns1", "A-Value")
    (eImkb1.keys.size) should be (1)        // key should not increment key count
    (eImkb1.nsIds.size) should be (1)       // same nsId should not increment key count
    (eImkb1.resolutions.head.size) should be (4) // new key should increment entry count
    eImkb1.addEntries("a/b/c", "ns1", "A-Value")
    (eImkb1.keys.size) should be (1)        // key should not increment key count
    (eImkb1.nsIds.size) should be (1)       // same nsId should not increment key count
    (eImkb1.resolutions.head.size) should be (5) // new key should increment entry count
    // eImkb1.dump                             // DEBUGGING
  }

  "Empty KB 1" should "be able to resolve lookups based on previous filling" in {
    // eImkb1.dump                              // DEBUGGING
    (eImkb1.lookup("")) should be (empty)
    (eImkb1.lookup("abc").head.size) should be (5)
    (eImkb1.lookup("ABC").head.size) should be (5)
    (eImkb1.lookup("a b c").head.size) should be (5)
    (eImkb1.lookup("A B C").head.size) should be (5)
    (eImkb1.lookup("A-b-C").head.size) should be (5)
    (eImkb1.lookup("A/b-C").head.size) should be (5)
    (eImkb1.lookup("a/b/c").head.size) should be (5)
    (eImkb1.lookup("a/b")) should be (empty)
    (eImkb1.lookup("a/b/d")) should be (empty)
    (eImkb1.lookup("A/B-C DE")) should be (empty)
  }


  "Empty KB 2" should "add unique keys only" in {
    eImkb2.addEntries("abc", "tst1", "abc1")
    (eImkb2.keys.size) should be (1)
    (eImkb2.nsIds.size) should be (1)
    (eImkb2.resolutions.head.size) should be (1)
    eImkb2.addEntries("abc", "tst1", "abc1")
    (eImkb2.keys.size) should be (1)        // same add should be idempotent
    (eImkb2.nsIds.size) should be (1)       // same nsId should not increment key count
    (eImkb2.resolutions.head.size) should be (1) // same key should not increment entry count
    eImkb2.addEntries("ABC", "tst1", "abc1")
    (eImkb2.keys.size) should be (2)        // should have only added one more
    (eImkb2.nsIds.size) should be (1)       // same nsId should not increment key count
    (eImkb2.resolutions.head.size) should be (1) // key should increment entry count
    eImkb2.addEntries("XYZ", "tst1", "abc1")
    (eImkb2.keys.size) should be (4)        // should have added two more resolutions
    (eImkb2.nsIds.size) should be (1)       // same nsId should not increment key count
    (eImkb2.resolutions.head.size) should be (2) // new key should increment entry count
    eImkb2.addEntries("abc", "tst1", "abc2")
    (eImkb2.keys.size) should be (4)        // same key should not increment key count
    (eImkb2.nsIds.size) should be (2)       // new nsId should have added one more key
    (eImkb2.resolutions.head.size) should be (3) // new value should increment entry count
    eImkb2.addEntries("ABC", "tst1", "abc2")
    (eImkb2.keys.size) should be (4)        // same key should not increment key count
    (eImkb2.nsIds.size) should be (2)       // same nsId should not increment key count
    (eImkb2.resolutions.head.size) should be (3) // key should not increment entry count
    eImkb2.addEntries("EntryKey", "tst2", "abc2")
    (eImkb2.keys.size) should be (6)        // new key should increment key count
    (eImkb2.nsIds.size) should be (3)       // new nsId should increment key count
    (eImkb2.resolutions.head.size) should be (4) // new key/ns/value should increment entry count
    eImkb2.addEntries("abc", "tst2", "abc2")
    (eImkb2.keys.size) should be (6)        // key should not increment key count
    (eImkb2.nsIds.size) should be (3)       // nsId should not increment key count
    (eImkb2.resolutions.head.size) should be (5) // new key/ns/value should increment entry count
    // eImkb2.dump                             // DEBUGGING
  }

  "Empty KB 2" should "be able to resolve lookups based on previous filling" in {
    // eImkb2.dump                              // DEBUGGING
    (eImkb2.lookup("")) should be (empty)
    (eImkb2.lookup("abc").head.size) should be (5)
    (eImkb2.lookup("ABC").head.size) should be (3)
    (eImkb2.lookup("xyz").head.size) should be (2)
    (eImkb2.lookup("XYZ").head.size) should be (2)
    (eImkb2.lookup("EntryKey").head.size) should be (2)
    (eImkb2.lookup("entrykey").head.size) should be (2)
    (eImkb2.lookup("Entry Key").head.size) should be (2)
    (eImkb2.lookup("entry key").head.size) should be (2)
    (eImkb2.lookup("Entry-Key").head.size) should be (2)
    (eImkb2.lookup("entry-key").head.size) should be (2)
    (eImkb2.lookup("A/B-C DE")) should be (empty)
  }
}


class EmptyIMKBFactory {
  def makeIMKB(keyTransforms: KBKeyTransformsGroup): InMemoryKB = {
    val metaInfo = new IMKBMetaInfo(kbFilename=None, namespace="tst", baseURI="http://test.com")
    new InMemoryKB(metaInfo, keyTransforms)
  }
}
