package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests of InMemoryKBs.
  *   Written by: Tom Hicks. 1/22/2017.
  *   Last Modified: Add tests for species for NS/ID method.
  */
class TestIMKB extends FlatSpec with Matchers {
  val eFactory = new EmptyIMKBFactory

  // create empty IMKB with one canonical key transform:
  val canonKTs = Seq( canonicalKT _)
  val ktConfig1 = KBKeyTransformsGroup(canonKTs)
  val eImkb1 = eFactory.makeIMKB(ktConfig1) // configure new IMKB (factory below)

  // create empty IMKB with several types of storage key transforms:
  val multiKTs = Seq( identityKT _, lowercaseKT _, canonicalKT _)
  val ktConfig2 = KBKeyTransformsGroup(multiKTs)
  val eImkb2 = eFactory.makeIMKB(ktConfig2) // configure new IMKB (factory below)

  // create empty IMKB with lowercase storage key transforms:
  val lowerKTs = Seq( lowercaseKT _ )
  val ktConfig3 = KBKeyTransformsGroup(lowerKTs)
  val eImkb3 = eFactory.makeIMKB(ktConfig3) // configure new IMKB (factory below)


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

  /** Fill an IMKB for use with species set testing. */
  val erkVals = Seq(
    Seq("ERK1",  "C4YGK0", "Candida albicans"),
    Seq("ERK1",  "C4YGK0", "Yeast"),
    Seq("ERK-1", "P21708", "Rat"),
    Seq("Erk1",  "P21708", "Rat"),
    Seq("ERK-1", "P21708", "Rattus norvegicus"),
    Seq("Erk1",  "P21708", "Rattus norvegicus"),
    Seq("ERK-1", "P27361", "Homo sapiens"),
    Seq("ERK1",  "P27361", "Homo sapiens"),
    Seq("ERK-1", "P27361", "Human"),
    Seq("ERK1",  "P27361", "Human"),
    Seq("erk1",  "P42525", "Dictyostelium discoideum"),
    Seq("erk1",  "P42525", "Slime mold"),
    Seq("ERK1",  "Q07176", "Alfalfa"),
    Seq("ERK1",  "Q07176", "Medicago sativa"),
    Seq("ERK1",  "Q5A1D3", "Candida albicans"),
    Seq("ERK1",  "Q5A1D3", "Yeast"),
    Seq("ERK-1", "Q63844", "Mouse"),
    Seq("Erk1",  "Q63844", "Mouse"),
    Seq("ERK-1", "Q63844", "Mus musculus"),
    Seq("Erk1",  "Q63844", "Mus musculus"),
    Seq("ERK-2", "P28482", "Homo sapiens"),
    Seq("ERK2",  "P28482", "Homo sapiens"),
    Seq("ERK-2", "P28482", "Human"),
    Seq("ERK2",  "P28482", "Human"),
    Seq("ERK-2", "P46196", "Bos taurus"),
    Seq("ERK2",  "P46196", "Bos taurus"),
    Seq("ERK-2", "P46196", "Bovine"),
    Seq("ERK2",  "P46196", "Bovine"),
    Seq("ERK-2", "P63085", "Mouse"),
    Seq("Erk2",  "P63085", "Mouse"),
    Seq("ERK-2", "P63085", "Mus musculus"),
    Seq("Erk2",  "P63085", "Mus musculus"),
    Seq("ERK-2", "P63086", "Rat"),
    Seq("Erk2",  "P63086", "Rat"),
    Seq("ERK-2", "P63086", "Rattus norvegicus"),
    Seq("Erk2",  "P63086", "Rattus norvegicus"),
    Seq("erk2",  "Q54QB1", "Dictyostelium discoideum"),
    Seq("erk2",  "Q54QB1", "Slime mold"),
    Seq("ERK-3", "P27704", "Rat"),
    Seq("Erk3",  "P27704", "Rat"),
    Seq("ERK-3", "P27704", "Rattus norvegicus"),
    Seq("Erk3",  "P27704", "Rattus norvegicus"),
    Seq("ERK-3", "Q16659", "Homo sapiens"),
    Seq("ERK3",  "Q16659", "Homo sapiens"),
    Seq("ERK-3", "Q16659", "Human"),
    Seq("ERK3",  "Q16659", "Human"),
    Seq("ERK-3", "Q61532", "Mouse"),
    Seq("Erk3",  "Q61532", "Mouse"),
    Seq("ERK-3", "Q61532", "Mus musculus"),
    Seq("Erk3",  "Q61532", "Mus musculus"),
    Seq("ERK-4", "P31152", "Homo sapiens"),
    Seq("ERK4",  "P31152", "Homo sapiens"),
    Seq("ERK-4", "P31152", "Human"),
    Seq("ERK4",  "P31152", "Human"),
    Seq("ERK-4", "Q63454", "Rat"),
    Seq("ERK-4", "Q63454", "Rattus norvegicus"),
    Seq("ERK-4", "Q6P5G0", "Mouse"),
    Seq("Erk4",  "Q6P5G0", "Mouse"),
    Seq("ERK-4", "Q6P5G0", "Mus musculus"),
    Seq("Erk4",  "Q6P5G0", "Mus musculus"),
    Seq("ERK-5", "P0C865", "Rat"),
    Seq("Erk5",  "P0C865", "Rat"),
    Seq("ERK-5", "P0C865", "Rattus norvegicus"),
    Seq("Erk5",  "P0C865", "Rattus norvegicus"),
    Seq("ERK-5", "Q13164", "Homo sapiens"),
    Seq("ERK5",  "Q13164", "Homo sapiens"),
    Seq("ERK-5", "Q13164", "Human"),
    Seq("ERK5",  "Q13164", "Human"),
    Seq("ERK-5", "Q9WVS8", "Mouse"),
    Seq("Erk5",  "Q9WVS8", "Mouse"),
    Seq("ERK-5", "Q9WVS8", "Mus musculus"),
    Seq("Erk5",  "Q9WVS8", "Mus musculus"),
    Seq("ERK-6", "O08911", "Mouse"),
    Seq("ERK-6", "O08911", "Mus musculus"),
    Seq("ERK-6", "P53778", "Homo sapiens"),
    Seq("ERK6",  "P53778", "Homo sapiens"),
    Seq("ERK-6", "P53778", "Human"),
    Seq("ERK6",  "P53778", "Human"),
    Seq("ERK-6", "Q63538", "Rat"),
    Seq("ERK-6", "Q63538", "Rattus norvegicus"),
    Seq("ERK-7", "Q80Y86", "Mouse"),
    Seq("Erk7",  "Q80Y86", "Mouse"),
    Seq("ERK-7", "Q80Y86", "Mus musculus"),
    Seq("Erk7",  "Q80Y86", "Mus musculus"),
    Seq("ERK-7", "Q8TD08", "Homo sapiens"),
    Seq("ERK7",  "Q8TD08", "Homo sapiens"),
    Seq("ERK-7", "Q8TD08", "Human"),
    Seq("ERK7",  "Q8TD08", "Human"),
    Seq("ERK-7", "Q9Z2A6", "Rat"),
    Seq("Erk7",  "Q9Z2A6", "Rat"),
    Seq("ERK-7", "Q9Z2A6", "Rattus norvegicus"),
    Seq("Erk7",  "Q9Z2A6", "Rattus norvegicus"),
    Seq("ERK-8", "Q8TD08", "Homo sapiens"),
    Seq("ERK8",  "Q8TD08", "Homo sapiens"),
    Seq("ERK-8", "Q8TD08", "Human"),
    Seq("ERK8",  "Q8TD08", "Human")
  )
  erkVals.foreach { ent =>
    eImkb3.addEntries(ent(0), "uniprot", ent(1), ent(2))
  }

  "Empty KB 3" should "be able to compute species sets based on previous filling" in {
    // eImkb3.dump                              // DEBUGGING
    (eImkb3.speciesForNsId("uniprot")) should be (empty)         // no ID field
    (eImkb3.speciesForNsId("uniprot:")) should be (empty)        // no ID field
    (eImkb3.speciesForNsId("NotInKB")) should be (empty)         // no namespace field
    (eImkb3.speciesForNsId("Q8TD08")) should be (empty)          // no namespace field
    (eImkb3.speciesForNsId("uniprot:NotInKB")) should be (empty) // not in KB
    (eImkb3.speciesForNsId("ZZZZZ")) should be (empty)           // not in KB

    val q8TD08 = eImkb3.speciesForNsId("uniprot:Q8TD08")
    (q8TD08) should not be (empty)
    (q8TD08) should contain ("human")
    (q8TD08) should contain ("homo sapiens")
    (q8TD08) should not contain ("rat")
    (q8TD08) should not contain ("rattus norvegicus")
    (q8TD08) should not contain ("mouse")
    (q8TD08) should not contain ("mus musculus")
    (q8TD08) should not contain ("bovine")
    (q8TD08) should not contain ("bos taurus")
    (q8TD08) should not contain ("yeast")
    (q8TD08) should not contain ("candida albicans")

    val p21708 = eImkb3.speciesForNsId("uniprot:P21708")
    (p21708) should not be (empty)
    (p21708) should contain ("rat")
    (p21708) should contain ("rattus norvegicus")
    (p21708) should not contain ("human")
    (p21708) should not contain ("homo sapiens")
    (p21708) should not contain ("mouse")
    (p21708) should not contain ("mus musculus")
    (p21708) should not contain ("bovine")
    (p21708) should not contain ("bos taurus")
    (p21708) should not contain ("yeast")
    (p21708) should not contain ("candida albicans")
  }
}


class EmptyIMKBFactory {
  def makeIMKB(keyTransforms: KBKeyTransformsGroup): InMemoryKB = {
    val metaInfo = new IMKBMetaInfo(kbFilename=None, namespace="tst", baseURI="http://test.com")
    new InMemoryKB(metaInfo, keyTransforms)
  }
}
