package org.clulab.reach

import org.clulab.odin._
import org.clulab.reach.mentions._
import org.clulab.utils.Serializer

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try
import TestUtils._

/**
  * Test the serialization of mention sequences.
  *   Written by: Tom Hicks. 3/8/2016
  *   Last Modified: Update for use of Uberon as organ KB.
  */
class TestMentionSerialization extends FlatSpec with Matchers {

  val text1 = "Mek was not phosphorylized by AKT1"  // this one serializes
  val mentions1 = testReach.extractFrom(text1, "serialization-test", "1", None)
  val tempFile1 = java.io.File.createTempFile("TestMention1", ".ser")
  val serfile1 = tempFile1.getPath

  text1 should "produce 2 mentions" in {
    mentions1 should have size (2)
  }

  "Serializer" should "write serialized non-context mentions" in {
    Serializer.save[Seq[BioMention]](mentions1, serfile1)
  }

  "Serializer" should "load serialized non-context mentions" in {
    val rdMentions1 = Serializer.load[Seq[BioMention]](serfile1)
    rdMentions1 should have size (2)
  }


  val text2 = "Mouse AKT2 phosphorylates PTHR2 in chicken adenoid."
  val mentions2 = testReach.extractFrom(text2, "serialization-test", "2", None)
  val tempFile2 = java.io.File.createTempFile("TestMention2", ".ser")
  val serfile2 = tempFile2.getPath

  text2 should "produce 7 mentions" in {
    mentions2 should have size (7)
  }

  "Serializer" should "write serialized context mentions" in {
    Serializer.save[Seq[BioMention]](mentions2, serfile2)
  }

  "Serializer" should "load serialized context mentions" in {
    val rdMentions2 = Serializer.load[Seq[BioMention]](serfile2)
    rdMentions2 should have size (7)
  }

  val text3 = "Tbet Rag2 mice (Garrett et al., 2010) as well as Bacteroides spp. (Bloom et al., 2011), Helicobacter spp. (Fox et al., 2011), and Bilophila wadsworthia (Devkota et al., 2012) in Il10 have been shown to enhance intestinal inflammation.The acute dextran sulfate sodium"
  val mentions3 = testReach.extractFrom(text3, "serialization-test", "3", None)
  val koTriggers3 = getKOtriggers(mentions3)
  val tempFile3 = java.io.File.createTempFile("TestMention3", ".ser")
  val serfile3 = tempFile3.getPath

  def getKOtriggers(bioMentions: Seq[BioMention]) = bioMentions.flatMap { mention => mention.modifications.collect { case koTrigger: KOtrigger => koTrigger } }

  text3 should "produce 8 mentions and 4 triggers" in {
    mentions3 should have size (8)
    koTriggers3 should have size (4)
  }

  "Serializer" should "write serialized modifications" in {
    Serializer.save[Seq[BioMention]](mentions3, serfile3)
  }

  "Serializer" should "load serialized modifications" in {
    val rdMentions3 = Serializer.load[Seq[BioMention]](serfile3)
    val rdKoTriggers3 = getKOtriggers(rdMentions3)
    rdMentions3 should have size (8)
    rdKoTriggers3 should have size (4)

    koTriggers3 should contain theSameElementsInOrderAs (rdKoTriggers3)
  }
}
