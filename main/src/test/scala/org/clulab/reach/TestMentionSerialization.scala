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
  val mentions1 = testReach.extractFrom(text1, "serialization-test", "1")
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
  val mentions2 = testReach.extractFrom(text2, "serialization-test", "2")
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

}
