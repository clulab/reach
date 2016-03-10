package edu.arizona.sista.reach

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.utils.Serializer

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try
import TestUtils._

/**
  * Test the serialization of mention sequences.
  *   Written by: Tom Hicks. 3/8/2016
  *   Last Modified: Some alterations to try and get this test to work.
  */
class TestMentionSerialization extends FlatSpec with Matchers {

  val text1 = "Mouse AKT1 phosphorylates PTHR2 in chicken adenoids."
  val mentions = testReach.extractFrom(text1, "serialization-test", "1")

  val tempFile = java.io.File.createTempFile("TestMention", ".ser")
  val serfile = tempFile.getPath

  text1 should "produce 7 mentions" in {
    mentions should have size (7)
  }

  "Serializer" should "write serialized mentions" in {
    Serializer.save[Seq[BioMention]](mentions, serfile)
  }

  "Serializer" should "load serialized mentions" in {
    val rdMentions = Serializer.load[Seq[BioMention]](serfile)
    rdMentions should have size (7)
  }

}
