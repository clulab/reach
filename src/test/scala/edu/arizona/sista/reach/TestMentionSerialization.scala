package edu.arizona.sista.reach

import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.utils.Serializer

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try
import TestUtils._

/**
  * Test the serialization of mention sequences.
  *   Written by: Tom Hicks. 3/8/2016
  *   Last Modified: Initial creation.
  */
class TestMentionSerialization extends FlatSpec with Matchers {

  val text1 = "Mouse AKT1 phosphorylates PTHR2 in chicken adenoids."
  val mentions = getBioMentions(text1)

  val serfile = "/tmp/TestMention.ser"

  text1 should "produce 7 mentions" in {
    mentions should have size (7)
  }

  "Text" should "write serialized mentions" in {
    Serializer.save[Seq[BioMention]](mentions, serfile)
  }

  "Text" should "load serialized mentions" in {
    val rdMentions = Serializer.load[Seq[BioMention]](serfile)
    rdMentions should have size (7)
  }

}
