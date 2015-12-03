package edu.arizona.sista.reach.nxml

import io.Source
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.reach.ReachSystem

trait Fixtures {
  // Set up the fixtures
  def nxml = Source.fromURL(getClass.getResource("/nxml/ContextTests.nxml")).mkString
  def reader = new NxmlReader
  def entry = reader.readNxml(nxml, "ContextTests")
  def reachSystem = new ReachSystem
  /////////
}

class DeterministicPoliciesTests extends FlatSpec with Matchers with Fixtures {

  // Tests
  behavior of "Deterministic policies"

  it should "have two abstract entries with no titles in them" in {
    info("Running reach over a paragraph")

    val paragraph = entry(2)

    // Remove/Change the filter if context is attached to other BioMentions
    val mentions = reachSystem.extractFrom(Seq(paragraph)) filter {
      case em:BioEventMention => true
      case _ => false
    }

    info(s"The number of abstract entries is $size")
    size should equal (2)

    val titleSize = absEntries.filter(_.isTitle).size
    info(s"The number of abstract entry titles is $titleSize")
    titleSize should equal (0)
  }

  ////////
}
