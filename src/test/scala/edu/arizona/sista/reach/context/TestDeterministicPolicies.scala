package edu.arizona.sista.reach.context

import io.Source
import scala.collection.mutable
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.reach.nxml.NxmlReader
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.ReachSystem

trait Fixtures {
  // Set up the fixtures
  def nxml = Source.fromURL(getClass.getResource("/inputs/nxml/PMC2597732.nxml")).mkString
  def reader = new NxmlReader
  def entries = reader.readNxml(nxml, "ContextTests")
  def reachSystem = new ReachSystem

  val mentions:Seq[BioEventMention] = reachSystem.extractFrom(entries).filter{
      case em:BioEventMention => true
      case _ => false
    }.map(_.asInstanceOf[BioEventMention])

  val context = mentions.map{
      _.context.getOrElse(Map[String,Seq[String]]())
  }
  /////////
}

class DeterministicPoliciesTests extends FlatSpec with Matchers with Fixtures {

  // Tests
  behavior of "Deterministic policies"

  it should "Have some context" in {
    // Remove/Change the filter if context is attached to other BioMentions

    val size = context.flatMap(_.values.flatten).size

    info(s"The number of context resolutions is: $size")
    size should be > 0

  }

  // No more than one context per type
  it should "have no more than a context of each type simultaneously" in {
      context foreach {
            _ foreach {
                keyVal =>
                    keyVal._2.size should be <= 1
            }
      }
  }
  
  // No more than 5 repetitions of the same context
  // Mention where context starts

  ////////
}
