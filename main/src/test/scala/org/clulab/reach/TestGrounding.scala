package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import org.clulab.reach.mentions._
import TestUtils._

/**
 * Unit tests to ensure grounding is working properly
 */
class TestGrounding extends FlatSpec with Matchers {

  val grounding1 = "p65 and NF-kappaB p65 are the same entity."
  grounding1 should "contain 2 entities with the same grounding ID: (uniprot,P21579)" in {
    val mentions = getBioMentions(grounding1)
    mentions should have size (2)
    val e1 = mentions.head
    val e2 = mentions.last
    // Entities should have the same ID
    //TODO: revisit grounding lookup algo after eval
    //(e1.grounding == e2.grounding) should be (true)
  }
  //We love NF-kappaB p65, p65, and NF-kappaB.
  val groundingTwo = "We love NF-kappaB p65, p65, and NF-kappaB."
  groundingTwo should "contain 3 entities, 2 of which have the same grounding ID: (uniprot,P21579)" in {
    val mentions = getBioMentions(groundingTwo)
    mentions should have size (3)
    val e1 = mentions.find(_.text == "p65").get
    val e2 = mentions.find(_.text == "NF-kappaB p65").get
    // Entities should have the same ID
    //TODO: revisit grounding lookup algo after eval
    //(e1.grounding == e2.grounding) should be (true)
  }

  val grounding3 = "MEK phosphorylates Ras."
  grounding3 should "contain grounded entities" in {
    val mentions = getBioMentions(grounding3)
    val tbms = mentions flatMap {
      case m: BioTextBoundMention => Some(m)
      case _ => None
    }
    tbms(0).grounding should be ('defined)
    tbms(1).grounding should be ('defined)
  }

  val grounding4 = "Nav1.8 binds Cav1.2"
  grounding4 should "have correct groundings" in {
    val mentions7 = getBioMentions(grounding4)
    mentions7(0).grounding.get.id == "Q9Y5Y9"
    mentions7(1).grounding.get.id == "Q13936"
  }

}
