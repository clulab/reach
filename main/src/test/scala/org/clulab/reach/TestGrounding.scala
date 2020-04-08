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
    mentions should have size (3)
    var count = 0
    for(m <- mentions) {
      if(m.text == "p65") {
        count += 1
      }
    }
    (count == 2) should be (true)
  }
  //We love NF-kappaB p65, p65, and NF-kappaB.
  val groundingTwo = "We love NF-kappaB p65, p65, and NF-kappaB."
  groundingTwo should "contain 4 entities, 2 of which have the same grounding ID: (uniprot,P21579)" in {
    val mentions = getBioMentions(groundingTwo)
    mentions should have size (4)
    var count = 0
    for(m <- mentions) {
      if(m.text == "p65") {
        count += 1
      }
    }
    (count == 2) should be (true)
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

}
