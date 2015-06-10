package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.bionlp.mentions._
import org.scalatest._
import TestUtils._

/**
 * Unit tests to ensure grounding is working properly
 */
class TestModifications extends FlatSpec with Matchers {

  val grounding1 = "p65 and NF-kappaB p65 are the same entity."
  grounding1 should "contain two entities with the same grounding ID: Xref(uniprot,P21579)" in {
    val mentions = parseSentence(grounding1)
    mentions should have size (2)
    val e1 = mentions.head
    val e2 = mentions.last
    // Entities should have the same ID
    (e1.xref == e2.xref) should be (true)
  }
}
