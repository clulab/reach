package org.clulab.reach

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.Speciated._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._

/**
  * Test that our override KB works properly for NER and grounding.
  *   Written by: Tom Hicks. 12/26/2016.
  *   Last Modified: Initial creation.
  */
class TestPhase3Overrides extends FlatSpec with Matchers {
  val GGP = "Gene_or_gene_product"
  val Protein = "Protein"
  val Goat = SpeciesNameSet("goat")
  val Mouse = SpeciesNameSet("mouse")
  val Rabbit = SpeciesNameSet("rabbit")
  val Rat = SpeciesNameSet("rat")

  // test data sets
  val ggp1 = "RPA32 is an override GGP."
  val ggp1_ids = Seq("P15927")

  val ggpG = """Beclin, BECN1, CD274, Pdcd-1L1, and XBP1 are override GGPs."""
  val ggpG_ids = Seq("Q14457", "Q14457", "Q9NZQ7", "Q9NZQ7", "P17861")


  /** Override test driver method. */
  def testMentions (
    text: String,
    ids: Seq[String],
    label: String,
    displayLabel: Option[String] = None,
    groundedSpecies: Option[SpeciesNameSet] = None,
    debug: Boolean = false
  ): Unit = {
    val mentions = getBioMentions(text)

    if (debug)                              // allow debugging on group-by-group basis
      printMentions(Try(mentions), debug)

    text should "have expected number of results" in {
      mentions should not be (empty)
      mentions should have size (ids.size)
    }

    it should s"have labeled all mentions as ${label}" in {
      mentions.count(_ matches label) should be (ids.size)
    }

    if (displayLabel.isDefined) {
      it should s"have display labeled all mentions as ${displayLabel}" in {
        mentions.count(_.displayLabel == displayLabel.get) should be (ids.size)
      }
    }

    if (groundedSpecies.isDefined) {
      val species = groundedSpecies.get
      it should "have grounded all mentions as ${species}" in {
        mentions.forall(m => m.grounding.isDefined &&
          Speciated.isMemberOf(m.grounding.get.species, species)) should be (true)
      }
    }

    it should "match expected grounding IDs" in {
      for ((m, ndx) <- mentions.zipWithIndex) {
        m.grounding.isDefined && (m.grounding.get.id == ids(ndx)) should be (true)
      }
    }
  }

  // Run the actual tests:
  testMentions(ggp1, ggp1_ids, GGP, Some(Protein), Some(Rat))
  testMentions(ggpG, ggpG_ids, GGP, Some(Protein), Some(Goat))
//  testMentions(ggpM, ggpM_ids, GGP, Some(Protein), Some(Mouse))
//  testMentions(ggpB, ggpB_ids, GGP, Some(Protein), Some(Rabbit))

}
