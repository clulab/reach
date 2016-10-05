package org.clulab.reach

import com.typesafe.config.ConfigFactory
import org.clulab.reach.context._
import org.clulab.reach.mentions._
import org.clulab.reach.grounding._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // ignore IntelliJ: THIS IS USED!
import TestUtils._


/**
  * Unit tests of the grounding trait.
  *   Written by: Tom Hicks. 3/7/2016
  *   Last Modified: Update tests for new Species mentions.
  */
class TestReachGrounder extends FlatSpec with Matchers {

  val config = ConfigFactory.load()
  val overrideSpecies = config.getBoolean("grounding.overrideSpecies")

  val text1 = "AKT1 phosphorylates PTHR2."
  val mentions1 = getBioMentions(text1)

  // Test human-by-default grounding:
  text1 should "produce 4 mentions" in {
    // printMentions(Try(mentions1), true)      // DEBUGGING
    mentions1 should have size (4)
  }

  "Text1 non-BioTextBound mentions" should "not have groundings" in {
    mentions1.filter(!_.isInstanceOf[BioTextBoundMention]).forall(_.grounding.isEmpty) should be (true)
  }

  "Text1 BioTextBound mentions" should "have groundings" in {
    mentions1.filter(_.isInstanceOf[BioTextBoundMention]).forall(_.grounding.isDefined) should be (true)
  }

  "Text1 mentions" should "be grounded as human" in {
    mentions1.filter(_.isInstanceOf[BioTextBoundMention])
             .forall(m => m.grounding.isDefined &&
                     Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }


  // Test scope of species and associated grounding:
  val text2 = "Mouse AKT1 is different from rice AKT1."
  // val text2 = "Rat ITSN1 is different from human ITSN1."
  val mentions2 = getBioMentions(text2)

  text2 should "produce 4 mentions" in {
    // printMentions(Try(mentions2), true)       // DEBUGGING
    mentions2 should have size (4)
  }

  "Text2 non-BioTextBound mentions" should "not have groundings" in {
    mentions2.filter(!_.isInstanceOf[BioTextBoundMention]).forall(_.grounding.isEmpty) should be (true)
  }

  "Text2 BioTextBound mentions" should "have groundings" in {
    mentions2.filter(_.isInstanceOf[BioTextBoundMention]).forall(_.grounding.isDefined) should be (true)
  }

  "Text2 mentions" should "may or may not have all rice groundings" in {
    // This test depends on the setting of grounding.overrideSpecies flag in application.conf:
    mentions2.filter(m => hasSpeciesContext(m)).forall(m => m.isGrounded && m.grounding.get.species == "rice") should be (!overrideSpecies)
    // Using species, this will no longer be true when Reach issue #152 is implemented:
    // mentions2.filter(m => hasSpeciesContext(m)).forall(m => m.isGrounded && m.grounding.get.species == "rice") should be (true)
    // This will only pass when Reach issue #152 is implemented:
    // mentions2.filter(m => hasSpeciesContext(m)).forall(m => m.isGrounded && m.grounding.get.species == "rice") should be (false)
  }

  "Text2 mentions" should "have mouse and rice groundings" in {
    val spms = mentions2.filter(m => hasSpeciesContext(m))
    spms should not be empty
    // This will only pass when Reach issue #152 is implemented:
    // (spms(0).isGrounded &&
    //  spms(0).grounding.get.hasSpecies &&
    //  spms(0).grounding.get.species == "mouse") should be (true)
    // This test depends on the setting of grounding.overrideSpecies flag in application.conf:
    (spms(1).isGrounded &&
     spms(1).grounding.get.hasSpecies &&
     spms(1).grounding.get.species == "rice") should be (!overrideSpecies)
  }


  // Test proteins in mouse context:
  val text3 = "Bcdin3 and Bcp and CK-4 and Zzz3 are found in the mouse."
  val mentions3 = getBioMentions(text3)

  text3 should "produce mentions" in {
    // printMentions(Try(mentions3), true)      // DEBUGGING
    mentions3.isEmpty should be(false)
  }

  "Text3 mentions" should "have mouse groundings" in {
    mentions3.isEmpty should be (false)
    val spms = mentions3.filter(m => hasSpeciesContext(m) && m.isGrounded && m.grounding.get.hasSpecies)
    spms should not be empty
    spms should have size(4)
   // This test depends on the setting of grounding.overrideSpecies flag in application.conf:
    spms.forall(m => m.grounding.get.species == "mouse") should be (!overrideSpecies)
  }

  // Test same proteins in human context:
  val text4 = "BCDIN3 and BCP and CK-4 and ZZZ3 are found in the human."
  val mentions4 = getBioMentions(text4)

  text4 should "produce mentions" in {
    // printMentions(Try(mentions4), true)      // DEBUGGING
    mentions4.isEmpty should be(false)
  }

  "Text4 mentions" should "have human groundings" in {
    mentions4.isEmpty should be (false)
    val spms = mentions4.filter(m => hasSpeciesContext(m) && m.isGrounded && m.grounding.get.hasSpecies)
    spms should not be empty
    spms should have size(4)
    spms.forall(m => Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  // Test same proteins in African clawed frog context (where they are not found):
  val text5 = "BCDIN3 and BCP and CK-4 and ZZZ3 are found in the african clawed frog."
  val mentions5 = getBioMentions(text5)

  text5 should "produce mentions" in {
    // printMentions(Try(mentions5), true)      // DEBUGGING
    mentions5.isEmpty should be(false)
  }

  "Text5 mentions" should "have human groundings" in {
    mentions5.isEmpty should be (false)
    val spms = mentions5.filter(m => hasSpeciesContext(m) && m.isGrounded && m.grounding.get.hasSpecies)
    spms should not be empty
    spms should have size(4)
    spms.forall(m => Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

  // Test species/grounding boundaries across sentences.
  val text6 = "ATRX and B2GPI and Zonulin are found in rats. ATRX and B2GPI and Zonulin are also found in humans."
  val mentions6 = getBioMentions(text6) 

  text6 should "produce mentions" in {
    // printMentions(Try(mentions6), true)      // DEBUGGING
    mentions6.isEmpty should be(false)
  }

  "Text6 mentions" should "have sentence bounded species context and groundings" in {
    mentions6.isEmpty should be (false)
    val spms0 = mentions6.filter(m => hasSpeciesContext(m) && (m.sentence == 0) &&
                                      m.isGrounded && m.grounding.get.hasSpecies)
    spms0 should not be empty
    spms0 should have size(3)

    val spms1 = mentions6.filter(m => hasSpeciesContext(m) && (m.sentence == 1) &&
                                      m.isGrounded && m.grounding.get.hasSpecies)
    spms1 should not be empty
    spms1 should have size(3)

    // This test depends on the setting of grounding.overrideSpecies flag in application.conf:
    spms0.forall(m => m.grounding.get.species == "rat") should be (!overrideSpecies)
    spms1.forall(m => Speciated.isHumanSpecies(m.grounding.get.species)) should be (true)
  }

}
