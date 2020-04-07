package org.clulab.reach

import scala.util.Try                       // do not remove: needed for debugging
import org.scalatest._

import TestUtils._

import org.clulab.reach.mentions._
import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachKBUtils._

/**
  * Unit tests to ensure protein complex resolutions are working for KB grounding.
  *   Written by: Tom Hicks. 11/17/2016.
  *   Last Modified: Update for updated BE families and complexes of 8/22/2017.
  */
class TestComplexResolutions extends FlatSpec with Matchers {

  val bePC = new TestComplexKBL           // defined after this class (LOOK BELOW)

  "Test sentences" should "should be marked as protein grounded but not family grounded" in {
    val txtU = "PTHR2 is cool."
    val menU = getBioMentions(txtU).head
    val txtL = "pthr2 is also cool."
    val menL = getBioMentions(txtL).head

    (isProteinGrounded(menU)) should be (true)
    (isProteinGrounded(menL)) should be (true)
    (isFamilyGrounded(menU)) should be (false)
    (isFamilyGrounded(menL)) should be (false)
  }

  "BE-PC resolve" should "fail despite alternate lookups" in {
    // keys not in KB:
    (bePC.resolve("NOTINKB")) should be (empty)
    (bePC.resolve("notinkb")) should be (empty)
    (bePC.resolve("notinkb_human")) should be (empty)
    (bePC.resolve("notinkb protein")) should be (empty)
    (bePC.resolve("notinkb family")) should be (empty)
    (bePC.resolve("mutant-zyx-1")) should be (empty) // mutant pattern not matched
    // family key transforms not applicable for complexes:
    (bePC.resolve("pthr2 family")) should be (empty)
    (bePC.resolve("zyx-1 family")) should be (empty)
  }

  // these are all families, so this should fail now
  /*
  "BE-PC resolve" should "work for protein complex Bioentities" in {
    (bePC.resolve("9_1_1")) should be (defined) // first entry
    (bePC.resolve("Activin_A")) should be (defined)
    (bePC.resolve("activin_a")) should be (defined)
    (bePC.resolve("Activin_AB")) should be (defined)
    (bePC.resolve("AMPK")) should be (defined)
    (bePC.resolve("AMPK_A2B2G2")) should be (defined)
    (bePC.resolve("COX")) should be (defined)
    (bePC.resolve("Cox")) should be (defined)
    (bePC.resolve("cox")) should be (defined)
    (bePC.resolve("NF-kappaB")) should be (defined)
    (bePC.resolve("NFkappaB")) should be (defined)
    (bePC.resolve("PI")) should be (defined)
    (bePC.resolve("PI-3")) should be (defined)
    (bePC.resolve("pi-3")) should be (defined)
    (bePC.resolve("PI 3")) should be (defined)
    (bePC.resolve("PI3-kinase")) should be (defined)
    (bePC.resolve("pi3-kinase")) should be (defined)
    (bePC.resolve("PI3K")) should be (defined)
    (bePC.resolve("PI3Kinase")) should be (defined)
    (bePC.resolve("PI3 kinase")) should be (defined)
    (bePC.resolve("pi3 kinase")) should be (defined)
    (bePC.resolve("TORC1")) should be (defined) // last entry
    (bePC.resolve("TORC-1")) should be (defined)
  }
  */

  "BE-PC resolve" should "fail for protein complex Bioentities" in {
    (bePC.resolve("AMPK-alpha1")) should be (empty) // GGP
    (bePC.resolve("FOXP3")) should be (empty)       // GGP
  }

}


// KB for Protein Complexes. Uses alternate protein lookups.
class TestComplexKBL extends IMKBLookup {
  val meta = new IMKBMetaInfo(
    kbFilename = Some(StaticProteinFamilyOrComplexFilename),
    namespace = "be",
    baseURI = "https://identifiers.org/fplx/",
    isProteinKB = true
  )
  // println(s"BE-KB.metaInfo=${memoryKB.metaInfo}")
  val keyTransforms = KBKeyTransformsGroup(CasedKeyTransforms, ProteinAuxKeyTransforms)
  memoryKB = (new TsvIMKBFactory).make(meta, keyTransforms)
}
