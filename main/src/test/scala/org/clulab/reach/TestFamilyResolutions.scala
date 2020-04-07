package org.clulab.reach

import scala.util.Try

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.mentions._
import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachKBUtils._

/**
  * Unit tests to ensure alternate resolutions are working for KB grounding.
  *   Written by: Tom Hicks. 11/4/2015.
  *   Last Modified: Update to match configured KB parameters.
  */
class TestFamilyResolutions extends FlatSpec with Matchers {

  val ipPF = new IPProtFamKBL               // defined after this class (LOOK BELOW)
  // println("== IP_PF ==========================================================")
  // ipPF.memoryKB.dump                        // DEBUGGING

  "IP-PF" should "should be marked as family grounded but not protein grounded" in {
    val txtU = "PTHR21244 is cool."
    val menU = getBioMentions(txtU).head
    val txtL = "pthr21244 is also cool."
    val menL = getBioMentions(txtL).head

    (isFamilyGrounded(menU)) should be (true)
    (isFamilyGrounded(menL)) should be (true)
    (isProteinGrounded(menU)) should be (false)
    (isProteinGrounded(menL)) should be (false)
  }

  // this KB is a protein Family, therefore resolves using protein transforms should fail:
  "IP-PF resolve" should "fail despite alternate family lookups" in {
    // keys not in KB:
    (ipPF.resolve("NOTINKB")) should be (empty)
    (ipPF.resolve("notinkb")) should be (empty)
    (ipPF.resolve("notinkb_human")) should be (empty)
    (ipPF.resolve("notinkb protein")) should be (empty)
    (ipPF.resolve("notinkb family")) should be (empty)
    // protein key transforms not applicable for protein families:
    (ipPF.resolve("pthr21244 protein")) should be (empty)
    (ipPF.resolve("mutant-pthr21244")) should be (empty)
    (ipPF.resolve("hk protein")) should be (empty)
    (ipPF.resolve("mutant-hk")) should be (empty)
  }

  "IP-PF resolve" should "work with alternate family lookups" in {
    (ipPF.resolve("PTHR21244")) should be (defined)
    (ipPF.resolve("pthr21244")) should be (defined)
    (ipPF.resolve("pthr21244_human")) should be (defined)
    (ipPF.resolve("pthr21244_Human")) should be (defined)
    (ipPF.resolve("pthr21244_HUMAN")) should be (defined)
    (ipPF.resolve("pthr21244 family")) should be (defined)
    (ipPF.resolve("pthr21244 Family")) should be (defined)
    (ipPF.resolve("pthr21244 FAMILY")) should be (defined)
    (ipPF.resolve("hk")) should be (defined)
    (ipPF.resolve("hk_human")) should be (defined)
    (ipPF.resolve("hk_Human")) should be (defined)
    (ipPF.resolve("hk_HUMAN")) should be (defined)
    (ipPF.resolve("hk family")) should be (defined)
  }

  "IP-PF resolveByASpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (ipPF.resolveByASpecies("NotInKB", "ant")) should be (empty)
    (ipPF.resolveByASpecies("NotInKB_human", "ant")) should be (empty)
    (ipPF.resolveByASpecies("NotInKB protein", "ant")) should be (empty)
    (ipPF.resolveByASpecies("NotInKB family", "ant")) should be (empty)
    (ipPF.resolveByASpecies("mutant-NotInKB", "ant")) should be (empty)
    // entry does not have this species:
    (ipPF.resolveByASpecies("hk", "frog")) should be (empty)
    (ipPF.resolveByASpecies("hk_human", "frog")) should be (empty)
    (ipPF.resolveByASpecies("hk protein", "frog")) should be (empty)
    (ipPF.resolveByASpecies("hk family", "frog")) should be (empty)
    (ipPF.resolveByASpecies("mutant-hk", "frog")) should be (empty)
    // protein key transforms not applicable for protein families:
    (ipPF.resolveByASpecies("pthr21244 protein", "human")) should be (empty)
    (ipPF.resolveByASpecies("PTHR21244 protein", "human")) should be (empty)
    (ipPF.resolveByASpecies("mutant-pthr21244", "human")) should be (empty)
    (ipPF.resolveByASpecies("hk protein", "saccharomyces cerevisiae")) should be (empty)
    (ipPF.resolveByASpecies("mutant-hk", "saccharomyces cerevisiae")) should be (empty)
  }

  "IP-PF resolveByASpecies" should "work with alternate lookups" in {
    (ipPF.resolveByASpecies("pthr21244", "human")) should be (defined)
    (ipPF.resolveByASpecies("pthr21244_human", "human")) should be (defined)
    (ipPF.resolveByASpecies("pthr21244 family", "human")) should be (defined)
    (ipPF.resolveByASpecies("hk", "saccharomyces cerevisiae")) should be (defined)
    (ipPF.resolveByASpecies("hk_human", "saccharomyces cerevisiae")) should be (defined)
    (ipPF.resolveByASpecies("hk family", "saccharomyces cerevisiae")) should be (defined)
  }

  val setA =   Set("aardvark")
  val setF =   Set("frog")
  val setH =   Set("human")
  val setHM =  Set("human", "mouse")
  val setHMG = Set("human", "mouse", "gorilla")
  "IP-PF resolveBySpecies" should "should fail despite alternate lookups" in {
    // key not in KB:
    (ipPF.resolveBySpecies("NotInKB", setA)) should be (empty)
    (ipPF.resolveBySpecies("NotInKB_human", setA)) should be (empty)
    (ipPF.resolveBySpecies("NotInKB protein", setA)) should be (empty)
    (ipPF.resolveBySpecies("NotInKB family", setA)) should be (empty)
    (ipPF.resolveBySpecies("mutant-NotInKB", setA)) should be (empty)
    (ipPF.resolveBySpecies("pthr21244 mouse", setH)) should be (empty)
    // entry does not have this species:
    (ipPF.resolveBySpecies("pthr21244", setF)) should be (empty)
    (ipPF.resolveBySpecies("pthr21244_human", setF)) should be (empty)
    (ipPF.resolveBySpecies("pthr21244 protein", setF)) should be (empty)
    (ipPF.resolveBySpecies("pthr21244 family", setF)) should be (empty)
    (ipPF.resolveBySpecies("mutant-pthr21244", setF)) should be (empty)
    // entry does not have these species (yeast only):
    (ipPF.resolveBySpecies("hk", setHM)) should be (empty)
    (ipPF.resolveBySpecies("hk_human", setHM)) should be (empty)
    (ipPF.resolveBySpecies("hk protein", setHM)) should be (empty)
    (ipPF.resolveBySpecies("hk family", setHM)) should be (empty)
    (ipPF.resolveBySpecies("mutant-hk", setHM)) should be (empty)
    // protein key transforms not applicable for protein families:
    (ipPF.resolveBySpecies("pthr21244 protein", setH)) should be (empty)
    (ipPF.resolveBySpecies("PTHR21244 protein", setH)) should be (empty)
    (ipPF.resolveBySpecies("mutant-pthr21244", setH)) should be (empty)
    (ipPF.resolveBySpecies("MUTANT-PTHR21244", setH)) should be (empty)
    (ipPF.resolveBySpecies("pthr21244 protein", setHM)) should be (empty)
    (ipPF.resolveBySpecies("mutant-pthr21244", setHM)) should be (empty)
    (ipPF.resolveBySpecies("pthr21244 protein", setHMG)) should be (empty)
    (ipPF.resolveBySpecies("mutant-pthr21244", setHMG)) should be (empty)
  }

  "IP-PF resolveBySpecies" should "work with alternate lookups" in {
    (ipPF.resolveBySpecies("pthr21244", setH)) should be (defined)
    (ipPF.resolveBySpecies("PTHR21244", setH)) should be (defined)
    (ipPF.resolveBySpecies("pthr21244_human", setH)) should be (defined)
    (ipPF.resolveBySpecies("PTHR21244_human", setH)) should be (defined)
    (ipPF.resolveBySpecies("pthr21244 family", setH)) should be (defined)
    (ipPF.resolveBySpecies("PTHR21244 family", setH)) should be (defined)

    (ipPF.resolveBySpecies("pthr21244", setHM)) should be (defined)
    (ipPF.resolveBySpecies("pthr21244", setHM).get.size) should be (4)
    (ipPF.resolveBySpecies("pthr21244_human", setHM)) should be (defined)
    (ipPF.resolveBySpecies("pthr21244_human", setHM).get.size) should be (4)
    (ipPF.resolveBySpecies("pthr21244 family", setHM)) should be (defined)
    (ipPF.resolveBySpecies("pthr21244 family", setHM).get.size) should be (4)

    (ipPF.resolveBySpecies("pthr21244", setHMG)) should be (defined)
    (ipPF.resolveBySpecies("pthr21244", setHMG).get.size) should be (4)
    (ipPF.resolveBySpecies("pthr21244_human", setHMG)) should be (defined)
    (ipPF.resolveBySpecies("pthr21244_human", setHMG).get.size) should be (4)
    (ipPF.resolveBySpecies("pthr21244 family", setHMG)) should be (defined)
    (ipPF.resolveBySpecies("pthr21244 family", setHMG).get.size) should be (4)

    (ipPF.resolveBySpecies("hk", Set("saccharomyces cerevisiae", "ant"))) should be (defined)
    (ipPF.resolveBySpecies("hk", Set("ant", "saccharomyces cerevisiae"))) should be (defined)
    (ipPF.resolveBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).get.size) should be (5)
  }

  "IP-PF resolveHuman" should "fail despite alternate lookups" in {
    // key not in KB:
    (ipPF.resolveHuman("NotInKB")) should be (empty)
    (ipPF.resolveHuman("NotInKB_human")) should be (empty)
    (ipPF.resolveHuman("NotInKB protein")) should be (empty)
    (ipPF.resolveHuman("NotInKB family")) should be (empty)
    (ipPF.resolveHuman("mutant-NotInKB")) should be (empty)
    // entry does not have human species (yeast only):
    (ipPF.resolveHuman("hk")) should be (empty)
    (ipPF.resolveHuman("hk_human")) should be (empty)
    (ipPF.resolveHuman("hk protein")) should be (empty)
    (ipPF.resolveHuman("hk family")) should be (empty)
    (ipPF.resolveHuman("mutant-hk")) should be (empty)
    // protein key transforms not applicable for protein families:
    (ipPF.resolveHuman("pthr21244 protein")) should be (empty)
    (ipPF.resolveHuman("PTHR21244 protein")) should be (empty)
    (ipPF.resolveHuman("mutant-pthr21244")) should be (empty)
    (ipPF.resolveHuman("mutant-PTHR21244")) should be (empty)
  }

  "IP-PF resolveHuman" should "work with alternate lookups" in {
    (ipPF.resolveHuman("pthr21244")) should be (defined)
    (ipPF.resolveHuman("PTHR21244")) should be (defined)
    (ipPF.resolveHuman("pthr21244_human")) should be (defined)
    (ipPF.resolveHuman("PTHR21244_human")) should be (defined)
    (ipPF.resolveHuman("pthr21244 family")) should be (defined)
    (ipPF.resolveHuman("PTHR21244 family")) should be (defined)
  }

  // this KB includes species, therefore resolveNoSpecies should always fail:
  "IP-PF resolveNoSpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (ipPF.resolveNoSpecies("NOTINKB")) should be (empty)
    (ipPF.resolveNoSpecies("notinkb")) should be (empty)
    (ipPF.resolveNoSpecies("notinkb_human")) should be (empty)
    (ipPF.resolveNoSpecies("notinkb protein")) should be (empty)
    (ipPF.resolveNoSpecies("notinkb family")) should be (empty)
    // entry has a species:
    (ipPF.resolveNoSpecies("PTHR21244")) should be (empty)
    (ipPF.resolveNoSpecies("pthr21244")) should be (empty)
    (ipPF.resolveNoSpecies("pthr21244_human")) should be (empty)
    (ipPF.resolveNoSpecies("pthr21244 protein")) should be (empty)
    (ipPF.resolveNoSpecies("pthr21244 family")) should be (empty)
    (ipPF.resolveNoSpecies("mutant-pthr21244")) should be (empty)
    (ipPF.resolveNoSpecies("hk")) should be (empty)
    (ipPF.resolveNoSpecies("hk_human")) should be (empty)
    (ipPF.resolveNoSpecies("hk protein")) should be (empty)
    (ipPF.resolveNoSpecies("hk family")) should be (empty)
    (ipPF.resolveNoSpecies("mutant-hk")) should be (empty)
  }


  val bePF = new BEProtFamKBL               // defined after this class (LOOK BELOW)
  // println("== BE_PF ==========================================================")
  // bePF.memoryKB.dump                        // DEBUGGING

  "BE-PF" should "should be marked as family grounded but not protein grounded" in {
    val txtU = "PTHR21244 is cool."
    val menU = getBioMentions(txtU).head
    val txtL = "pthr21244 is also cool."
    val menL = getBioMentions(txtL).head

    (isFamilyGrounded(menU)) should be (true)
    (isFamilyGrounded(menL)) should be (true)
    (isProteinGrounded(menU)) should be (false)
    (isProteinGrounded(menL)) should be (false)
  }

  "BE-PF resolve" should "work for protein family Bioentities" in {
    (bePF.resolve("14-3-3 proteins")) should be (defined) // first entry
    (bePF.resolve("ABL")) should be (defined)
    //(bePF.resolve("ABL_family")) should be (defined)
    (bePF.resolve("AMPK")) should be (defined)
    (bePF.resolve("AMPKalpha")) should be (defined)
    //(bePF.resolve("AMPK_alpha")) should be (defined)
    //(bePF.resolve("AMPK alpha")) should be (defined)
    //(bePF.resolve("ampk Alpha")) should be (defined)
    //(bePF.resolve("Death_receptor")) should be (defined)
    //(bePF.resolve("death receptor")) should be (defined)
    (bePF.resolve("ERK")) should be (defined)
    (bePF.resolve("Erk")) should be (defined)
    (bePF.resolve("erk")) should be (defined)
    (bePF.resolve("ERK 1/2")) should be (defined)
    (bePF.resolve("ERK1/2")) should be (defined)
    (bePF.resolve("Erk1/2")) should be (defined)
    (bePF.resolve("Erk-1/2")) should be (defined)
    (bePF.resolve("HDAC")) should be (defined)
    //(bePF.resolve("HDAC_I")) should be (defined)
    //(bePF.resolve("HDAC_II")) should be (defined)
    //(bePF.resolve("HDAC_III")) should be (defined)
    //(bePF.resolve("HDAC_IV")) should be (defined)
    (bePF.resolve("IKappaB kinase")) should be (defined)
    (bePF.resolve("IkappaB kinase")) should be (defined)
    (bePF.resolve("IKK")) should be (defined)
    (bePF.resolve("IKK_family")) should be (defined)
    (bePF.resolve("inhibin")) should be (defined)
    //(bePF.resolve("Sulfonylurea_receptor")) should be (defined)
    (bePF.resolve("VEGF")) should be (defined)
    (bePF.resolve("VEGFR")) should be (defined)
    (bePF.resolve("WNT")) should be (defined) // last entry
  }

  "BE-PF resolve" should "fail despite alternate lookups" in {
    (bePF.resolve("NOTINKB")) should be (empty) // keys not in KB
    (bePF.resolve("notinkb")) should be (empty)
    (bePF.resolve("notinkb_human")) should be (empty)
    (bePF.resolve("notinkb protein")) should be (empty)
    (bePF.resolve("notinkb family")) should be (empty)
    (bePF.resolve("mutant-acad")) should be (empty) // mutant pattern not matched
    (bePF.resolve("ERK_family")) should be (empty)  // _family is allowed but this one not in KB
    (bePF.resolve("Erk_family")) should be (empty)
    (bePF.resolve("erk_family")) should be (empty)
    (bePF.resolve("ERK_FAMILY")) should be (empty)
    (bePF.resolve("Erk_FAMILY")) should be (empty)
    (bePF.resolve("erk_FAMILY")) should be (empty)
    (bePF.resolve("ERK1/2_family")) should be (empty)
    (bePF.resolve("Erk1/2_family")) should be (empty)
    (bePF.resolve("erk1/2_family")) should be (empty)
    (bePF.resolve("ERK-1/2_family")) should be (empty)
    (bePF.resolve("Erk-1/2_family")) should be (empty)
    (bePF.resolve("erk-1/2_family")) should be (empty)
  }

  "BE-PF resolve" should "work via alternate lookups" in {
    (bePF.resolve("4EBP family")) should be (defined)
    (bePF.resolve("ABL family")) should be (defined)
    //(bePF.resolve("ABL_family family")) should be (defined) // _family should not be stripped
    //(bePF.resolve("ABL_family protein family")) should be (defined) // _family should not be stripped
    (bePF.resolve("ERK family")) should be (defined)
    (bePF.resolve("Erk family")) should be (defined)
    (bePF.resolve("erk family")) should be (defined)
    (bePF.resolve("ERK 1/2 family")) should be (defined)
    (bePF.resolve("Erk 1/2 family")) should be (defined)
    (bePF.resolve("erk 1/2 family")) should be (defined)
    (bePF.resolve("ERK1/2 family")) should be (defined)
    (bePF.resolve("Erk1/2 family")) should be (defined)
    (bePF.resolve("erk1/2 family")) should be (defined)
    (bePF.resolve("ERK-1/2 family")) should be (defined)
    (bePF.resolve("Erk-1/2 family")) should be (defined)
    (bePF.resolve("erk-1/2 family")) should be (defined)
    (bePF.resolve("IkappaB kinase family")) should be (defined)
    (bePF.resolve("inhibin family")) should be (defined)
    (bePF.resolve("Sulfonylurea_receptor family")) should be (defined)
    (bePF.resolve("WNT family")) should be (defined)
  }

  "BE-PF resolve" should "work for protein complexes despite NER override" in {
    // in NER Override only:
    (bePF.resolve("ACOX")) should be (defined)
    (bePF.resolve("BMP")) should be (defined)
    (bePF.resolve("Cadherin")) should be (defined)
    (bePF.resolve("COX4")) should be (defined)
    (bePF.resolve("COX6A")) should be (defined)
    (bePF.resolve("COX6B")) should be (defined)
    (bePF.resolve("COX8")) should be (defined)
    (bePF.resolve("CRISP")) should be (defined)
    (bePF.resolve("MAF")) should be (defined)
    (bePF.resolve("NOTCH")) should be (defined)
    (bePF.resolve("PKI")) should be (defined)
    (bePF.resolve("RAS")) should be (defined)
  }

  "BE-PF family resolve" should "fail for protein complex Bioentities" in {
    // Complexes, not families:
    (bePF.resolve("ACC")) should be (defined)
    (bePF.resolve("COX")) should be (empty)
    (bePF.resolve("Cox")) should be (empty)
    (bePF.resolve("cox")) should be (empty)
    (bePF.resolve("Fibrinogen")) should be (defined)
    (bePF.resolve("PI3K")) should be (defined)
  }

}


// InterPro Protein Family KB using alternate protein family resolutions
class IPProtFamKBL extends IMKBLookup {
  val meta = new IMKBMetaInfo(
    kbFilename = Some(StaticProteinFamily2Filename),
    namespace = "interpro",
    baseURI = "http://identifiers.org/interpro/",
    resourceId = "MIR:00000011",
    hasSpeciesInfo = true,
    isFamilyKB = true
  )
  // println(s"IP-KB.metaInfo=${memoryKB.metaInfo}")
  val keyTransforms = KBKeyTransformsGroup(DefaultKeyTransforms, FamilyAuxKeyTransforms, DefaultKeyTransforms)
  memoryKB = (new TsvIMKBFactory).make(meta, keyTransforms)
}

// Bioentities Protein Family KB using alternate protein family resolutions
class BEProtFamKBL extends IMKBLookup {
  val meta = new IMKBMetaInfo(
    kbFilename = Some(StaticProteinFamilyOrComplexFilename),
    namespace = "be",
    baseURI = "https://identifiers.org/fplx/",
    isFamilyKB = true
  )
  // println(s"BE-KB.metaInfo=${memoryKB.metaInfo}")
  val keyTransforms = KBKeyTransformsGroup(DefaultKeyTransforms, FamilyAuxKeyTransforms, DefaultKeyTransforms)
  memoryKB = (new TsvIMKBFactory).make(meta, keyTransforms)
}
