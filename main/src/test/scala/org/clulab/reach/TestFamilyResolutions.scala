package org.clulab.reach

import scala.util.Try

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.mentions._
import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBUtils._

/**
  * Unit tests to ensure alternate resolutions are working for KB grounding.
  *   Written by: Tom Hicks. 11/4/2015.
  *   Last Modified: Add tests for Bioentities protein family KB.
  */
class TestFamilyResolutions extends FlatSpec with Matchers {

  val ipPF = new IPProtFamKBL           // defined after this class (LOOK BELOW)

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
    (ipPF.resolve("NOTINKB").isDefined) should be (false)
    (ipPF.resolve("notinkb").isDefined) should be (false)
    (ipPF.resolve("notinkb_human").isDefined) should be (false)
    (ipPF.resolve("notinkb protein").isDefined) should be (false)
    (ipPF.resolve("notinkb family").isDefined) should be (false)
    // protein key transforms not applicable for protein families:
    (ipPF.resolve("pthr21244 protein").isDefined) should be (false)
    (ipPF.resolve("mutant-pthr21244").isDefined) should be (false)
    (ipPF.resolve("hk protein").isDefined) should be (false)
    (ipPF.resolve("mutant-hk").isDefined) should be (false)
  }

  "IP-PF resolve" should "work with alternate family lookups" in {
    (ipPF.resolve("PTHR21244").isDefined) should be (true)
    (ipPF.resolve("pthr21244").isDefined) should be (true)
    (ipPF.resolve("pthr21244_human").isDefined) should be (true)
    (ipPF.resolve("pthr21244 family").isDefined) should be (true)
    (ipPF.resolve("hk").isDefined) should be (true)
    (ipPF.resolve("hk_human").isDefined) should be (true)
    (ipPF.resolve("hk family").isDefined) should be (true)
  }

  "IP-PF resolveByASpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (ipPF.resolveByASpecies("NotInKB", "ant").isDefined) should be (false)
    (ipPF.resolveByASpecies("NotInKB_human", "ant").isDefined) should be (false)
    (ipPF.resolveByASpecies("NotInKB protein", "ant").isDefined) should be (false)
    (ipPF.resolveByASpecies("NotInKB family", "ant").isDefined) should be (false)
    (ipPF.resolveByASpecies("mutant-NotInKB", "ant").isDefined) should be (false)
    // entry does not have this species:
    (ipPF.resolveByASpecies("hk", "frog").isDefined) should be (false)
    (ipPF.resolveByASpecies("hk_human", "frog").isDefined) should be (false)
    (ipPF.resolveByASpecies("hk protein", "frog").isDefined) should be (false)
    (ipPF.resolveByASpecies("hk family", "frog").isDefined) should be (false)
    (ipPF.resolveByASpecies("mutant-hk", "frog").isDefined) should be (false)
    // protein key transforms not applicable for protein families:
    (ipPF.resolveByASpecies("pthr21244 protein", "human").isDefined) should be (false)
    (ipPF.resolveByASpecies("PTHR21244 protein", "human").isDefined) should be (false)
    (ipPF.resolveByASpecies("mutant-pthr21244", "human").isDefined) should be (false)
    (ipPF.resolveByASpecies("hk protein", "saccharomyces cerevisiae").isDefined) should be (false)
    (ipPF.resolveByASpecies("mutant-hk", "saccharomyces cerevisiae").isDefined) should be (false)
  }

  "IP-PF resolveByASpecies" should "work with alternate lookups" in {
    (ipPF.resolveByASpecies("pthr21244", "human").isDefined) should be (true)
    (ipPF.resolveByASpecies("pthr21244_human", "human").isDefined) should be (true)
    (ipPF.resolveByASpecies("pthr21244 family", "human").isDefined) should be (true)
    (ipPF.resolveByASpecies("hk", "saccharomyces cerevisiae").isDefined) should be (true)
    (ipPF.resolveByASpecies("hk_human", "saccharomyces cerevisiae").isDefined) should be (true)
    (ipPF.resolveByASpecies("hk family", "saccharomyces cerevisiae").isDefined) should be (true)
  }

  val setA =   Set("aardvark")
  val setF =   Set("frog")
  val setH =   Set("human")
  val setHM =  Set("human", "mouse")
  val setHMG = Set("human", "mouse", "gorilla")
  "IP-PF resolveBySpecies" should "should fail despite alternate lookups" in {
    // key not in KB:
    (ipPF.resolveBySpecies("NotInKB", setA).isDefined) should be (false)
    (ipPF.resolveBySpecies("NotInKB_human", setA).isDefined) should be (false)
    (ipPF.resolveBySpecies("NotInKB protein", setA).isDefined) should be (false)
    (ipPF.resolveBySpecies("NotInKB family", setA).isDefined) should be (false)
    (ipPF.resolveBySpecies("mutant-NotInKB", setA).isDefined) should be (false)
    (ipPF.resolveBySpecies("pthr21244 mouse", setH).isDefined) should be (false)
    // entry does not have this species:
    (ipPF.resolveBySpecies("pthr21244", setF).isDefined) should be (false)
    (ipPF.resolveBySpecies("pthr21244_human", setF).isDefined) should be (false)
    (ipPF.resolveBySpecies("pthr21244 protein", setF).isDefined) should be (false)
    (ipPF.resolveBySpecies("pthr21244 family", setF).isDefined) should be (false)
    (ipPF.resolveBySpecies("mutant-pthr21244", setF).isDefined) should be (false)
    // entry does not have these species (yeast only):
    (ipPF.resolveBySpecies("hk", setHM).isDefined) should be (false)
    (ipPF.resolveBySpecies("hk_human", setHM).isDefined) should be (false)
    (ipPF.resolveBySpecies("hk protein", setHM).isDefined) should be (false)
    (ipPF.resolveBySpecies("hk family", setHM).isDefined) should be (false)
    (ipPF.resolveBySpecies("mutant-hk", setHM).isDefined) should be (false)
    // protein key transforms not applicable for protein families:
    (ipPF.resolveBySpecies("pthr21244 protein", setH).isDefined) should be (false)
    (ipPF.resolveBySpecies("PTHR21244 protein", setH).isDefined) should be (false)
    (ipPF.resolveBySpecies("mutant-pthr21244", setH).isDefined) should be (false)
    (ipPF.resolveBySpecies("MUTANT-PTHR21244", setH).isDefined) should be (false)
    (ipPF.resolveBySpecies("pthr21244 protein", setHM).isDefined) should be (false)
    (ipPF.resolveBySpecies("mutant-pthr21244", setHM).isDefined) should be (false)
    (ipPF.resolveBySpecies("pthr21244 protein", setHMG).isDefined) should be (false)
    (ipPF.resolveBySpecies("mutant-pthr21244", setHMG).isDefined) should be (false)
  }

  "IP-PF resolveBySpecies" should "work with alternate lookups" in {
    (ipPF.resolveBySpecies("pthr21244", setH).isDefined) should be (true)
    (ipPF.resolveBySpecies("PTHR21244", setH).isDefined) should be (true)
    (ipPF.resolveBySpecies("pthr21244_human", setH).isDefined) should be (true)
    (ipPF.resolveBySpecies("PTHR21244_human", setH).isDefined) should be (true)
    (ipPF.resolveBySpecies("pthr21244 family", setH).isDefined) should be (true)
    (ipPF.resolveBySpecies("PTHR21244 family", setH).isDefined) should be (true)

    (ipPF.resolveBySpecies("pthr21244", setHM).isDefined) should be (true)
    (ipPF.resolveBySpecies("pthr21244", setHM).get.size == 2) should be (true)
    (ipPF.resolveBySpecies("pthr21244_human", setHM).isDefined) should be (true)
    (ipPF.resolveBySpecies("pthr21244_human", setHM).get.size == 2) should be (true)
    (ipPF.resolveBySpecies("pthr21244 family", setHM).isDefined) should be (true)
    (ipPF.resolveBySpecies("pthr21244 family", setHM).get.size == 2) should be (true)

    (ipPF.resolveBySpecies("pthr21244", setHMG).isDefined) should be (true)
    (ipPF.resolveBySpecies("pthr21244", setHMG).get.size == 2) should be (true)
    (ipPF.resolveBySpecies("pthr21244_human", setHMG).isDefined) should be (true)
    (ipPF.resolveBySpecies("pthr21244_human", setHMG).get.size == 2) should be (true)
    (ipPF.resolveBySpecies("pthr21244 family", setHMG).isDefined) should be (true)
    (ipPF.resolveBySpecies("pthr21244 family", setHMG).get.size == 2) should be (true)

    (ipPF.resolveBySpecies("hk", Set("saccharomyces cerevisiae", "ant")).isDefined) should be (true)
    (ipPF.resolveBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).isDefined) should be (true)
    (ipPF.resolveBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).get.size == 1) should be (true)
  }

  "IP-PF resolveHuman" should "fail despite alternate lookups" in {
    // key not in KB:
    (ipPF.resolveHuman("NotInKB").isDefined) should be (false)
    (ipPF.resolveHuman("NotInKB_human").isDefined) should be (false)
    (ipPF.resolveHuman("NotInKB protein").isDefined) should be (false)
    (ipPF.resolveHuman("NotInKB family").isDefined) should be (false)
    (ipPF.resolveHuman("mutant-NotInKB").isDefined) should be (false)
    // entry does not have human species (yeast only):
    (ipPF.resolveHuman("hk").isDefined) should be (false)
    (ipPF.resolveHuman("hk_human").isDefined) should be (false)
    (ipPF.resolveHuman("hk protein").isDefined) should be (false)
    (ipPF.resolveHuman("hk family").isDefined) should be (false)
    (ipPF.resolveHuman("mutant-hk").isDefined) should be (false)
    // protein key transforms not applicable for protein families:
    (ipPF.resolveHuman("pthr21244 protein").isDefined) should be (false)
    (ipPF.resolveHuman("PTHR21244 protein").isDefined) should be (false)
    (ipPF.resolveHuman("mutant-pthr21244").isDefined) should be (false)
    (ipPF.resolveHuman("mutant-PTHR21244").isDefined) should be (false)
  }

  "IP-PF resolveHuman" should "work with alternate lookups" in {
    (ipPF.resolveHuman("pthr21244").isDefined) should be (true)
    (ipPF.resolveHuman("PTHR21244").isDefined) should be (true)
    (ipPF.resolveHuman("pthr21244_human").isDefined) should be (true)
    (ipPF.resolveHuman("PTHR21244_human").isDefined) should be (true)
    (ipPF.resolveHuman("pthr21244 family").isDefined) should be (true)
    (ipPF.resolveHuman("PTHR21244 family").isDefined) should be (true)
  }

  // this KB includes species, therefore resolveNoSpecies should always fail:
  "IP-PF resolveNoSpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (ipPF.resolveNoSpecies("NOTINKB").isDefined) should be (false)
    (ipPF.resolveNoSpecies("notinkb").isDefined) should be (false)
    (ipPF.resolveNoSpecies("notinkb_human").isDefined) should be (false)
    (ipPF.resolveNoSpecies("notinkb protein").isDefined) should be (false)
    (ipPF.resolveNoSpecies("notinkb family").isDefined) should be (false)
    // entry has a species:
    (ipPF.resolveNoSpecies("PTHR21244").isDefined) should be (false)
    (ipPF.resolveNoSpecies("pthr21244").isDefined) should be (false)
    (ipPF.resolveNoSpecies("pthr21244_human").isDefined) should be (false)
    (ipPF.resolveNoSpecies("pthr21244 protein").isDefined) should be (false)
    (ipPF.resolveNoSpecies("pthr21244 family").isDefined) should be (false)
    (ipPF.resolveNoSpecies("mutant-pthr21244").isDefined) should be (false)
    (ipPF.resolveNoSpecies("hk").isDefined) should be (false)
    (ipPF.resolveNoSpecies("hk_human").isDefined) should be (false)
    (ipPF.resolveNoSpecies("hk protein").isDefined) should be (false)
    (ipPF.resolveNoSpecies("hk family").isDefined) should be (false)
    (ipPF.resolveNoSpecies("mutant-hk").isDefined) should be (false)
  }


  val bePF = new BEProtFamKBL               // defined after this class (LOOK BELOW)

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

  "BE-PF resolve" should "fail despite alternate lookups" in {
    // keys not in KB:
    (bePF.resolve("NOTINKB")) should be (empty)
    (bePF.resolve("notinkb")) should be (empty)
    (bePF.resolve("notinkb_human")) should be (empty)
    (bePF.resolve("notinkb protein")) should be (empty)
    (bePF.resolve("notinkb family")) should be (empty)
    (bePF.resolve("mutant-acad")) should be (empty) // mutant pattern not matched
  }

  "BE-PF resolve" should "work for protein family Bioentities" in {
    (bePF.resolve("14-3-3 proteins")) should be (defined) // first entry
    (bePF.resolve("ABL")) should be (defined)
    (bePF.resolve("ABL_family")) should be (defined)
    (bePF.resolve("AMPK")) should be (defined)
    (bePF.resolve("AMPKalpha")) should be (defined)
    (bePF.resolve("AMPK_alpha")) should be (defined)
    (bePF.resolve("AMPK alpha")) should be (defined)
    (bePF.resolve("ampk Alpha")) should be (defined)
    (bePF.resolve("Death_receptor")) should be (defined)
    (bePF.resolve("death receptor")) should be (defined)
    (bePF.resolve("ERK")) should be (defined)
    (bePF.resolve("Erk")) should be (defined)
    (bePF.resolve("erk")) should be (defined)
    (bePF.resolve("ERK 1/2")) should be (defined)
    (bePF.resolve("ERK1/2")) should be (defined)
    (bePF.resolve("Erk1/2")) should be (defined)
    (bePF.resolve("Erk-1/2")) should be (defined)
    (bePF.resolve("HDAC")) should be (defined)
    (bePF.resolve("HDAC_I")) should be (defined)
    (bePF.resolve("HDAC_II")) should be (defined)
    (bePF.resolve("HDAC_III")) should be (defined)
    (bePF.resolve("HDAC_IV")) should be (defined)
    (bePF.resolve("IKappaB kinase")) should be (defined)
    (bePF.resolve("IkappaB kinase")) should be (defined)
    (bePF.resolve("IKK")) should be (defined)
    (bePF.resolve("IKK_family")) should be (defined)
    (bePF.resolve("inhibin")) should be (defined)
    (bePF.resolve("Sulfonylurea_receptor")) should be (defined)
    (bePF.resolve("VEGF")) should be (defined)
    (bePF.resolve("VEGFR")) should be (defined)
    (bePF.resolve("WNT")) should be (defined) // last entry
  }

  "BE-PF resolve" should "work via alternate lookups" in {
    (bePF.resolve("4EBP family")) should be (defined)
    (bePF.resolve("ABL family")) should be (defined)
    (bePF.resolve("ABL_family family")) should be (defined)
    (bePF.resolve("ERK family")) should be (defined)
    (bePF.resolve("Erk family")) should be (defined)
    (bePF.resolve("erk family")) should be (defined)
    (bePF.resolve("ERK 1/2 family")) should be (defined)
    (bePF.resolve("ERK1/2 family")) should be (defined)
    (bePF.resolve("Erk1/2 family")) should be (defined)
    (bePF.resolve("Erk-1/2 family")) should be (defined)
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
    (bePF.resolve("ACC")) should be (empty)
    (bePF.resolve("COX")) should be (empty)
    (bePF.resolve("Cox")) should be (empty)
    (bePF.resolve("cox")) should be (empty)
    (bePF.resolve("Fibrinogen")) should be (empty)
    (bePF.resolve("PI3K")) should be (empty)
  }

}


// InterPro Protein Family KB using alternate protein family resolutions
class IPProtFamKBL extends IMKBFamilyLookup {
  val meta = new IMKBMetaInfo("http://identifiers.org/interpro/", "MIR:00000011")
  meta.put("family", "true")                // mark as from a protein family KB
  memoryKB = (new TsvIMKBFactory).make("interpro", StaticProteinFamily2Filename, true, meta)
  // println(s"IP-KB.metaInfo=${memoryKB.metaInfo}")
}

// Bioentities Protein Family KB using alternate protein family resolutions
class BEProtFamKBL extends IMKBFamilyLookup {
  val meta = new IMKBMetaInfo("https://github.com/sorgerlab/bioentities")
  meta.put("family", "true")                // mark as from a protein family KB
  memoryKB = (new TsvIMKBFactory).make("be", StaticProteinFamily0Filename, meta)
  // println(s"BE-KB.metaInfo=${memoryKB.metaInfo}")
}
