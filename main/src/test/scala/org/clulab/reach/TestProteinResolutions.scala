package org.clulab.reach

import scala.util.Try
import org.scalatest._

import TestUtils._

import org.clulab.reach.mentions._
import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachKBUtils._

/**
  * Unit tests to ensure alternate resolutions are working for KB grounding.
  *   Written by: Tom Hicks. 11/16/2015.
  *   Last Modified: Update for GNA transform enhancement.
  */
class TestProteinResolutions extends FlatSpec with Matchers {

  val imkbP = new TestProteinKBL           // defined after this class (LOOK BELOW)
  // imkbP.memoryKB.dump                        // DEBUGGING

  "ProteinKBL resolves" should "should be marked as protein grounded but not family grounded" in {
    val txtU = "PTHR2 is cool."
    val menU = getBioMentions(txtU).head
    val txtL = "pthr2 is also cool."
    val menL = getBioMentions(txtL).head

    (isProteinGrounded(menU)) should be (true)
    (isProteinGrounded(menL)) should be (true)
    (isFamilyGrounded(menU)) should be (false)
    (isFamilyGrounded(menL)) should be (false)
  }

  "ProteinKBL resolve" should "fail despite alternate lookups" in {
    // keys not in KB:
    (imkbP.resolve("NOTINKB")) should be (empty)
    (imkbP.resolve("notinkb")) should be (empty)
    (imkbP.resolve("notinkb_human")) should be (empty)
    (imkbP.resolve("notinkb protein")) should be (empty)
    (imkbP.resolve("notinkb family")) should be (empty)
    // family key transforms not applicable for proteins:
    (imkbP.resolve("pthr2 family")) should be (empty)
    (imkbP.resolve("zyx-1 family")) should be (empty)
  }

  "ProteinKBL resolve" should "work with alternate lookups" in {
    (imkbP.resolve("PTHR2")) should be (defined)
    (imkbP.resolve("pthr2")) should be (defined)
    (imkbP.resolve("pthr2_human")) should be (defined)
    (imkbP.resolve("pthr2 protein")) should be (defined)
    (imkbP.resolve("mutant-pthr2")) should be (defined)
    (imkbP.resolve("mutant-zyx-1")) should be (defined)
    (imkbP.resolve("zyx-1")) should be (defined)
    (imkbP.resolve("zyx-1_human")) should be (defined)
    (imkbP.resolve("zyx-1 protein")) should be (defined)
    (imkbP.resolve("STAT1")) should be (defined)
    (imkbP.resolve("stat1")) should be (defined)
    (imkbP.resolve("STBA")) should be (defined)
    (imkbP.resolve("stba")) should be (defined)
    (imkbP.resolve("SMAD2")) should be (defined)
    (imkbP.resolve("Smad2")) should be (defined)
    (imkbP.resolve("smad2")) should be (defined)
    (imkbP.resolve("SMAD 2")) should be (defined)
    (imkbP.resolve("Smad 2")) should be (defined)
    (imkbP.resolve("smad 2")) should be (defined)
    (imkbP.resolve("SMAD-2")) should be (defined)
    (imkbP.resolve("Smad-2")) should be (defined)
    (imkbP.resolve("smad-2")) should be (defined)
  }

  "ProteinKBL resolve" should "work via protein domain lookup" in {
    (imkbP.resolve("PI3Kbeta-RBD")) should be (defined)
    (imkbP.resolve("pi3kbeta-rbd")) should be (defined)
    (imkbP.resolve("PI3Kbeta-DSS1_SEM1")) should be (defined)
    (imkbP.resolve("pi3kbeta-dss1_sem1")) should be (defined)
    (imkbP.resolve("PTHR2-ZU5")) should be (defined)
    (imkbP.resolve("pthr2-zu5")) should be (defined)
    (imkbP.resolve("pthr2-DSS1_SEM1")) should be (defined)
  }

  "ProteinKBL resolve" should "fail despite protein domain lookup" in {
    (imkbP.resolve("NotInKB-RBD")) should be (empty) // protein not in KB
    (imkbP.resolve("PI3KC2b-RBD")) should be (empty) // protein not in KB
    (imkbP.resolve("zyx-1-rbd")) should be (empty) // pre-key text fails pattern match
    (imkbP.resolve("PI3K-C2-alpha-RBD")) should be (empty) // pre-key text fails pattern match
  }

  "ProteinKBL resolve" should "fail despite PTM prefix stripping" in {
    (imkbP.resolve("pNOTINKB")) should be (empty)
    (imkbP.resolve("pnotinkb")) should be (empty)
    (imkbP.resolve("uNOTINKB")) should be (empty)
    (imkbP.resolve("unotinkb")) should be (empty)
    // does not match restricted pattern (and not in KB without pattern matching)
    (imkbP.resolve("PSTAT1")) should be (empty)
    (imkbP.resolve("Pstat1")) should be (empty)
    (imkbP.resolve("pstat1")) should be (empty)
    (imkbP.resolve("uerk")) should be (empty)
    (imkbP.resolve("ustat1")) should be (empty)
    (imkbP.resolve("ustba")) should be (empty)
  }

  "ProteinKBL resolve" should "work with PTM prefix stripping" in {
    (imkbP.resolve("pSTAT1")) should be (defined)
    (imkbP.resolve("pSTBC")) should be (defined)
    (imkbP.resolve("pERK")) should be (defined)
    (imkbP.resolve("uERK")) should be (defined)
    (imkbP.resolve("uSTAT1")) should be (defined)
    (imkbP.resolve("uSTBA")) should be (defined)
  }


  "ProteinKBL resolve" should "fail despite gene name affix stripping" in {
    (imkbP.resolve("prefix-NOTINKB")) should be (empty)
    (imkbP.resolve("suffix-notinkb")) should be (empty)
    (imkbP.resolve("xxx-NOTINKB")) should be (empty)
    (imkbP.resolve("u-notinkb")) should be (empty)
    (imkbP.resolve("rAAV")) should be (empty)            // only non-entity prefix
    (imkbP.resolve("EGFP-MCHY")) should be (empty)       // only non-entity prefixes
    (imkbP.resolve("shRNA")) should be (empty)           // only suffix
    (imkbP.resolve("KD-shRNA")) should be (empty)        // only suffixes
    (imkbP.resolve("Gfp-kd")) should be (empty)          // only prefix & suffix
    (imkbP.resolve("Gfp-SH-kd-shRNA")) should be (empty) // only prefixes & suffixes
    (imkbP.resolve("Yfp-virus-YFP")) should be (empty)   // no such protein
    (imkbP.resolve("KD-VIRUS-shRNA")) should be (empty)  // no such protein
    (imkbP.resolve("Gfp-Virus-kd")) should be (empty)    // no such protein
  }

  "ProteinKBL resolve" should "work with gene name affix stripping" in {
    (imkbP.resolve("activated-STAT1")) should be (defined)
    (imkbP.resolve("lent-STAT1")) should be (defined)
    (imkbP.resolve("lenti-STAT1")) should be (defined)
    (imkbP.resolve("lentivirus-STAT1")) should be (defined)
    (imkbP.resolve("Lent-STAT1")) should be (defined)
    (imkbP.resolve("Lenti-STAT1")) should be (defined)
    (imkbP.resolve("Lentivirus-STAT1")) should be (defined)
    (imkbP.resolve("Myc-STBC")) should be (defined)
    (imkbP.resolve("rAAV-STBA")) should be (defined)
    (imkbP.resolve("Yfp-STBA")) should be (defined)
    (imkbP.resolve("YFP-STBA")) should be (defined)
    (imkbP.resolve("phospho-ERK")) should be (defined)
    (imkbP.resolve("phosphorylated-ERK")) should be (defined)
    (imkbP.resolve("ERK-KD")) should be (defined)            // suffixed
    (imkbP.resolve("ERK-kd")) should be (defined)            // suffixed
    (imkbP.resolve("ERK-KD-SHRNA")) should be (defined)      // multiple suffixes
    (imkbP.resolve("ERK-kd-shrna")) should be (defined)      // multiple suffixes
    (imkbP.resolve("Myr-Flag-Akt1")) should be (defined)     // multiple prefix
    (imkbP.resolve("Myr-Flag-Akt-1")) should be (defined)    // multiple prefix
    (imkbP.resolve("Sh-Myr-Flag-Akt1")) should be (defined)  // multiple prefix
    (imkbP.resolve("SH-MYR-FLAG-Akt-1")) should be (defined) // multiple prefix
    (imkbP.resolve("GFP-Mchy-SH")) should be (defined)       // only prefixes
    (imkbP.resolve("GFP-KRAS-KD")) should be (defined)       // prefix & suffix
    (imkbP.resolve("GFP-KRAS-Kd")) should be (defined)       // prefix & suffix
    (imkbP.resolve("WT-Gfp-KRAS-Kd-shRNA")) should be (defined) // prefixes & suffixes
    (imkbP.resolve("WT-Gfp-KRAS-Kd-shRNA")) should be (defined) // prefixes & suffixes
  }


  "ProteinKBL resolveByASpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbP.resolveByASpecies("NotInKB", "ant")) should be (empty)
    (imkbP.resolveByASpecies("NotInKB_human", "ant")) should be (empty)
    (imkbP.resolveByASpecies("NotInKB protein", "ant")) should be (empty)
    (imkbP.resolveByASpecies("NotInKB family", "ant")) should be (empty)
    (imkbP.resolveByASpecies("mutant-NotInKB", "ant")) should be (empty)
    // entry does not have this species:
    (imkbP.resolveByASpecies("zyx-1", "frog")) should be (empty)
    (imkbP.resolveByASpecies("zyx-1_human", "frog")) should be (empty)
    (imkbP.resolveByASpecies("zyx-1 protein", "frog")) should be (empty)
    (imkbP.resolveByASpecies("zyx-1 family", "frog")) should be (empty)
    (imkbP.resolveByASpecies("mutant-zyx-1", "frog")) should be (empty)
    // family key transforms not applicable for proteins:
    (imkbP.resolveByASpecies("pthr2 family", "human")) should be (empty)
    (imkbP.resolveByASpecies("zyx-1 family", "caenorhabditis elegans")) should be (empty)
  }

  "ProteinKBL resolveByASpecies" should "work with alternate lookups" in {
    (imkbP.resolveByASpecies("pthr2", "human")) should be (defined)
    (imkbP.resolveByASpecies("pthr2_human", "human")) should be (defined)
    (imkbP.resolveByASpecies("pthr2 protein", "human")) should be (defined)
    (imkbP.resolveByASpecies("PTHR2 protein", "human")) should be (defined)
    (imkbP.resolveByASpecies("mutant-pthr2", "human")) should be (defined)
    (imkbP.resolveByASpecies("zyx-1", "caenorhabditis elegans")) should be (defined)
    (imkbP.resolveByASpecies("zyx-1_human", "caenorhabditis elegans")) should be (defined)
    (imkbP.resolveByASpecies("zyx-1 protein", "caenorhabditis elegans")) should be (defined)
    (imkbP.resolveByASpecies("mutant-zyx-1", "caenorhabditis elegans")) should be (defined)
  }

  "ProteinKBL resolveByASpecies" should "work via protein domain lookup" in {
    (imkbP.resolveByASpecies("PI3Kbeta-RBD", "human")) should be (defined)
    (imkbP.resolveByASpecies("pi3kbeta-rbd", "human")) should be (defined)
    (imkbP.resolveByASpecies("PI3Kbeta-DSS1_SEM1", "human")) should be (defined)
    (imkbP.resolveByASpecies("pi3kbeta-dss1_sem1", "human")) should be (defined)
    (imkbP.resolveByASpecies("PI3Kdelta-RBD", "mouse")) should be (defined)
    (imkbP.resolveByASpecies("pi3kdelta-rbd", "mouse")) should be (defined)
    (imkbP.resolveByASpecies("PI3Kdelta-DSS1_SEM1", "mouse")) should be (defined)
    (imkbP.resolveByASpecies("pi3kdelta-dss1_sem1", "mouse")) should be (defined)
  }

  "ProteinKBL resolveByASpecies" should "fail despite protein domain lookup" in {
    // proteins not in KB:
    (imkbP.resolveByASpecies("NotInKB-RBD", "human")) should be (empty)
    (imkbP.resolveByASpecies("PI3KC2b-RBD", "human")) should be (empty)
    // pre-key text fails pattern match:
    (imkbP.resolveByASpecies("zyx-1-rbd", "human")) should be (empty)
    (imkbP.resolveByASpecies("PI3K-C2-alpha-RBD", "human")) should be (empty)
    // wrong species:
    (imkbP.resolveByASpecies("PI3Kdelta-RBD", "rat")) should be (empty)
    (imkbP.resolveByASpecies("pi3kdelta-rbd", "rat")) should be (empty)
    (imkbP.resolveByASpecies("PI3Kdelta-DSS1_SEM1", "rat")) should be (empty)
    (imkbP.resolveByASpecies("pi3kdelta-dss1_sem1", "rat")) should be (empty)
  }

  "ProteinKBL resolveByASpecies" should "work in straightforward tests" in {
    (imkbP.resolveByASpecies("SMAD2", "human")) should be (defined)
    (imkbP.resolveByASpecies("Smad2", "human")) should be (defined)
    (imkbP.resolveByASpecies("smad2", "human")) should be (defined)
    (imkbP.resolveByASpecies("SMAD 2", "human")) should be (defined)
    (imkbP.resolveByASpecies("Smad 2", "human")) should be (defined)
    (imkbP.resolveByASpecies("smad 2", "human")) should be (defined)
    (imkbP.resolveByASpecies("SMAD-2", "human")) should be (defined)
    (imkbP.resolveByASpecies("Smad-2", "human")) should be (defined)
    (imkbP.resolveByASpecies("smad-2", "human")) should be (defined)
  }


  val setA =   Set("aardvark")
  val setF =   Set("frog")
  val setH =   Set("human")
  val setHM =  Set("human", "mouse")
  val setHMG = Set("human", "mouse", "gorilla")
  "ProteinKBL resolveBySpecies" should "should fail despite alternate lookups" in {
    // key not in KB:
    (imkbP.resolveBySpecies("NotInKB", setA)) should be (empty)
    (imkbP.resolveBySpecies("NotInKB_human", setA)) should be (empty)
    (imkbP.resolveBySpecies("NotInKB protein", setA)) should be (empty)
    (imkbP.resolveBySpecies("NotInKB family", setA)) should be (empty)
    (imkbP.resolveBySpecies("mutant-NotInKB", setA)) should be (empty)
    (imkbP.resolveBySpecies("pthr2 mouse", setH)) should be (empty)
    // entry does not have this species:
    (imkbP.resolveBySpecies("pthr2", setF)) should be (empty)
    (imkbP.resolveBySpecies("pthr2_human", setF)) should be (empty)
    (imkbP.resolveBySpecies("pthr2 protein", setF)) should be (empty)
    (imkbP.resolveBySpecies("pthr2 family", setF)) should be (empty)
    (imkbP.resolveBySpecies("mutant-pthr2", setF)) should be (empty)
    // entry does not have these species (nematode only):
    (imkbP.resolveBySpecies("zyx-1", setHM)) should be (empty)
    (imkbP.resolveBySpecies("zyx-1_human", setHM)) should be (empty)
    (imkbP.resolveBySpecies("zyx-1 protein", setHM)) should be (empty)
    (imkbP.resolveBySpecies("zyx-1 family", setHM)) should be (empty)
    (imkbP.resolveBySpecies("mutant-zyx-1", setHM)) should be (empty)
    // family key transforms not applicable for proteins:
    (imkbP.resolveBySpecies("pthr2 family", setH)) should be (empty)
    (imkbP.resolveBySpecies("PTHR2 family", setH)) should be (empty)
    (imkbP.resolveBySpecies("pthr2 family", setHM)) should be (empty)
    (imkbP.resolveBySpecies("pthr2 family", setHMG)) should be (empty)

  }

  "ProteinKBL resolveBySpecies" should "work with alternate lookups" in {
    (imkbP.resolveBySpecies("pthr2", setH)) should be (defined)
    (imkbP.resolveBySpecies("PTHR2", setH)) should be (defined)
    (imkbP.resolveBySpecies("pthr2_human", setH)) should be (defined)
    (imkbP.resolveBySpecies("PTHR2_human", setH)) should be (defined)
    (imkbP.resolveBySpecies("pthr2 protein", setH)) should be (defined)
    (imkbP.resolveBySpecies("PTHR2 protein", setH)) should be (defined)
    (imkbP.resolveBySpecies("mutant-pthr2", setH)) should be (defined)
    (imkbP.resolveBySpecies("MUTANT-PTHR2", setH)) should be (defined)

    (imkbP.resolveBySpecies("pthr2", setHM)) should be (defined)
    (imkbP.resolveBySpecies("pthr2", setHM).get) should have size 8
    (imkbP.resolveBySpecies("pthr2_human", setHM)) should be (defined)
    (imkbP.resolveBySpecies("pthr2_human", setHM).get) should have size 8
    (imkbP.resolveBySpecies("pthr2 protein", setHM)) should be (defined)
    (imkbP.resolveBySpecies("mutant-pthr2", setHM)) should be (defined)

    (imkbP.resolveBySpecies("pthr2", setHMG)) should be (defined)
    (imkbP.resolveBySpecies("pthr2", setHMG).get) should have size 8
    (imkbP.resolveBySpecies("pthr2_human", setHMG)) should be (defined)
    (imkbP.resolveBySpecies("pthr2_human", setHMG).get) should have size 8
    (imkbP.resolveBySpecies("pthr2 protein", setHMG)) should be (defined)
    (imkbP.resolveBySpecies("mutant-pthr2", setHMG)) should be (defined)

    (imkbP.resolveBySpecies("zyx-1", Set("caenorhabditis elegans", "ant"))) should be (defined)
    (imkbP.resolveBySpecies("zyx-1", Set("ant", "caenorhabditis elegans"))) should be (defined)
    (imkbP.resolveBySpecies("zyx-1", Set("ant", "caenorhabditis elegans")).get) should have size 3
  }

  "ProteinKBL resolveBySpecies" should "work via protein domain lookup" in {
    (imkbP.resolveBySpecies("PI3Kbeta-RBD", setHM)) should be (defined)
    (imkbP.resolveBySpecies("pi3kbeta-rbd", setHM)) should be (defined)
    (imkbP.resolveBySpecies("PI3Kbeta-DSS1_SEM1", setHM)) should be (defined)
    (imkbP.resolveBySpecies("pi3kbeta-dss1_sem1", setHM)) should be (defined)
    (imkbP.resolveBySpecies("PI3Kdelta-RBD", setHM)) should be (defined)
    (imkbP.resolveBySpecies("pi3kdelta-rbd", setHM)) should be (defined)
    (imkbP.resolveBySpecies("PI3Kdelta-DSS1_SEM1", setHM)) should be (defined)
    (imkbP.resolveBySpecies("pi3kdelta-dss1_sem1", setHM)) should be (defined)
  }

  "ProteinKBL resolveBySpecies" should "fail despite protein domain lookup" in {
    // proteins not in KB:
    (imkbP.resolveBySpecies("NotInKB-RBD", setHM)) should be (empty)
    (imkbP.resolveBySpecies("PI3KC2b-RBD", setHM)) should be (empty)
    // pre-key text fails pattern match:
    (imkbP.resolveBySpecies("zyx-1-rbd", setHM)) should be (empty)
    (imkbP.resolveBySpecies("PI3K-C2-alpha-RBD", setHM)) should be (empty)
    // wrong species:
    (imkbP.resolveBySpecies("PI3Kdelta-RBD", setF)) should be (empty)
    (imkbP.resolveBySpecies("pi3kdelta-rbd", setF)) should be (empty)
    (imkbP.resolveBySpecies("PI3Kdelta-DSS1_SEM1", setF)) should be (empty)
    (imkbP.resolveBySpecies("pi3kdelta-dss1_sem1", setF)) should be (empty)
  }


  "ProteinKBL resolveHuman" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbP.resolveHuman("NotInKB")) should be (empty)
    (imkbP.resolveHuman("NotInKB_human")) should be (empty)
    (imkbP.resolveHuman("NotInKB protein")) should be (empty)
    (imkbP.resolveHuman("NotInKB family")) should be (empty)
    (imkbP.resolveHuman("mutant-NotInKB")) should be (empty)
    // entry does not have human species (nematode only):
    (imkbP.resolveHuman("zyx-1")) should be (empty)
    (imkbP.resolveHuman("zyx-1_human")) should be (empty)
    (imkbP.resolveHuman("zyx-1 protein")) should be (empty)
    (imkbP.resolveHuman("zyx-1 family")) should be (empty)
    (imkbP.resolveHuman("mutant-zyx-1")) should be (empty)
    // family key transforms not applicable for proteins:
    (imkbP.resolveHuman("pthr2 family")) should be (empty)
    (imkbP.resolveHuman("PTHR2 family")) should be (empty)
  }

  "ProteinKBL resolveHuman" should "work with alternate lookups" in {
    (imkbP.resolveHuman("pthr2")) should be (defined)
    (imkbP.resolveHuman("PTHR2")) should be (defined)
    (imkbP.resolveHuman("pthr2_human")) should be (defined)
    (imkbP.resolveHuman("PTHR2_human")) should be (defined)
    (imkbP.resolveHuman("pthr2 protein")) should be (defined)
    (imkbP.resolveHuman("PTHR2 protein")) should be (defined)
    (imkbP.resolveHuman("mutant-pthr2")) should be (defined)
    (imkbP.resolveHuman("mutant-PTHR2")) should be (defined)
  }

  "ProteinKBL resolveHuman" should "work via protein domain lookup" in {
    (imkbP.resolveHuman("PI3Kbeta-RBD")) should be (defined)
    (imkbP.resolveHuman("pi3kbeta-rbd")) should be (defined)
    (imkbP.resolveHuman("PI3Kbeta-DSS1_SEM1")) should be (defined)
    (imkbP.resolveHuman("pi3kbeta-dss1_sem1")) should be (defined)
    (imkbP.resolveHuman("PTHR2-ZU5")) should be (defined)
    (imkbP.resolveHuman("pthr2-ZU5")) should be (defined)
    (imkbP.resolveHuman("pthr2-DSS1_SEM1")) should be (defined)
  }

  "ProteinKBL resolveHuman" should "fail despite protein domain lookup" in {
    // proteins not in KB:
    (imkbP.resolveHuman("NotInKB-RBD")) should be (empty)
    (imkbP.resolveHuman("PI3KC2b-RBD")) should be (empty)
    // pre-key text fails pattern match:
    (imkbP.resolveHuman("zyx-1-rbd")) should be (empty)
    (imkbP.resolveHuman("PI3K-C2-alpha-RBD")) should be (empty)
  }

  "ProteinKBL resolveHuman" should "work in straightforward tests" in {
    (imkbP.resolveHuman("SMAD2")) should be (defined)
    (imkbP.resolveHuman("Smad2")) should be (defined)
    (imkbP.resolveHuman("smad2")) should be (defined)
    (imkbP.resolveHuman("SMAD 2")) should be (defined)
    (imkbP.resolveHuman("Smad 2")) should be (defined)
    (imkbP.resolveHuman("smad 2")) should be (defined)
    (imkbP.resolveHuman("SMAD-2")) should be (defined)
    (imkbP.resolveHuman("Smad-2")) should be (defined)
    (imkbP.resolveHuman("smad-2")) should be (defined)
  }


  // this KB includes species, therefore resolveNoSpecies should always fail:
  "ProteinKBL resolveNoSpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbP.resolveNoSpecies("NOTINKB")) should be (empty)
    (imkbP.resolveNoSpecies("notinkb")) should be (empty)
    (imkbP.resolveNoSpecies("notinkb_human")) should be (empty)
    (imkbP.resolveNoSpecies("notinkb protein")) should be (empty)
    (imkbP.resolveNoSpecies("notinkb family")) should be (empty)
    (imkbP.resolveNoSpecies("notinkb-RBD")) should be (empty)
    // entry has a species:
    (imkbP.resolveNoSpecies("PTHR2")) should be (empty)
    (imkbP.resolveNoSpecies("pthr2")) should be (empty)
    (imkbP.resolveNoSpecies("pthr2_human")) should be (empty)
    (imkbP.resolveNoSpecies("pthr2 protein")) should be (empty)
    (imkbP.resolveNoSpecies("pthr2 family")) should be (empty)
    (imkbP.resolveNoSpecies("mutant-pthr2")) should be (empty)
    (imkbP.resolveNoSpecies("zyx-1")) should be (empty)
    (imkbP.resolveNoSpecies("zyx-1_human")) should be (empty)
    (imkbP.resolveNoSpecies("zyx-1 protein")) should be (empty)
    (imkbP.resolveNoSpecies("zyx-1 family")) should be (empty)
    (imkbP.resolveNoSpecies("mutant-zyx-1")) should be (empty)
    (imkbP.resolveNoSpecies("PI3Kbeta-RBD")) should be (empty)
    (imkbP.resolveNoSpecies("PTHR2-ZU5")) should be (empty)
  }

}

// Protein KB using alternate protein resolutions
class TestProteinKBL extends IMKBLookup {
  val meta = new IMKBMetaInfo(
    kbFilename = Some(StaticProteinFilename),
    namespace = "uniprot",
    baseURI = "http://identifiers.org/uniprot/",
    resourceId = "MIR:00100164",
    hasSpeciesInfo = true,
    isProteinKB = true
  )
  val keyTransforms = KBKeyTransformsGroup(CasedKeyTransforms, ProteinAuxKeyTransforms, CasedKeyTransforms)
  memoryKB = (new TsvIMKBFactory).make(meta, keyTransforms)
}
