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

  val imkbP0F = new TestProteinKBL(StaticProteinFilename)          // defined after this class (LOOK BELOW)
  val imkbPGP = new TestProteinKBL(StaticProteinFilename2)
  val imkbPQZ = new TestProteinKBL(StaticProteinFilename3)
  // imkbPGP.memoryKB.dump                        // DEBUGGING

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
    (imkbPGP.resolve("NOTINKB")) should be (empty)
    (imkbPGP.resolve("notinkb")) should be (empty)
    (imkbPGP.resolve("notinkb_human")) should be (empty)
    (imkbPGP.resolve("notinkb protein")) should be (empty)
    (imkbPGP.resolve("notinkb family")) should be (empty)
    // family key transforms not applicable for proteins:
    (imkbPGP.resolve("pthr2 family")) should be (empty)
    (imkbPQZ.resolve("zyx-1 family")) should be (empty)
  }

  "ProteinKBL resolve" should "work with alternate lookups" in {
    (imkbPGP.resolve("PTHR2")) should be (defined)
    (imkbPGP.resolve("pthr2")) should be (defined)
    (imkbPGP.resolve("pthr2_human")) should be (defined)
    (imkbPGP.resolve("pthr2 protein")) should be (defined)
    (imkbPGP.resolve("mutant-pthr2")) should be (defined)
    (imkbPQZ.resolve("mutant-zyx-1")) should be (defined)
    (imkbPQZ.resolve("zyx-1")) should be (defined)
    (imkbPQZ.resolve("zyx-1_human")) should be (defined)
    (imkbPQZ.resolve("zyx-1 protein")) should be (defined)
    (imkbPQZ.resolve("STAT1")) should be (defined)
    (imkbPQZ.resolve("stat1")) should be (defined)
    (imkbPQZ.resolve("STBA")) should be (defined)
    (imkbPQZ.resolve("stba")) should be (defined)
    (imkbPQZ.resolve("SMAD2")) should be (defined)
    (imkbPQZ.resolve("Smad2")) should be (defined)
    (imkbPQZ.resolve("smad2")) should be (defined)
    (imkbPQZ.resolve("SMAD 2")) should be (defined)
    (imkbPQZ.resolve("Smad 2")) should be (defined)
    (imkbPQZ.resolve("smad 2")) should be (defined)
    (imkbPQZ.resolve("SMAD-2")) should be (defined)
    (imkbPQZ.resolve("Smad-2")) should be (defined)
    (imkbPQZ.resolve("smad-2")) should be (defined)
  }

  "ProteinKBL resolve" should "work via protein domain lookup" in {
    (imkbPGP.resolve("PI3Kbeta-RBD")) should be (defined)
    (imkbPGP.resolve("pi3kbeta-rbd")) should be (defined)
    (imkbPGP.resolve("PI3Kbeta-DSS1_SEM1")) should be (defined)
    (imkbPGP.resolve("pi3kbeta-dss1_sem1")) should be (defined)
    (imkbPGP.resolve("PTHR2-ZU5")) should be (defined)
    (imkbPGP.resolve("pthr2-zu5")) should be (defined)
    (imkbPGP.resolve("pthr2-DSS1_SEM1")) should be (defined)
  }

  "ProteinKBL resolve" should "fail despite protein domain lookup" in {
    (imkbPGP.resolve("NotInKB-RBD")) should be (empty) // protein not in KB
    (imkbPGP.resolve("PI3KC2b-RBD")) should be (empty) // protein not in KB
    (imkbPGP.resolve("zyx-1-rbd")) should be (empty) // pre-key text fails pattern match
    (imkbPGP.resolve("PI3K-C2-alpha-RBD")) should be (empty) // pre-key text fails pattern match
  }

  "ProteinKBL resolve" should "fail despite PTM prefix stripping" in {
    (imkbPGP.resolve("pNOTINKB")) should be (empty)
    (imkbPGP.resolve("pnotinkb")) should be (empty)
    (imkbPGP.resolve("uNOTINKB")) should be (empty)
    (imkbPGP.resolve("unotinkb")) should be (empty)
    // does not match restricted pattern (and not in KB without pattern matching)
    (imkbPGP.resolve("PSTAT1")) should be (empty)
    (imkbPGP.resolve("Pstat1")) should be (empty)
    (imkbPGP.resolve("pstat1")) should be (empty)
    (imkbPGP.resolve("uerk")) should be (empty)
    (imkbPGP.resolve("ustat1")) should be (empty)
    (imkbPGP.resolve("ustba")) should be (empty)
  }

  "ProteinKBL resolve" should "work with PTM prefix stripping" in {
    (imkbPQZ.resolve("pSTAT1")) should be (defined)
    (imkbPQZ.resolve("pSTBC")) should be (defined)
    (imkbP0F.resolve("pERK")) should be (defined)
    (imkbP0F.resolve("uERK")) should be (defined)
    (imkbPQZ.resolve("uSTAT1")) should be (defined)
    (imkbPQZ.resolve("uSTBA")) should be (defined)
  }


  "ProteinKBL resolve" should "fail despite gene name affix stripping" in {
    (imkbPGP.resolve("prefix-NOTINKB")) should be (empty)
    (imkbPGP.resolve("suffix-notinkb")) should be (empty)
    (imkbPGP.resolve("xxx-NOTINKB")) should be (empty)
    (imkbPGP.resolve("u-notinkb")) should be (empty)
    (imkbPGP.resolve("rAAV")) should be (empty)            // only non-entity prefix
    (imkbPGP.resolve("EGFP-MCHY")) should be (empty)       // only non-entity prefixes
    (imkbPGP.resolve("shRNA")) should be (empty)           // only suffix
    (imkbPGP.resolve("KD-shRNA")) should be (empty)        // only suffixes
    (imkbPGP.resolve("Gfp-kd")) should be (empty)          // only prefix & suffix
    (imkbPGP.resolve("Gfp-SH-kd-shRNA")) should be (empty) // only prefixes & suffixes
    (imkbPGP.resolve("Yfp-virus-YFP")) should be (empty)   // no such protein
    (imkbPGP.resolve("KD-VIRUS-shRNA")) should be (empty)  // no such protein
    (imkbPGP.resolve("Gfp-Virus-kd")) should be (empty)    // no such protein
  }

  "ProteinKBL resolve" should "work with gene name affix stripping" in {
    (imkbPQZ.resolve("activated-STAT1")) should be (defined)
    (imkbPQZ.resolve("lent-STAT1")) should be (defined)
    (imkbPQZ.resolve("lenti-STAT1")) should be (defined)
    (imkbPQZ.resolve("lentivirus-STAT1")) should be (defined)
    (imkbPQZ.resolve("Lent-STAT1")) should be (defined)
    (imkbPQZ.resolve("Lenti-STAT1")) should be (defined)
    (imkbPQZ.resolve("Lentivirus-STAT1")) should be (defined)
    (imkbPQZ.resolve("Myc-STBC")) should be (defined)
    (imkbPQZ.resolve("rAAV-STBA")) should be (defined)
    (imkbPQZ.resolve("Yfp-STBA")) should be (defined)
    (imkbPQZ.resolve("YFP-STBA")) should be (defined)
    (imkbP0F.resolve("phospho-ERK")) should be (defined)
    (imkbP0F.resolve("phosphorylated-ERK")) should be (defined)
    (imkbP0F.resolve("ERK-KD")) should be (defined)            // suffixed
    (imkbP0F.resolve("ERK-kd")) should be (defined)            // suffixed
    (imkbP0F.resolve("ERK-KD-SHRNA")) should be (defined)      // multiple suffixes
    (imkbP0F.resolve("ERK-kd-shrna")) should be (defined)      // multiple suffixes
    (imkbP0F.resolve("Myr-Flag-Akt1")) should be (defined)     // multiple prefix
    (imkbP0F.resolve("Myr-Flag-Akt-1")) should be (defined)    // multiple prefix
    (imkbP0F.resolve("Sh-Myr-Flag-Akt1")) should be (defined)  // multiple prefix
    (imkbP0F.resolve("SH-MYR-FLAG-Akt-1")) should be (defined) // multiple prefix
    (imkbPQZ.resolve("GFP-Mchy-SH")) should be (defined)       // only prefixes
    (imkbPGP.resolve("GFP-KRAS-KD")) should be (defined)       // prefix & suffix
    (imkbPGP.resolve("GFP-KRAS-Kd")) should be (defined)       // prefix & suffix
    (imkbPGP.resolve("WT-Gfp-KRAS-Kd-shRNA")) should be (defined) // prefixes & suffixes
    (imkbPGP.resolve("WT-Gfp-KRAS-Kd-shRNA")) should be (defined) // prefixes & suffixes
  }


  "ProteinKBL resolveByASpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbPGP.resolveByASpecies("NotInKB", "ant")) should be (empty)
    (imkbPGP.resolveByASpecies("NotInKB_human", "ant")) should be (empty)
    (imkbPGP.resolveByASpecies("NotInKB protein", "ant")) should be (empty)
    (imkbPGP.resolveByASpecies("NotInKB family", "ant")) should be (empty)
    (imkbPGP.resolveByASpecies("mutant-NotInKB", "ant")) should be (empty)
    // entry does not have this species:
    (imkbPGP.resolveByASpecies("zyx-1", "frog")) should be (empty)
    (imkbPGP.resolveByASpecies("zyx-1_human", "frog")) should be (empty)
    (imkbPGP.resolveByASpecies("zyx-1 protein", "frog")) should be (empty)
    (imkbPGP.resolveByASpecies("zyx-1 family", "frog")) should be (empty)
    (imkbPGP.resolveByASpecies("mutant-zyx-1", "frog")) should be (empty)
    // family key transforms not applicable for proteins:
    (imkbPGP.resolveByASpecies("pthr2 family", "human")) should be (empty)
    (imkbPGP.resolveByASpecies("zyx-1 family", "caenorhabditis elegans")) should be (empty)
  }

  "ProteinKBL resolveByASpecies" should "work with alternate lookups" in {
    (imkbPGP.resolveByASpecies("pthr2", "human")) should be (defined)
    (imkbPGP.resolveByASpecies("pthr2_human", "human")) should be (defined)
    (imkbPGP.resolveByASpecies("pthr2 protein", "human")) should be (defined)
    (imkbPGP.resolveByASpecies("PTHR2 protein", "human")) should be (defined)
    (imkbPGP.resolveByASpecies("mutant-pthr2", "human")) should be (defined)
    (imkbPQZ.resolveByASpecies("zyx-1", "caenorhabditis elegans")) should be (defined)
    (imkbPQZ.resolveByASpecies("zyx-1_human", "caenorhabditis elegans")) should be (defined)
    (imkbPQZ.resolveByASpecies("zyx-1 protein", "caenorhabditis elegans")) should be (defined)
    (imkbPQZ.resolveByASpecies("mutant-zyx-1", "caenorhabditis elegans")) should be (defined)
  }

  "ProteinKBL resolveByASpecies" should "work via protein domain lookup" in {
    (imkbPGP.resolveByASpecies("PI3Kbeta-RBD", "human")) should be (defined)
    (imkbPGP.resolveByASpecies("pi3kbeta-rbd", "human")) should be (defined)
    (imkbPGP.resolveByASpecies("PI3Kbeta-DSS1_SEM1", "human")) should be (defined)
    (imkbPGP.resolveByASpecies("pi3kbeta-dss1_sem1", "human")) should be (defined)
    (imkbPGP.resolveByASpecies("PI3Kdelta-RBD", "mouse")) should be (defined)
    (imkbPGP.resolveByASpecies("pi3kdelta-rbd", "mouse")) should be (defined)
    (imkbPGP.resolveByASpecies("PI3Kdelta-DSS1_SEM1", "mouse")) should be (defined)
    (imkbPGP.resolveByASpecies("pi3kdelta-dss1_sem1", "mouse")) should be (defined)
  }

  "ProteinKBL resolveByASpecies" should "fail despite protein domain lookup" in {
    // proteins not in KB:
    (imkbPGP.resolveByASpecies("NotInKB-RBD", "human")) should be (empty)
    (imkbPGP.resolveByASpecies("PI3KC2b-RBD", "human")) should be (empty)
    // pre-key text fails pattern match:
    (imkbPGP.resolveByASpecies("zyx-1-rbd", "human")) should be (empty)
    (imkbPGP.resolveByASpecies("PI3K-C2-alpha-RBD", "human")) should be (empty)
    // wrong species:
    (imkbPGP.resolveByASpecies("PI3Kdelta-RBD", "rat")) should be (empty)
    (imkbPGP.resolveByASpecies("pi3kdelta-rbd", "rat")) should be (empty)
    (imkbPGP.resolveByASpecies("PI3Kdelta-DSS1_SEM1", "rat")) should be (empty)
    (imkbPGP.resolveByASpecies("pi3kdelta-dss1_sem1", "rat")) should be (empty)
  }

  "ProteinKBL resolveByASpecies" should "work in straightforward tests" in {
    (imkbPQZ.resolveByASpecies("SMAD2", "human")) should be (defined)
    (imkbPQZ.resolveByASpecies("Smad2", "human")) should be (defined)
    (imkbPQZ.resolveByASpecies("smad2", "human")) should be (defined)
    (imkbPQZ.resolveByASpecies("SMAD 2", "human")) should be (defined)
    (imkbPQZ.resolveByASpecies("Smad 2", "human")) should be (defined)
    (imkbPQZ.resolveByASpecies("smad 2", "human")) should be (defined)
    (imkbPQZ.resolveByASpecies("SMAD-2", "human")) should be (defined)
    (imkbPQZ.resolveByASpecies("Smad-2", "human")) should be (defined)
    (imkbPQZ.resolveByASpecies("smad-2", "human")) should be (defined)
  }


  val setA =   Set("aardvark")
  val setF =   Set("frog")
  val setH =   Set("human")
  val setHM =  Set("human", "mouse")
  val setHMG = Set("human", "mouse", "gorilla")
  "ProteinKBL resolveBySpecies" should "should fail despite alternate lookups" in {
    // key not in KB:
    (imkbPGP.resolveBySpecies("NotInKB", setA)) should be (empty)
    (imkbPGP.resolveBySpecies("NotInKB_human", setA)) should be (empty)
    (imkbPGP.resolveBySpecies("NotInKB protein", setA)) should be (empty)
    (imkbPGP.resolveBySpecies("NotInKB family", setA)) should be (empty)
    (imkbPGP.resolveBySpecies("mutant-NotInKB", setA)) should be (empty)
    (imkbPGP.resolveBySpecies("pthr2 mouse", setH)) should be (empty)
    // entry does not have this species:
    (imkbPGP.resolveBySpecies("pthr2", setF)) should be (empty)
    (imkbPGP.resolveBySpecies("pthr2_human", setF)) should be (empty)
    (imkbPGP.resolveBySpecies("pthr2 protein", setF)) should be (empty)
    (imkbPGP.resolveBySpecies("pthr2 family", setF)) should be (empty)
    (imkbPGP.resolveBySpecies("mutant-pthr2", setF)) should be (empty)
    // entry does not have these species (nematode only):
    (imkbPGP.resolveBySpecies("zyx-1", setHM)) should be (empty)
    (imkbPGP.resolveBySpecies("zyx-1_human", setHM)) should be (empty)
    (imkbPGP.resolveBySpecies("zyx-1 protein", setHM)) should be (empty)
    (imkbPGP.resolveBySpecies("zyx-1 family", setHM)) should be (empty)
    (imkbPGP.resolveBySpecies("mutant-zyx-1", setHM)) should be (empty)
    // family key transforms not applicable for proteins:
    (imkbPGP.resolveBySpecies("pthr2 family", setH)) should be (empty)
    (imkbPGP.resolveBySpecies("PTHR2 family", setH)) should be (empty)
    (imkbPGP.resolveBySpecies("pthr2 family", setHM)) should be (empty)
    (imkbPGP.resolveBySpecies("pthr2 family", setHMG)) should be (empty)

  }

  "ProteinKBL resolveBySpecies" should "work with alternate lookups" in {
    (imkbPGP.resolveBySpecies("pthr2", setH)) should be (defined)
    (imkbPGP.resolveBySpecies("PTHR2", setH)) should be (defined)
    (imkbPGP.resolveBySpecies("pthr2_human", setH)) should be (defined)
    (imkbPGP.resolveBySpecies("PTHR2_human", setH)) should be (defined)
    (imkbPGP.resolveBySpecies("pthr2 protein", setH)) should be (defined)
    (imkbPGP.resolveBySpecies("PTHR2 protein", setH)) should be (defined)
    (imkbPGP.resolveBySpecies("mutant-pthr2", setH)) should be (defined)
    (imkbPGP.resolveBySpecies("MUTANT-PTHR2", setH)) should be (defined)

    (imkbPGP.resolveBySpecies("pthr2", setHM)) should be (defined)
    (imkbPGP.resolveBySpecies("pthr2", setHM).get) should have size 8
    (imkbPGP.resolveBySpecies("pthr2_human", setHM)) should be (defined)
    (imkbPGP.resolveBySpecies("pthr2_human", setHM).get) should have size 8
    (imkbPGP.resolveBySpecies("pthr2 protein", setHM)) should be (defined)
    (imkbPGP.resolveBySpecies("mutant-pthr2", setHM)) should be (defined)

    (imkbPGP.resolveBySpecies("pthr2", setHMG)) should be (defined)
    (imkbPGP.resolveBySpecies("pthr2", setHMG).get) should have size 8
    (imkbPGP.resolveBySpecies("pthr2_human", setHMG)) should be (defined)
    (imkbPGP.resolveBySpecies("pthr2_human", setHMG).get) should have size 8
    (imkbPGP.resolveBySpecies("pthr2 protein", setHMG)) should be (defined)
    (imkbPGP.resolveBySpecies("mutant-pthr2", setHMG)) should be (defined)

    (imkbPQZ.resolveBySpecies("zyx-1", Set("caenorhabditis elegans", "ant"))) should be (defined)
    (imkbPQZ.resolveBySpecies("zyx-1", Set("ant", "caenorhabditis elegans"))) should be (defined)
    // TODO Enrique: This test originally had size 3, but after splitting uniprot lexicographically, the specific one
    // we are testing only has size 2. Figure out a way to merge multiple files into the same resolution instance
    (imkbPQZ.resolveBySpecies("zyx-1", Set("ant", "caenorhabditis elegans")).get) should have size 2 //3
  }

  "ProteinKBL resolveBySpecies" should "work via protein domain lookup" in {
    (imkbPGP.resolveBySpecies("PI3Kbeta-RBD", setHM)) should be (defined)
    (imkbPGP.resolveBySpecies("pi3kbeta-rbd", setHM)) should be (defined)
    (imkbPGP.resolveBySpecies("PI3Kbeta-DSS1_SEM1", setHM)) should be (defined)
    (imkbPGP.resolveBySpecies("pi3kbeta-dss1_sem1", setHM)) should be (defined)
    (imkbPGP.resolveBySpecies("PI3Kdelta-RBD", setHM)) should be (defined)
    (imkbPGP.resolveBySpecies("pi3kdelta-rbd", setHM)) should be (defined)
    (imkbPGP.resolveBySpecies("PI3Kdelta-DSS1_SEM1", setHM)) should be (defined)
    (imkbPGP.resolveBySpecies("pi3kdelta-dss1_sem1", setHM)) should be (defined)
  }

  "ProteinKBL resolveBySpecies" should "fail despite protein domain lookup" in {
    // proteins not in KB:
    (imkbPGP.resolveBySpecies("NotInKB-RBD", setHM)) should be (empty)
    (imkbPGP.resolveBySpecies("PI3KC2b-RBD", setHM)) should be (empty)
    // pre-key text fails pattern match:
    (imkbPGP.resolveBySpecies("zyx-1-rbd", setHM)) should be (empty)
    (imkbPGP.resolveBySpecies("PI3K-C2-alpha-RBD", setHM)) should be (empty)
    // wrong species:
    (imkbPGP.resolveBySpecies("PI3Kdelta-RBD", setF)) should be (empty)
    (imkbPGP.resolveBySpecies("pi3kdelta-rbd", setF)) should be (empty)
    (imkbPGP.resolveBySpecies("PI3Kdelta-DSS1_SEM1", setF)) should be (empty)
    (imkbPGP.resolveBySpecies("pi3kdelta-dss1_sem1", setF)) should be (empty)
  }


  "ProteinKBL resolveHuman" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbPGP.resolveHuman("NotInKB")) should be (empty)
    (imkbPGP.resolveHuman("NotInKB_human")) should be (empty)
    (imkbPGP.resolveHuman("NotInKB protein")) should be (empty)
    (imkbPGP.resolveHuman("NotInKB family")) should be (empty)
    (imkbPGP.resolveHuman("mutant-NotInKB")) should be (empty)
    // entry does not have human species (nematode only):
    (imkbPGP.resolveHuman("zyx-1")) should be (empty)
    (imkbPGP.resolveHuman("zyx-1_human")) should be (empty)
    (imkbPGP.resolveHuman("zyx-1 protein")) should be (empty)
    (imkbPGP.resolveHuman("zyx-1 family")) should be (empty)
    (imkbPGP.resolveHuman("mutant-zyx-1")) should be (empty)
    // family key transforms not applicable for proteins:
    (imkbPGP.resolveHuman("pthr2 family")) should be (empty)
    (imkbPGP.resolveHuman("PTHR2 family")) should be (empty)
  }

  "ProteinKBL resolveHuman" should "work with alternate lookups" in {
    (imkbPGP.resolveHuman("pthr2")) should be (defined)
    (imkbPGP.resolveHuman("PTHR2")) should be (defined)
    (imkbPGP.resolveHuman("pthr2_human")) should be (defined)
    (imkbPGP.resolveHuman("PTHR2_human")) should be (defined)
    (imkbPGP.resolveHuman("pthr2 protein")) should be (defined)
    (imkbPGP.resolveHuman("PTHR2 protein")) should be (defined)
    (imkbPGP.resolveHuman("mutant-pthr2")) should be (defined)
    (imkbPGP.resolveHuman("mutant-PTHR2")) should be (defined)
  }

  "ProteinKBL resolveHuman" should "work via protein domain lookup" in {
    (imkbPGP.resolveHuman("PI3Kbeta-RBD")) should be (defined)
    (imkbPGP.resolveHuman("pi3kbeta-rbd")) should be (defined)
    (imkbPGP.resolveHuman("PI3Kbeta-DSS1_SEM1")) should be (defined)
    (imkbPGP.resolveHuman("pi3kbeta-dss1_sem1")) should be (defined)
    (imkbPGP.resolveHuman("PTHR2-ZU5")) should be (defined)
    (imkbPGP.resolveHuman("pthr2-ZU5")) should be (defined)
    (imkbPGP.resolveHuman("pthr2-DSS1_SEM1")) should be (defined)
  }

  "ProteinKBL resolveHuman" should "fail despite protein domain lookup" in {
    // proteins not in KB:
    (imkbPGP.resolveHuman("NotInKB-RBD")) should be (empty)
    (imkbPGP.resolveHuman("PI3KC2b-RBD")) should be (empty)
    // pre-key text fails pattern match:
    (imkbPGP.resolveHuman("zyx-1-rbd")) should be (empty)
    (imkbPGP.resolveHuman("PI3K-C2-alpha-RBD")) should be (empty)
  }

  "ProteinKBL resolveHuman" should "work in straightforward tests" in {
    (imkbPQZ.resolveHuman("SMAD2")) should be (defined)
    (imkbPQZ.resolveHuman("Smad2")) should be (defined)
    (imkbPQZ.resolveHuman("smad2")) should be (defined)
    (imkbPQZ.resolveHuman("SMAD 2")) should be (defined)
    (imkbPQZ.resolveHuman("Smad 2")) should be (defined)
    (imkbPQZ.resolveHuman("smad 2")) should be (defined)
    (imkbPQZ.resolveHuman("SMAD-2")) should be (defined)
    (imkbPQZ.resolveHuman("Smad-2")) should be (defined)
    (imkbPQZ.resolveHuman("smad-2")) should be (defined)
  }


  // this KB includes species, therefore resolveNoSpecies should always fail:
  "ProteinKBL resolveNoSpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbPGP.resolveNoSpecies("NOTINKB")) should be (empty)
    (imkbPGP.resolveNoSpecies("notinkb")) should be (empty)
    (imkbPGP.resolveNoSpecies("notinkb_human")) should be (empty)
    (imkbPGP.resolveNoSpecies("notinkb protein")) should be (empty)
    (imkbPGP.resolveNoSpecies("notinkb family")) should be (empty)
    (imkbPGP.resolveNoSpecies("notinkb-RBD")) should be (empty)
    // entry has a species:
    (imkbPGP.resolveNoSpecies("PTHR2")) should be (empty)
    (imkbPGP.resolveNoSpecies("pthr2")) should be (empty)
    (imkbPGP.resolveNoSpecies("pthr2_human")) should be (empty)
    (imkbPGP.resolveNoSpecies("pthr2 protein")) should be (empty)
    (imkbPGP.resolveNoSpecies("pthr2 family")) should be (empty)
    (imkbPGP.resolveNoSpecies("mutant-pthr2")) should be (empty)
    (imkbPGP.resolveNoSpecies("zyx-1")) should be (empty)
    (imkbPGP.resolveNoSpecies("zyx-1_human")) should be (empty)
    (imkbPGP.resolveNoSpecies("zyx-1 protein")) should be (empty)
    (imkbPGP.resolveNoSpecies("zyx-1 family")) should be (empty)
    (imkbPGP.resolveNoSpecies("mutant-zyx-1")) should be (empty)
    (imkbPGP.resolveNoSpecies("PI3Kbeta-RBD")) should be (empty)
    (imkbPGP.resolveNoSpecies("PTHR2-ZU5")) should be (empty)
  }

}

// Protein KB using alternate protein resolutions
class TestProteinKBL(fileName:String) extends IMKBLookup {
  val meta = new IMKBMetaInfo(
    kbFilename = Some(fileName),
    namespace = "uniprot",
    baseURI = "http://identifiers.org/uniprot/",
    resourceId = "MIR:00100164",
    hasSpeciesInfo = true,
    isProteinKB = true
  )
  val keyTransforms = KBKeyTransformsGroup(CasedKeyTransforms, ProteinAuxKeyTransforms, CasedKeyTransforms)
  memoryKB = (new TsvIMKBFactory).make(meta, keyTransforms)
}
