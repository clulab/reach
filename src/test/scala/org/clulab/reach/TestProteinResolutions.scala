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
  *   Written by: Tom Hicks. 11/16/2015.
  *   Last Modified: Add tests for Reach issue #274.
  */
class TestProteinResolutions extends FlatSpec with Matchers {

  val imkbP = new TestProteinKBL           // defined after this class (LOOK BELOW)

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
    (imkbP.resolve("NOTINKB").isDefined) should be (false)
    (imkbP.resolve("notinkb").isDefined) should be (false)
    (imkbP.resolve("notinkb_human").isDefined) should be (false)
    (imkbP.resolve("notinkb protein").isDefined) should be (false)
    (imkbP.resolve("notinkb family").isDefined) should be (false)
    (imkbP.resolve("mutant-zyx-1").isDefined) should be (false) // mutant pattern not matched
    // family key transforms not applicable for proteins:
    (imkbP.resolve("pthr2 family").isDefined) should be (false)
    (imkbP.resolve("zyx-1 family").isDefined) should be (false)
  }

  "ProteinKBL resolve" should "work with alternate lookups" in {
    (imkbP.resolve("PTHR2").isDefined) should be (true)
    (imkbP.resolve("pthr2").isDefined) should be (true)
    (imkbP.resolve("pthr2_human").isDefined) should be (true)
    (imkbP.resolve("pthr2 protein").isDefined) should be (true)
    (imkbP.resolve("mutant-pthr2").isDefined) should be (true)
    (imkbP.resolve("zyx-1").isDefined) should be (true)
    (imkbP.resolve("zyx-1_human").isDefined) should be (true)
    (imkbP.resolve("zyx-1 protein").isDefined) should be (true)
    (imkbP.resolve("STAT1").isDefined) should be (true)
    (imkbP.resolve("stat1").isDefined) should be (true)
    (imkbP.resolve("STBA").isDefined) should be (true)
    (imkbP.resolve("stba").isDefined) should be (true)
    (imkbP.resolve("SMAD2").isDefined) should be (true)
    (imkbP.resolve("Smad2").isDefined) should be (true)
    (imkbP.resolve("smad2").isDefined) should be (true)
    (imkbP.resolve("SMAD 2").isDefined) should be (true)
    (imkbP.resolve("Smad 2").isDefined) should be (true)
    (imkbP.resolve("smad 2").isDefined) should be (true)
    (imkbP.resolve("SMAD-2").isDefined) should be (true)
    (imkbP.resolve("Smad-2").isDefined) should be (true)
    (imkbP.resolve("smad-2").isDefined) should be (true)
  }

  "ProteinKBL resolve" should "work via protein domain lookup" in {
    (imkbP.resolve("PI3Kbeta-RBD").isDefined) should be (true)
    (imkbP.resolve("pi3kbeta-rbd").isDefined) should be (true)
    (imkbP.resolve("PI3Kbeta-DSS1_SEM1").isDefined) should be (true)
    (imkbP.resolve("pi3kbeta-dss1_sem1").isDefined) should be (true)
    (imkbP.resolve("PTHR2-ZU5").isDefined) should be (true)
    (imkbP.resolve("pthr2-zu5").isDefined) should be (true)
    (imkbP.resolve("pthr2-DSS1_SEM1").isDefined) should be (true)
  }

  "ProteinKBL resolve" should "fail despite protein domain lookup" in {
    (imkbP.resolve("NotInKB-RBD").isDefined) should be (false) // protein not in KB
    (imkbP.resolve("PI3KC2b-RBD").isDefined) should be (false) // protein not in KB
    (imkbP.resolve("zyx-1-rbd").isDefined) should be (false) // pre-key text fails pattern match
    (imkbP.resolve("PI3K-C2-alpha-RBD").isDefined) should be (false) // pre-key text fails pattern match
  }

  "ProteinKBL resolve" should "fail despite PTM prefix stripping" in {
    (imkbP.resolve("pNOTINKB").isDefined) should be (false)
    (imkbP.resolve("pnotinkb").isDefined) should be (false)
    (imkbP.resolve("uNOTINKB").isDefined) should be (false)
    (imkbP.resolve("unotinkb").isDefined) should be (false)
    // does not match restricted pattern (and not in KB without pattern matching)
    (imkbP.resolve("PSTAT1").isDefined) should be (false)
    (imkbP.resolve("Pstat1").isDefined) should be (false)
    (imkbP.resolve("pstat1").isDefined) should be (false)
    (imkbP.resolve("uerk").isDefined) should be (false)
    (imkbP.resolve("ustat1").isDefined) should be (false)
    (imkbP.resolve("ustba").isDefined) should be (false)
  }

  "ProteinKBL resolve" should "work with PTM prefix stripping" in {
    (imkbP.resolve("pSTAT1").isDefined) should be (true)
    (imkbP.resolve("pSTBC").isDefined) should be (true)
    (imkbP.resolve("pERK").isDefined) should be (true)
    (imkbP.resolve("uERK").isDefined) should be (true)
    (imkbP.resolve("uSTAT1").isDefined) should be (true)
    (imkbP.resolve("uSTBA").isDefined) should be (true)
  }


  "ProteinKBL resolveByASpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbP.resolveByASpecies("NotInKB", "ant").isDefined) should be (false)
    (imkbP.resolveByASpecies("NotInKB_human", "ant").isDefined) should be (false)
    (imkbP.resolveByASpecies("NotInKB protein", "ant").isDefined) should be (false)
    (imkbP.resolveByASpecies("NotInKB family", "ant").isDefined) should be (false)
    (imkbP.resolveByASpecies("mutant-NotInKB", "ant").isDefined) should be (false)
    (imkbP.resolveByASpecies("mutant-zyx-1", "caenorhabditis elegans").isDefined) should be (false)
    // entry does not have this species:
    (imkbP.resolveByASpecies("zyx-1", "frog").isDefined) should be (false)
    (imkbP.resolveByASpecies("zyx-1_human", "frog").isDefined) should be (false)
    (imkbP.resolveByASpecies("zyx-1 protein", "frog").isDefined) should be (false)
    (imkbP.resolveByASpecies("zyx-1 family", "frog").isDefined) should be (false)
    (imkbP.resolveByASpecies("mutant-zyx-1", "frog").isDefined) should be (false)
    // family key transforms not applicable for proteins:
    (imkbP.resolveByASpecies("pthr2 family", "human").isDefined) should be (false)
    (imkbP.resolveByASpecies("zyx-1 family", "caenorhabditis elegans").isDefined) should be (false)
  }

  "ProteinKBL resolveByASpecies" should "work with alternate lookups" in {
    (imkbP.resolveByASpecies("pthr2", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("pthr2_human", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("pthr2 protein", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("PTHR2 protein", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("mutant-pthr2", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("zyx-1", "caenorhabditis elegans").isDefined) should be (true)
    (imkbP.resolveByASpecies("zyx-1_human", "caenorhabditis elegans").isDefined) should be (true)
    (imkbP.resolveByASpecies("zyx-1 protein", "caenorhabditis elegans").isDefined) should be (true)
  }

  "ProteinKBL resolveByASpecies" should "work via protein domain lookup" in {
    (imkbP.resolveByASpecies("PI3Kbeta-RBD", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("pi3kbeta-rbd", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("PI3Kbeta-DSS1_SEM1", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("pi3kbeta-dss1_sem1", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("PI3Kdelta-RBD", "mouse").isDefined) should be (true)
    (imkbP.resolveByASpecies("pi3kdelta-rbd", "mouse").isDefined) should be (true)
    (imkbP.resolveByASpecies("PI3Kdelta-DSS1_SEM1", "mouse").isDefined) should be (true)
    (imkbP.resolveByASpecies("pi3kdelta-dss1_sem1", "mouse").isDefined) should be (true)
  }

  "ProteinKBL resolveByASpecies" should "fail despite protein domain lookup" in {
    // proteins not in KB:
    (imkbP.resolveByASpecies("NotInKB-RBD", "human").isDefined) should be (false)
    (imkbP.resolveByASpecies("PI3KC2b-RBD", "human").isDefined) should be (false)
    // pre-key text fails pattern match:
    (imkbP.resolveByASpecies("zyx-1-rbd", "human").isDefined) should be (false)
    (imkbP.resolveByASpecies("PI3K-C2-alpha-RBD", "human").isDefined) should be (false)
    // wrong species:
    (imkbP.resolveByASpecies("PI3Kdelta-RBD", "rat").isDefined) should be (false)
    (imkbP.resolveByASpecies("pi3kdelta-rbd", "rat").isDefined) should be (false)
    (imkbP.resolveByASpecies("PI3Kdelta-DSS1_SEM1", "rat").isDefined) should be (false)
    (imkbP.resolveByASpecies("pi3kdelta-dss1_sem1", "rat").isDefined) should be (false)
  }

  "ProteinKBL resolveByASpecies" should "work in straightforward tests" in {
    (imkbP.resolveByASpecies("SMAD2", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("Smad2", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("smad2", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("SMAD 2", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("Smad 2", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("smad 2", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("SMAD-2", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("Smad-2", "human").isDefined) should be (true)
    (imkbP.resolveByASpecies("smad-2", "human").isDefined) should be (true)
  }


  val setA =   Set("aardvark")
  val setF =   Set("frog")
  val setH =   Set("human")
  val setHM =  Set("human", "mouse")
  val setHMG = Set("human", "mouse", "gorilla")
  "ProteinKBL resolveBySpecies" should "should fail despite alternate lookups" in {
    // key not in KB:
    (imkbP.resolveBySpecies("NotInKB", setA).isDefined) should be (false)
    (imkbP.resolveBySpecies("NotInKB_human", setA).isDefined) should be (false)
    (imkbP.resolveBySpecies("NotInKB protein", setA).isDefined) should be (false)
    (imkbP.resolveBySpecies("NotInKB family", setA).isDefined) should be (false)
    (imkbP.resolveBySpecies("mutant-NotInKB", setA).isDefined) should be (false)
    (imkbP.resolveBySpecies("pthr2 mouse", setH).isDefined) should be (false)
    // entry does not have this species:
    (imkbP.resolveBySpecies("pthr2", setF).isDefined) should be (false)
    (imkbP.resolveBySpecies("pthr2_human", setF).isDefined) should be (false)
    (imkbP.resolveBySpecies("pthr2 protein", setF).isDefined) should be (false)
    (imkbP.resolveBySpecies("pthr2 family", setF).isDefined) should be (false)
    (imkbP.resolveBySpecies("mutant-pthr2", setF).isDefined) should be (false)
    // entry does not have these species (yeast only):
    (imkbP.resolveBySpecies("zyx-1", setHM).isDefined) should be (false)
    (imkbP.resolveBySpecies("zyx-1_human", setHM).isDefined) should be (false)
    (imkbP.resolveBySpecies("zyx-1 protein", setHM).isDefined) should be (false)
    (imkbP.resolveBySpecies("zyx-1 family", setHM).isDefined) should be (false)
    (imkbP.resolveBySpecies("mutant-zyx-1", setHM).isDefined) should be (false)
    // family key transforms not applicable for proteins:
    (imkbP.resolveBySpecies("pthr2 family", setH).isDefined) should be (false)
    (imkbP.resolveBySpecies("PTHR2 family", setH).isDefined) should be (false)
    (imkbP.resolveBySpecies("pthr2 family", setHM).isDefined) should be (false)
    (imkbP.resolveBySpecies("pthr2 family", setHMG).isDefined) should be (false)

  }

  "ProteinKBL resolveBySpecies" should "work with alternate lookups" in {
    (imkbP.resolveBySpecies("pthr2", setH).isDefined) should be (true)
    (imkbP.resolveBySpecies("PTHR2", setH).isDefined) should be (true)
    (imkbP.resolveBySpecies("pthr2_human", setH).isDefined) should be (true)
    (imkbP.resolveBySpecies("PTHR2_human", setH).isDefined) should be (true)
    (imkbP.resolveBySpecies("pthr2 protein", setH).isDefined) should be (true)
    (imkbP.resolveBySpecies("PTHR2 protein", setH).isDefined) should be (true)
    (imkbP.resolveBySpecies("mutant-pthr2", setH).isDefined) should be (true)
    (imkbP.resolveBySpecies("MUTANT-PTHR2", setH).isDefined) should be (true)

    (imkbP.resolveBySpecies("pthr2", setHM).isDefined) should be (true)
    (imkbP.resolveBySpecies("pthr2", setHM).get.size == 2) should be (true)
    (imkbP.resolveBySpecies("pthr2_human", setHM).isDefined) should be (true)
    (imkbP.resolveBySpecies("pthr2_human", setHM).get.size == 2) should be (true)
    (imkbP.resolveBySpecies("pthr2 protein", setHM).isDefined) should be (true)
    (imkbP.resolveBySpecies("mutant-pthr2", setHM).isDefined) should be (true)

    (imkbP.resolveBySpecies("pthr2", setHMG).isDefined) should be (true)
    (imkbP.resolveBySpecies("pthr2", setHMG).get.size == 2) should be (true)
    (imkbP.resolveBySpecies("pthr2_human", setHMG).isDefined) should be (true)
    (imkbP.resolveBySpecies("pthr2_human", setHMG).get.size == 2) should be (true)
    (imkbP.resolveBySpecies("pthr2 protein", setHMG).isDefined) should be (true)
    (imkbP.resolveBySpecies("mutant-pthr2", setHMG).isDefined) should be (true)

    (imkbP.resolveBySpecies("zyx-1", Set("caenorhabditis elegans", "ant")).isDefined) should be (true)
    (imkbP.resolveBySpecies("zyx-1", Set("ant", "caenorhabditis elegans")).isDefined) should be (true)
    (imkbP.resolveBySpecies("zyx-1", Set("ant", "caenorhabditis elegans")).get.size == 1) should be (true)
  }

  "ProteinKBL resolveBySpecies" should "work via protein domain lookup" in {
    (imkbP.resolveBySpecies("PI3Kbeta-RBD", setHM).isDefined) should be (true)
    (imkbP.resolveBySpecies("pi3kbeta-rbd", setHM).isDefined) should be (true)
    (imkbP.resolveBySpecies("PI3Kbeta-DSS1_SEM1", setHM).isDefined) should be (true)
    (imkbP.resolveBySpecies("pi3kbeta-dss1_sem1", setHM).isDefined) should be (true)
    (imkbP.resolveBySpecies("PI3Kdelta-RBD", setHM).isDefined) should be (true)
    (imkbP.resolveBySpecies("pi3kdelta-rbd", setHM).isDefined) should be (true)
    (imkbP.resolveBySpecies("PI3Kdelta-DSS1_SEM1", setHM).isDefined) should be (true)
    (imkbP.resolveBySpecies("pi3kdelta-dss1_sem1", setHM).isDefined) should be (true)
  }

  "ProteinKBL resolveBySpecies" should "fail despite protein domain lookup" in {
    // proteins not in KB:
    (imkbP.resolveBySpecies("NotInKB-RBD", setHM).isDefined) should be (false)
    (imkbP.resolveBySpecies("PI3KC2b-RBD", setHM).isDefined) should be (false)
    // pre-key text fails pattern match:
    (imkbP.resolveBySpecies("zyx-1-rbd", setHM).isDefined) should be (false)
    (imkbP.resolveBySpecies("PI3K-C2-alpha-RBD", setHM).isDefined) should be (false)
    // wrong species:
    (imkbP.resolveBySpecies("PI3Kdelta-RBD", setF).isDefined) should be (false)
    (imkbP.resolveBySpecies("pi3kdelta-rbd", setF).isDefined) should be (false)
    (imkbP.resolveBySpecies("PI3Kdelta-DSS1_SEM1", setF).isDefined) should be (false)
    (imkbP.resolveBySpecies("pi3kdelta-dss1_sem1", setF).isDefined) should be (false)
  }


  "ProteinKBL resolveHuman" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbP.resolveHuman("NotInKB").isDefined) should be (false)
    (imkbP.resolveHuman("NotInKB_human").isDefined) should be (false)
    (imkbP.resolveHuman("NotInKB protein").isDefined) should be (false)
    (imkbP.resolveHuman("NotInKB family").isDefined) should be (false)
    (imkbP.resolveHuman("mutant-NotInKB").isDefined) should be (false)
    // entry does not have human species (yeast only):
    (imkbP.resolveHuman("zyx-1").isDefined) should be (false)
    (imkbP.resolveHuman("zyx-1_human").isDefined) should be (false)
    (imkbP.resolveHuman("zyx-1 protein").isDefined) should be (false)
    (imkbP.resolveHuman("zyx-1 family").isDefined) should be (false)
    (imkbP.resolveHuman("mutant-zyx-1").isDefined) should be (false)
    // family key transforms not applicable for proteins:
    (imkbP.resolveHuman("pthr2 family").isDefined) should be (false)
    (imkbP.resolveHuman("PTHR2 family").isDefined) should be (false)
  }

  "ProteinKBL resolveHuman" should "work with alternate lookups" in {
    (imkbP.resolveHuman("pthr2").isDefined) should be (true)
    (imkbP.resolveHuman("PTHR2").isDefined) should be (true)
    (imkbP.resolveHuman("pthr2_human").isDefined) should be (true)
    (imkbP.resolveHuman("PTHR2_human").isDefined) should be (true)
    (imkbP.resolveHuman("pthr2 protein").isDefined) should be (true)
    (imkbP.resolveHuman("PTHR2 protein").isDefined) should be (true)
    (imkbP.resolveHuman("mutant-pthr2").isDefined) should be (true)
    (imkbP.resolveHuman("mutant-PTHR2").isDefined) should be (true)
  }

  "ProteinKBL resolveHuman" should "work via protein domain lookup" in {
    (imkbP.resolveHuman("PI3Kbeta-RBD").isDefined) should be (true)
    (imkbP.resolveHuman("pi3kbeta-rbd").isDefined) should be (true)
    (imkbP.resolveHuman("PI3Kbeta-DSS1_SEM1").isDefined) should be (true)
    (imkbP.resolveHuman("pi3kbeta-dss1_sem1").isDefined) should be (true)
    (imkbP.resolveHuman("PTHR2-ZU5").isDefined) should be (true)
    (imkbP.resolveHuman("pthr2-ZU5").isDefined) should be (true)
    (imkbP.resolveHuman("pthr2-DSS1_SEM1").isDefined) should be (true)
  }

  "ProteinKBL resolveHuman" should "fail despite protein domain lookup" in {
    // proteins not in KB:
    (imkbP.resolveHuman("NotInKB-RBD").isDefined) should be (false)
    (imkbP.resolveHuman("PI3KC2b-RBD").isDefined) should be (false)
    // pre-key text fails pattern match:
    (imkbP.resolveHuman("zyx-1-rbd").isDefined) should be (false)
    (imkbP.resolveHuman("PI3K-C2-alpha-RBD").isDefined) should be (false)
  }

  "ProteinKBL resolveHuman" should "work in straightforward tests" in {
    (imkbP.resolveHuman("SMAD2").isDefined) should be (true)
    (imkbP.resolveHuman("Smad2").isDefined) should be (true)
    (imkbP.resolveHuman("smad2").isDefined) should be (true)
    (imkbP.resolveHuman("SMAD 2").isDefined) should be (true)
    (imkbP.resolveHuman("Smad 2").isDefined) should be (true)
    (imkbP.resolveHuman("smad 2").isDefined) should be (true)
    (imkbP.resolveHuman("SMAD-2").isDefined) should be (true)
    (imkbP.resolveHuman("Smad-2").isDefined) should be (true)
    (imkbP.resolveHuman("smad-2").isDefined) should be (true)
  }


  // this KB includes species, therefore resolveNoSpecies should always fail:
  "ProteinKBL resolveNoSpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbP.resolveNoSpecies("NOTINKB").isDefined) should be (false)
    (imkbP.resolveNoSpecies("notinkb").isDefined) should be (false)
    (imkbP.resolveNoSpecies("notinkb_human").isDefined) should be (false)
    (imkbP.resolveNoSpecies("notinkb protein").isDefined) should be (false)
    (imkbP.resolveNoSpecies("notinkb family").isDefined) should be (false)
    (imkbP.resolveNoSpecies("notinkb-RBD").isDefined) should be (false)
    // entry has a species:
    (imkbP.resolveNoSpecies("PTHR2").isDefined) should be (false)
    (imkbP.resolveNoSpecies("pthr2").isDefined) should be (false)
    (imkbP.resolveNoSpecies("pthr2_human").isDefined) should be (false)
    (imkbP.resolveNoSpecies("pthr2 protein").isDefined) should be (false)
    (imkbP.resolveNoSpecies("pthr2 family").isDefined) should be (false)
    (imkbP.resolveNoSpecies("mutant-pthr2").isDefined) should be (false)
    (imkbP.resolveNoSpecies("zyx-1").isDefined) should be (false)
    (imkbP.resolveNoSpecies("zyx-1_human").isDefined) should be (false)
    (imkbP.resolveNoSpecies("zyx-1 protein").isDefined) should be (false)
    (imkbP.resolveNoSpecies("zyx-1 family").isDefined) should be (false)
    (imkbP.resolveNoSpecies("mutant-zyx-1").isDefined) should be (false)
    (imkbP.resolveNoSpecies("PI3Kbeta-RBD").isDefined) should be (false)
    (imkbP.resolveNoSpecies("PTHR2-ZU5").isDefined) should be (false)
  }

}


// Protein KB using alternate protein resolutions
class TestProteinKBL extends IMKBProteinLookup {
  val meta = new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00100164")
  meta.put("protein", "true")               // mark as from a protein KB
  memoryKB = (new TsvIMKBFactory).make("uniprot", StaticProteinFilename, true, meta)
}
