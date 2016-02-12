package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Unit tests to ensure alternate resolutions are working for KB grounding.
  *   Written by: Tom Hicks. 11/16/2015.
  *   Last Modified: Update for tsv factory.
  */
class TestProteinResolutions extends FlatSpec with Matchers {

  val imkbP = new TestProteinKBL           // defined after this class (LOOK BELOW)

  "ProteinKBL resolve" should "fail despite alternate lookups" in {
    // keys not in KB:
    (imkbP.resolve("NOTINKB").isDefined) should be (false)
    (imkbP.resolve("notinkb").isDefined) should be (false)
    (imkbP.resolve("notinkb_human").isDefined) should be (false)
    (imkbP.resolve("notinkb protein").isDefined) should be (false)
    (imkbP.resolve("notinkb family").isDefined) should be (false)
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
//    (imkbP.resolve("mutant-zyx-1").isDefined) should be (true) // MUTANT PATTERN MATCH FAILS
  }

  "ProteinKBL resolveByASpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbP.resolveByASpecies("NotInKB", "ant").isDefined) should be (false)
    (imkbP.resolveByASpecies("NotInKB_human", "ant").isDefined) should be (false)
    (imkbP.resolveByASpecies("NotInKB protein", "ant").isDefined) should be (false)
    (imkbP.resolveByASpecies("NotInKB family", "ant").isDefined) should be (false)
    (imkbP.resolveByASpecies("mutant-NotInKB", "ant").isDefined) should be (false)
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
//    (imkbP.resolveByASpecies("mutant-zyx-1", "caenorhabditis elegans").isDefined) should be (true) // MUTANT PATTERN MATCH FAILS
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

  // this KB includes species, therefore resolveNoSpecies should always fail:
  "ProteinKBL resolveNoSpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbP.resolveNoSpecies("NOTINKB").isDefined) should be (false)
    (imkbP.resolveNoSpecies("notinkb").isDefined) should be (false)
    (imkbP.resolveNoSpecies("notinkb_human").isDefined) should be (false)
    (imkbP.resolveNoSpecies("notinkb protein").isDefined) should be (false)
    (imkbP.resolveNoSpecies("notinkb family").isDefined) should be (false)
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
  }

}


// Protein KB using alternate protein resolutions
class TestProteinKBL extends IMKBProteinLookup {
  memoryKB = (new TsvIMKBFactory).make("uniprot", StaticProteinFilename, true,
    new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00100164")) // true = has species
}
