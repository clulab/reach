package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.grounding2._
import edu.arizona.sista.reach.grounding2.LocalKBConstants._

/**
  * Unit tests to ensure alternate resolutions are working for KB grounding.
  *   Written by: Tom Hicks. 11/4/2015.
  *   Last Modified: Update for IMKB has species arg, separation of protein/family lookups.
  */
class TestAltResolutions extends FlatSpec with Matchers {

  val imkbPF = new TestProtFamKBL           // defined after this class (LOOK BELOW)

  // this KB includes species, therefore plain resolve should always fail:
  "LocalProteinKBML resolve" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbPF.resolve("NOTINKB").isDefined) should be (false)
    (imkbPF.resolve("notinkb").isDefined) should be (false)
    (imkbPF.resolve("notinkb_human").isDefined) should be (false)
    (imkbPF.resolve("notinkb protein").isDefined) should be (false)
    (imkbPF.resolve("notinkb family").isDefined) should be (false)
    // entry has a species:
    (imkbPF.resolve("PTHR21244").isDefined) should be (false)
    (imkbPF.resolve("pthr21244").isDefined) should be (false)
    (imkbPF.resolve("pthr21244_human").isDefined) should be (false)
    (imkbPF.resolve("pthr21244 protein").isDefined) should be (false)
    (imkbPF.resolve("pthr21244 family").isDefined) should be (false)
    (imkbPF.resolve("mutant-pthr21244").isDefined) should be (false)
    (imkbPF.resolve("hk").isDefined) should be (false)
    (imkbPF.resolve("hk_human").isDefined) should be (false)
    (imkbPF.resolve("hk protein").isDefined) should be (false)
    (imkbPF.resolve("hk family").isDefined) should be (false)
    (imkbPF.resolve("mutant-hk").isDefined) should be (false)
  }

  "LocalProteinKBML resolveByASpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbPF.resolveByASpecies("NotInKB", "ant").isDefined) should be (false)
    (imkbPF.resolveByASpecies("NotInKB_human", "ant").isDefined) should be (false)
    (imkbPF.resolveByASpecies("NotInKB protein", "ant").isDefined) should be (false)
    (imkbPF.resolveByASpecies("NotInKB family", "ant").isDefined) should be (false)
    (imkbPF.resolveByASpecies("mutant-NotInKB", "ant").isDefined) should be (false)
    // entry does not have this species:
    (imkbPF.resolveByASpecies("hk", "frog").isDefined) should be (false)
    (imkbPF.resolveByASpecies("hk_human", "frog").isDefined) should be (false)
    (imkbPF.resolveByASpecies("hk protein", "frog").isDefined) should be (false)
    (imkbPF.resolveByASpecies("hk family", "frog").isDefined) should be (false)
    (imkbPF.resolveByASpecies("mutant-hk", "frog").isDefined) should be (false)
    // protein key transforms not applicable for protein families:
    (imkbPF.resolveByASpecies("pthr21244 protein", "human").isDefined) should be (false)
    (imkbPF.resolveByASpecies("PTHR21244 protein", "human").isDefined) should be (false)
    (imkbPF.resolveByASpecies("mutant-pthr21244", "human").isDefined) should be (false)
    (imkbPF.resolveByASpecies("hk protein", "saccharomyces cerevisiae").isDefined) should be (false)
    (imkbPF.resolveByASpecies("mutant-hk", "saccharomyces cerevisiae").isDefined) should be (false)
  }

  "LocalProteinKBML resolveByASpecies" should "work with alternate lookups" in {
    (imkbPF.resolveByASpecies("pthr21244", "human").isDefined) should be (true)
    (imkbPF.resolveByASpecies("pthr21244_human", "human").isDefined) should be (true)
    (imkbPF.resolveByASpecies("pthr21244 family", "human").isDefined) should be (true)
    (imkbPF.resolveByASpecies("hk", "saccharomyces cerevisiae").isDefined) should be (true)
    (imkbPF.resolveByASpecies("hk_human", "saccharomyces cerevisiae").isDefined) should be (true)
    (imkbPF.resolveByASpecies("hk family", "saccharomyces cerevisiae").isDefined) should be (true)
  }

  val setA =   Set("aardvark")
  val setF =   Set("frog")
  val setH =   Set("human")
  val setHM =  Set("human", "mouse")
  val setHMG = Set("human", "mouse", "gorilla")
  "LocalProteinKBML resolveBySpecies" should "should fail despite alternate lookups" in {
    // key not in KB:
    (imkbPF.resolveBySpecies("NotInKB", setA).isDefined) should be (false)
    (imkbPF.resolveBySpecies("NotInKB_human", setA).isDefined) should be (false)
    (imkbPF.resolveBySpecies("NotInKB protein", setA).isDefined) should be (false)
    (imkbPF.resolveBySpecies("NotInKB family", setA).isDefined) should be (false)
    (imkbPF.resolveBySpecies("mutant-NotInKB", setA).isDefined) should be (false)
    (imkbPF.resolveBySpecies("pthr21244 mouse", setH).isDefined) should be (false)
    // entry does not have this species:
    (imkbPF.resolveBySpecies("pthr21244", setF).isDefined) should be (false)
    (imkbPF.resolveBySpecies("pthr21244_human", setF).isDefined) should be (false)
    (imkbPF.resolveBySpecies("pthr21244 protein", setF).isDefined) should be (false)
    (imkbPF.resolveBySpecies("pthr21244 family", setF).isDefined) should be (false)
    (imkbPF.resolveBySpecies("mutant-pthr21244", setF).isDefined) should be (false)
    // entry does not have these species (yeast only):
    (imkbPF.resolveBySpecies("hk", setHM).isDefined) should be (false)
    (imkbPF.resolveBySpecies("hk_human", setHM).isDefined) should be (false)
    (imkbPF.resolveBySpecies("hk protein", setHM).isDefined) should be (false)
    (imkbPF.resolveBySpecies("hk family", setHM).isDefined) should be (false)
    (imkbPF.resolveBySpecies("mutant-hk", setHM).isDefined) should be (false)
    // protein key transforms not applicable for protein families:
    (imkbPF.resolveBySpecies("pthr21244 protein", setH).isDefined) should be (false)
    (imkbPF.resolveBySpecies("PTHR21244 protein", setH).isDefined) should be (false)
    (imkbPF.resolveBySpecies("mutant-pthr21244", setH).isDefined) should be (false)
    (imkbPF.resolveBySpecies("MUTANT-PTHR21244", setH).isDefined) should be (false)
    (imkbPF.resolveBySpecies("pthr21244 protein", setHM).isDefined) should be (false)
    (imkbPF.resolveBySpecies("mutant-pthr21244", setHM).isDefined) should be (false)
    (imkbPF.resolveBySpecies("pthr21244 protein", setHMG).isDefined) should be (false)
    (imkbPF.resolveBySpecies("mutant-pthr21244", setHMG).isDefined) should be (false)
  }

  "LocalProteinKBML resolveBySpecies" should "work with alternate lookups" in {
    (imkbPF.resolveBySpecies("pthr21244", setH).isDefined) should be (true)
    (imkbPF.resolveBySpecies("PTHR21244", setH).isDefined) should be (true)
    (imkbPF.resolveBySpecies("pthr21244_human", setH).isDefined) should be (true)
    (imkbPF.resolveBySpecies("PTHR21244_human", setH).isDefined) should be (true)
    (imkbPF.resolveBySpecies("pthr21244 family", setH).isDefined) should be (true)
    (imkbPF.resolveBySpecies("PTHR21244 family", setH).isDefined) should be (true)

    (imkbPF.resolveBySpecies("pthr21244", setHM).isDefined) should be (true)
    (imkbPF.resolveBySpecies("pthr21244", setHM).get.size == 2) should be (true)
    (imkbPF.resolveBySpecies("pthr21244_human", setHM).isDefined) should be (true)
    (imkbPF.resolveBySpecies("pthr21244_human", setHM).get.size == 2) should be (true)
    (imkbPF.resolveBySpecies("pthr21244 family", setHM).isDefined) should be (true)
    (imkbPF.resolveBySpecies("pthr21244 family", setHM).get.size == 2) should be (true)

    (imkbPF.resolveBySpecies("pthr21244", setHMG).isDefined) should be (true)
    (imkbPF.resolveBySpecies("pthr21244", setHMG).get.size == 2) should be (true)
    (imkbPF.resolveBySpecies("pthr21244_human", setHMG).isDefined) should be (true)
    (imkbPF.resolveBySpecies("pthr21244_human", setHMG).get.size == 2) should be (true)
    (imkbPF.resolveBySpecies("pthr21244 family", setHMG).isDefined) should be (true)
    (imkbPF.resolveBySpecies("pthr21244 family", setHMG).get.size == 2) should be (true)

    (imkbPF.resolveBySpecies("hk", Set("saccharomyces cerevisiae", "ant")).isDefined) should be (true)
    (imkbPF.resolveBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).isDefined) should be (true)
    (imkbPF.resolveBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).get.size == 1) should be (true)
  }

  "LocalProteinKBML resolveHuman" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkbPF.resolveHuman("NotInKB").isDefined) should be (false)
    (imkbPF.resolveHuman("NotInKB_human").isDefined) should be (false)
    (imkbPF.resolveHuman("NotInKB protein").isDefined) should be (false)
    (imkbPF.resolveHuman("NotInKB family").isDefined) should be (false)
    (imkbPF.resolveHuman("mutant-NotInKB").isDefined) should be (false)
    // entry does not have human species (yeast only):
    (imkbPF.resolveHuman("hk").isDefined) should be (false)
    (imkbPF.resolveHuman("hk_human").isDefined) should be (false)
    (imkbPF.resolveHuman("hk protein").isDefined) should be (false)
    (imkbPF.resolveHuman("hk family").isDefined) should be (false)
    (imkbPF.resolveHuman("mutant-hk").isDefined) should be (false)
    // protein key transforms not applicable for protein families:
    (imkbPF.resolveHuman("pthr21244 protein").isDefined) should be (false)
    (imkbPF.resolveHuman("PTHR21244 protein").isDefined) should be (false)
    (imkbPF.resolveHuman("mutant-pthr21244").isDefined) should be (false)
    (imkbPF.resolveHuman("mutant-PTHR21244").isDefined) should be (false)
  }

  "LocalProteinKBML resolveHuman" should "work with alternate lookups" in {
    (imkbPF.resolveHuman("pthr21244").isDefined) should be (true)
    (imkbPF.resolveHuman("PTHR21244").isDefined) should be (true)
    (imkbPF.resolveHuman("pthr21244_human").isDefined) should be (true)
    (imkbPF.resolveHuman("PTHR21244_human").isDefined) should be (true)
    (imkbPF.resolveHuman("pthr21244 family").isDefined) should be (true)
    (imkbPF.resolveHuman("PTHR21244 family").isDefined) should be (true)
  }

}


// Protein family KB using alternate protein resolutions
class TestProtFamKBL extends IMKBFamilyLookup {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/interpro/", "interpro", "MIR:00000011"),
                   StaticProteinFamilyFilename, true)
}
