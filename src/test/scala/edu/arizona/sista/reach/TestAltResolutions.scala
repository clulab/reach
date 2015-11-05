package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.grounding2._
import edu.arizona.sista.reach.grounding2.LocalKBConstants._

/**
 * Unit tests to ensure alternate resolutions are working for KB grounding.
 *   Written by: Tom Hicks. 11/4/2015.
 *   Last Modified: Initial creation.
 */
class TestAltResolutions extends FlatSpec with Matchers {

  val imkb4 = new TestProtFamKBML           // defined after this class (LOOK BELOW)

  println(imkb4)                            // REMOVE LATER

  // this KB includes species, therefore plain resolve should always fail:
  "LocalProteinKBML resolve" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkb4.resolve("NOTINKB").isDefined) should be (false)
    (imkb4.resolve("notinkb").isDefined) should be (false)
    (imkb4.resolve("notinkb_human").isDefined) should be (false)
    (imkb4.resolve("notinkb protein").isDefined) should be (false)
    (imkb4.resolve("notinkb family").isDefined) should be (false)
    // entry has a species:
    (imkb4.resolve("PTHR21244").isDefined) should be (false)
    (imkb4.resolve("pthr21244").isDefined) should be (false)
    (imkb4.resolve("pthr21244_human").isDefined) should be (false)
    (imkb4.resolve("pthr21244 protein").isDefined) should be (false)
    (imkb4.resolve("pthr21244 family").isDefined) should be (false)
    (imkb4.resolve("mutant-pthr21244").isDefined) should be (false)
    (imkb4.resolve("hk").isDefined) should be (false)
    (imkb4.resolve("hk_human").isDefined) should be (false)
    (imkb4.resolve("hk protein").isDefined) should be (false)
    (imkb4.resolve("hk family").isDefined) should be (false)
    (imkb4.resolve("mutant-hk").isDefined) should be (false)
  }

  "LocalProteinKBML resolveByASpecies" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkb4.resolveByASpecies("NotInKB", "ant").isDefined) should be (false)
    (imkb4.resolveByASpecies("NotInKB_human", "ant").isDefined) should be (false)
    (imkb4.resolveByASpecies("NotInKB protein", "ant").isDefined) should be (false)
    (imkb4.resolveByASpecies("NotInKB family", "ant").isDefined) should be (false)
    (imkb4.resolveByASpecies("mutant-NotInKB", "ant").isDefined) should be (false)
    // entry does not have this species:
    (imkb4.resolveByASpecies("hk", "frog").isDefined) should be (false)
    (imkb4.resolveByASpecies("hk_human", "frog").isDefined) should be (false)
    (imkb4.resolveByASpecies("hk protein", "frog").isDefined) should be (false)
    (imkb4.resolveByASpecies("hk family", "frog").isDefined) should be (false)
    (imkb4.resolveByASpecies("mutant-hk", "frog").isDefined) should be (false)
  }

  "LocalProteinKBML resolveByASpecies" should "work with alternate lookups" in {
    (imkb4.resolveByASpecies("pthr21244", "human").isDefined) should be (true)
    (imkb4.resolveByASpecies("pthr21244_human", "human").isDefined) should be (true)
    (imkb4.resolveByASpecies("pthr21244 protein", "human").isDefined) should be (true)
    (imkb4.resolveByASpecies("PTHR21244 protein", "human").isDefined) should be (true)
    (imkb4.resolveByASpecies("pthr21244 family", "human").isDefined) should be (true)
    (imkb4.resolveByASpecies("mutant-pthr21244", "human").isDefined) should be (true)
    (imkb4.resolveByASpecies("hk", "saccharomyces cerevisiae").isDefined) should be (true)
    (imkb4.resolveByASpecies("hk_human", "saccharomyces cerevisiae").isDefined) should be (true)
    (imkb4.resolveByASpecies("hk protein", "saccharomyces cerevisiae").isDefined) should be (true)
    (imkb4.resolveByASpecies("hk family", "saccharomyces cerevisiae").isDefined) should be (true)
    (imkb4.resolveByASpecies("mutant-hk", "saccharomyces cerevisiae").isDefined) should be (true)
  }

  val setA =   Set("aardvark")
  val setF =   Set("frog")
  val setH =   Set("human")
  val setHM =  Set("human", "mouse")
  val setHMG = Set("human", "mouse", "gorilla")
  "LocalProteinKBML resolveBySpecies" should "should fail despite alternate lookups" in {
    // key not in KB:
    (imkb4.resolveBySpecies("NotInKB", setA).isDefined) should be (false)
    (imkb4.resolveBySpecies("NotInKB_human", setA).isDefined) should be (false)
    (imkb4.resolveBySpecies("NotInKB protein", setA).isDefined) should be (false)
    (imkb4.resolveBySpecies("NotInKB family", setA).isDefined) should be (false)
    (imkb4.resolveBySpecies("mutant-NotInKB", setA).isDefined) should be (false)
    (imkb4.resolveBySpecies("pthr21244 mouse", setH).isDefined) should be (false)
    // entry does not have this species:
    (imkb4.resolveBySpecies("pthr21244", setF).isDefined) should be (false)
    (imkb4.resolveBySpecies("pthr21244_human", setF).isDefined) should be (false)
    (imkb4.resolveBySpecies("pthr21244 protein", setF).isDefined) should be (false)
    (imkb4.resolveBySpecies("pthr21244 family", setF).isDefined) should be (false)
    (imkb4.resolveBySpecies("mutant-pthr21244", setF).isDefined) should be (false)
    // entry does not have these species (yeast only):
    (imkb4.resolveBySpecies("hk", setHM).isDefined) should be (false)
    (imkb4.resolveBySpecies("hk_human", setHM).isDefined) should be (false)
    (imkb4.resolveBySpecies("hk protein", setHM).isDefined) should be (false)
    (imkb4.resolveBySpecies("hk family", setHM).isDefined) should be (false)
    (imkb4.resolveBySpecies("mutant-hk", setHM).isDefined) should be (false)
  }

  "LocalProteinKBML resolveBySpecies" should "work with alternate lookups" in {
    (imkb4.resolveBySpecies("pthr21244", setH).isDefined) should be (true)
    (imkb4.resolveBySpecies("PTHR21244", setH).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244_human", setH).isDefined) should be (true)
    (imkb4.resolveBySpecies("PTHR21244_human", setH).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244 protein", setH).isDefined) should be (true)
    (imkb4.resolveBySpecies("PTHR21244 protein", setH).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244 family", setH).isDefined) should be (true)
    (imkb4.resolveBySpecies("PTHR21244 family", setH).isDefined) should be (true)
    (imkb4.resolveBySpecies("mutant-pthr21244", setH).isDefined) should be (true)
    (imkb4.resolveBySpecies("MUTANT-PTHR21244", setH).isDefined) should be (true)

    (imkb4.resolveBySpecies("pthr21244", setHM).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244", setHM).get.size == 2) should be (true)
    (imkb4.resolveBySpecies("pthr21244_human", setHM).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244_human", setHM).get.size == 2) should be (true)
    (imkb4.resolveBySpecies("pthr21244 protein", setHM).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244 protein", setHM).get.size == 2) should be (true)
    (imkb4.resolveBySpecies("pthr21244 family", setHM).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244 family", setHM).get.size == 2) should be (true)
    (imkb4.resolveBySpecies("mutant-pthr21244", setHM).isDefined) should be (true)
    (imkb4.resolveBySpecies("mutant-pthr21244", setHM).get.size == 2) should be (true)

    (imkb4.resolveBySpecies("pthr21244", setHMG).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244", setHMG).get.size == 2) should be (true)
    (imkb4.resolveBySpecies("pthr21244_human", setHMG).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244_human", setHMG).get.size == 2) should be (true)
    (imkb4.resolveBySpecies("pthr21244 protein", setHMG).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244 protein", setHMG).get.size == 2) should be (true)
    (imkb4.resolveBySpecies("pthr21244 family", setHMG).isDefined) should be (true)
    (imkb4.resolveBySpecies("pthr21244 family", setHMG).get.size == 2) should be (true)
    (imkb4.resolveBySpecies("mutant-pthr21244", setHMG).isDefined) should be (true)
    (imkb4.resolveBySpecies("mutant-pthr21244", setHMG).get.size == 2) should be (true)

    (imkb4.resolveBySpecies("hk", Set("saccharomyces cerevisiae", "ant")).isDefined) should be (true)
    (imkb4.resolveBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).isDefined) should be (true)
    (imkb4.resolveBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).get.size == 1) should be (true)
  }

  "LocalProteinKBML resolveHuman" should "fail despite alternate lookups" in {
    // key not in KB:
    (imkb4.resolveHuman("NotInKB").isDefined) should be (false)
    (imkb4.resolveHuman("NotInKB_human").isDefined) should be (false)
    (imkb4.resolveHuman("NotInKB protein").isDefined) should be (false)
    (imkb4.resolveHuman("NotInKB family").isDefined) should be (false)
    (imkb4.resolveHuman("mutant-NotInKB").isDefined) should be (false)
    // entry does not have human species (yeast only):
    (imkb4.resolveHuman("hk").isDefined) should be (false)
    (imkb4.resolveHuman("hk_human").isDefined) should be (false)
    (imkb4.resolveHuman("hk protein").isDefined) should be (false)
    (imkb4.resolveHuman("hk family").isDefined) should be (false)
    (imkb4.resolveHuman("mutant-hk").isDefined) should be (false)
  }

  "LocalProteinKBML resolveHuman" should "work with alternate lookups" in {
    (imkb4.resolveHuman("pthr21244").isDefined) should be (true)
    (imkb4.resolveHuman("PTHR21244").isDefined) should be (true)
    (imkb4.resolveHuman("pthr21244_human").isDefined) should be (true)
    (imkb4.resolveHuman("PTHR21244_human").isDefined) should be (true)
    (imkb4.resolveHuman("pthr21244 protein").isDefined) should be (true)
    (imkb4.resolveHuman("PTHR21244 protein").isDefined) should be (true)
    (imkb4.resolveHuman("pthr21244 family").isDefined) should be (true)
    (imkb4.resolveHuman("PTHR21244 family").isDefined) should be (true)
    (imkb4.resolveHuman("mutant-pthr21244").isDefined) should be (true)
    (imkb4.resolveHuman("mutant-PTHR21244").isDefined) should be (true)
  }

}


// Protein family KB using alternate protein resolutions
class TestProtFamKBML extends LocalProteinKBML {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/interpro/", "interpro", "MIR:00000011"),
                   StaticProteinFamilyFilename)
}
