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

  "LocalProteinKBML resolve" should "work without using alternate lookups" in {
    (imkb4.resolve("NOT-IN-KB").isDefined) should be (false) // not in KB
    (imkb4.resolve("PTHR21244").isDefined) should be (false) // uppercase
    (imkb4.resolve("not-in-kb").isDefined) should be (false) // not it KB
    (imkb4.resolve("pthr21244").isDefined) should be (false) // has a species
    (imkb4.resolve("hk").isDefined) should be (false)        // has a species
  }

  "LocalProteinKBML resolveByASpecies" should "work without using alternate lookups" in {
    (imkb4.resolveByASpecies("NOT-IN-KB", "human").isDefined) should be (false) // not in KB
    (imkb4.resolveByASpecies("pthr21244", "human").isDefined) should be (true)
    (imkb4.resolveByASpecies("PTHR21244", "human").isDefined) should be (true)
    (imkb4.resolveByASpecies("pthr21244", "HUMAN").isDefined) should be (true)
    (imkb4.resolveByASpecies("pthr21244", "mouse").isDefined) should be (true)
    (imkb4.resolveByASpecies("pthr21244", "aardvark").isDefined) should be (false) // not in KB
    (imkb4.resolveByASpecies("hk", "saccharomyces cerevisiae").isDefined) should be (true)
  }

  "LocalProteinKBML resolveBySpecies" should "work without using alternate lookups" in {
    (imkb4.resolveBySpecies("pthr21244", Set("aardvark")).isDefined) should be (false)
    (imkb4.resolveBySpecies("pthr21244", Set("human")).isDefined) should be (true)
    val pt = imkb4.resolveBySpecies("pthr21244", Set("human", "mouse"))
    (pt.isDefined) should be (true)
    (pt.get.size == 2) should be (true)
    (imkb4.resolveBySpecies("pthr21244", Set("human","mouse","gorilla")).isDefined) should be (true)
    val pt2 = (imkb4.resolveBySpecies("pthr21244", Set("human", "mouse","gorilla")))
    (pt2.isDefined) should be (true)
    (pt2.get.size == 2) should be (true)
    (imkb4.resolveBySpecies("hk", Set("human", "mouse")).isDefined) should be (false)
    (imkb4.resolveBySpecies("hk", Set("saccharomyces cerevisiae", "ant")).isDefined) should be (true)
    (imkb4.resolveBySpecies("hk", Set("ant", "saccharomyces cerevisiae")).isDefined) should be (true)
    (imkb4.resolveBySpecies(
      "hk", Set("ant", "saccharomyces cerevisiae")).get.size == 1) should be (true)
  }

  "LocalProteinKBML resolveHuman" should "work without using alternate lookups" in {
    (imkb4.resolveHuman("hk").isDefined) should be (false) // yeast only
    (imkb4.resolveHuman("PTHR21244").isDefined) should be (true)
    (imkb4.resolveHuman("pthr21244").isDefined) should be (true)
  }

}


// Protein family KB using alternate protein resolutions
class TestProtFamKBML extends LocalProteinKBML {
  val memoryKB = new InMemoryKB(
    new KBMetaInfo("http://identifiers.org/interpro/", "interpro", "MIR:00000011"),
                   StaticProteinFamilyFilename)
}
