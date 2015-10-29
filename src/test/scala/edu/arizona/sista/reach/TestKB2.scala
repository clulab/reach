package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.grounding2._

/**
 * Unit tests to ensure the in-memory KB is working for grounding.
 *   Written by: Tom Hicks. 10/26/2015.
 *   Last Modified: Initial creation.
 */
class TestKB2 extends FlatSpec with Matchers {

  // test KBEntry
  "InMemoryKB COL-2" should "read and test data from COL-2 TSV file" in {
    val imkb2 = new InMemoryKB(
      new KBMetaInfo("http://identifiers.org/uazclu/", "UAZ", "MIR:00000000"),
                     "uniprot-subcellular-locations.tsv")
    (imkb2.lookup("NOT-IN-KB").isEmpty) should be (true)
    (imkb2.lookup("dendrite").isDefined) should be (true)
  }

  "InMemoryKB COL-3" should "read and test data from COL-3 gzipped TSV file" in {
    val imkb3 = new InMemoryKB(
      new KBMetaInfo("http://identifiers.org/uazclu/", "UAZ", "MIR:00000000"),
                     "ProteinFamilies.tsv.gz")
    (imkb3.lookup("NOT-IN-KB").isEmpty) should be (true)
    (imkb3.lookup("PTHR21244").isDefined) should be (true)
    // the following not yet implemented:
    (imkb3.lookupHuman("PTHR21244").isDefined) should be (false)
    (imkb3.lookupByASpecies("PTHR21244", "human").isDefined) should be (false)
    (imkb3.lookupByASpecies("PTHR21244", "mouse").isDefined) should be (false)
    (imkb3.lookupByASpecies("PTHR21244", "aardvark").isDefined) should be (false)
    (imkb3.lookupBySpecies("PTHR21244", Set("human")).isDefined) should be (false)
    (imkb3.lookupBySpecies("PTHR21244", Set("human", "mouse")).isDefined) should be (false)
    (imkb3.lookupBySpecies("PTHR21244", Set("aardvark")).isDefined) should be (false)
  }

}
