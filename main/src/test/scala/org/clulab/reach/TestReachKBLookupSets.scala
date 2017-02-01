package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import org.clulab.reach.grounding._
import org.clulab.reach.grounding.KBLookupSet._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests of additional lookup tables and knowledge bases.
  *   Written by: Tom Hicks. 7/10/2016.
  *   Last Modified: Rename class. Update for removal of misc lookups class. Remove unused test.
  */
class TestReachKBLookupSets extends FlatSpec with Matchers {

  // Tests of the singleton Protein Kinases set:
  val pkl = ProteinKinaseIds

  "Protein Kinases" should "test that contains method works" in {
    (pkl.contains("NOT-IN-KB")) should be (false) // not in KB
    (pkl.contains("not-in-kb")) should be (false) // not in KB
    (pkl.contains("P00000")) should be (false)    // not in KB
    (pkl.contains("Q00000")) should be (false)    // not in KB
    (pkl.contains("Q1")) should be (false)        // not in KB
    (pkl.contains("QPQPQP")) should be (false)    // not in KB
    (pkl.contains("Q99999")) should be (false)    // not a kinase
    (pkl.contains("Q99998")) should be (false)    // not a kinase
    (pkl.contains("O08560")) should be (true)     // first entry in UP list
    (pkl.contains("o08560")) should be (false)    // entries are uppercase
    (pkl.contains("P31749")) should be (true)     // AKT1_HUMAN
    (pkl.contains("p31749")) should be (false)    // entries are uppercase
    (pkl.contains("P31750")) should be (true)     // AKT1_MOUSE
    (pkl.contains("Q13882")) should be (true)     // PTK6_HUMAN
    (pkl.contains("Q8AYK6")) should be (true)     // last entry in UP list
    (pkl.contains("Q2M2I8")) should be (true)     // first in SP list
    (pkl.contains("P43404")) should be (true)     // last in SP list
    (pkl.contains("Q3UHJ0")) should be (true)     // middle of SP list
  }

  "Protein Kinases" should "test that isProteinKinase method works" in {
    (isProteinKinase("NOT-IN-KB")) should be (false) // not in KB
    (isProteinKinase("not-in-kb")) should be (false) // not in KB
    (isProteinKinase("P00000")) should be (false)    // not in KB
    (isProteinKinase("Q00000")) should be (false)    // not in KB
    (isProteinKinase("Q1")) should be (false)        // not in KB
    (isProteinKinase("QPQPQP")) should be (false)    // not in KB
    (isProteinKinase("Q99999")) should be (false)    // not a kinase
    (isProteinKinase("Q99998")) should be (false)    // not a kinase
    (isProteinKinase("O08560")) should be (true)     // first entry in UP list
    (isProteinKinase("o08560")) should be (false)    // entries are uppercase
    (isProteinKinase("P31749")) should be (true)     // AKT1_HUMAN
    (isProteinKinase("p31749")) should be (false)    // entries are uppercase
    (isProteinKinase("P31750")) should be (true)     // AKT1_MOUSE
    (isProteinKinase("Q13882")) should be (true)     // PTK6_HUMAN
    (isProteinKinase("Q8AYK6")) should be (true)     // last entry in UP list
    (isProteinKinase("Q2M2I8")) should be (true)     // first in SP list
    (isProteinKinase("P43404")) should be (true)     // last in SP list
    (isProteinKinase("Q3UHJ0")) should be (true)     // middle of SP list
  }


  // Tests of the singleton Protein Domains Short Names set:
  val pds = ProteinDomainShortNames

  "Protein Domain Short Names" should "test that contains method works" in {
    (pds.contains("NOT-IN-KB")) should be (false) // not in KB
    (pds.contains("not-in-kb")) should be (false) // not in KB
    (pds.contains("P00000")) should be (false)    // not in KB
    (pds.contains("Q00000")) should be (false)    // not in KB
    (pds.contains("Q1")) should be (false)        // not in KB
    (pds.contains("P31749")) should be (false)    // not in KB
    (pds.contains("14_3_3")) should be (true)     // first entry in list
    (pds.contains("AAA")) should be (true)
    (pds.contains("aaa")) should be (true)
    (pds.contains("AICARFT_IMPCHas")) should be (true)
    (pds.contains("aicarft_impchas")) should be (true)
    (pds.contains("HAT")) should be (true)
    (pds.contains("hat")) should be (true)
    (pds.contains("ZU5")) should be (true)
    (pds.contains("zu5")) should be (true)        // last entry in list
    (pds.contains("Germane")) should be (true)
    (pds.contains("germane")) should be (true)    // odd but true
  }

  "Protein Domain Short Names" should "test that isProteinDomain method works" in {
    (isProteinDomain("NOT-IN-KB")) should be (false) // not in KB
    (isProteinDomain("not-in-kb")) should be (false) // not in KB
    (isProteinDomain("P00000")) should be (false)    // not in KB
    (isProteinDomain("Q00000")) should be (false)    // not in KB
    (isProteinDomain("Q1")) should be (false)        // not in KB
    (isProteinDomain("P31749")) should be (false)    // not in KB
    (isProteinDomain("14_3_3")) should be (true)     // first entry in list
    (isProteinDomain("AAA")) should be (true)        // case should not matter
    (isProteinDomain("aaa")) should be (true)
    (isProteinDomain("AICARFT_IMPCHas")) should be (true)
    (isProteinDomain("aicarft_impchas")) should be (true)
    (isProteinDomain("HAT")) should be (true)
    (isProteinDomain("hat")) should be (true)
    (isProteinDomain("ZU5")) should be (true)
    (isProteinDomain("zu5")) should be (true)        // last entry in list
    (isProteinDomain("Germane")) should be (true)
    (isProteinDomain("germane")) should be (true)    // odd but true
  }


  // Tests of the singleton Gene Name Prefixes set:
  val gnp = GeneNamePrefixes

  "Gene Name Prefixes" should "test that contains method works" in {
    (gnp.contains("NOT-IN-KB")) should be (false) // not in KB
    (gnp.contains("not-in-kb")) should be (false) // not in KB
    (gnp.contains("prefix")) should be (false)    // not in KB
    (gnp.contains("suffix")) should be (false)    // not in KB
    (gnp.contains("affix")) should be (false)     // not in KB
    (gnp.contains("uaz")) should be (false)       // not in KB
    (gnp.contains("activated")) should be (true)  // first entry in list
    (gnp.contains("ACTIVATED")) should be (true)
    (gnp.contains("flag")) should be (true)
    (gnp.contains("Flag")) should be (true)
    (gnp.contains("FLAG")) should be (true)
    (gnp.contains("gst")) should be (true)
    (gnp.contains("GST")) should be (true)
    (gnp.contains("phospho")) should be (true)
    (gnp.contains("phosphor")) should be (true)
    (gnp.contains("phosphorylated")) should be (true)
    (gnp.contains("shRNA")) should be (true)
    (gnp.contains("shrna")) should be (true)      // last entry in list
  }

  "Gene Name Prefixes" should "test that isGeneNamePrefix method works" in {
    (isGeneNamePrefix("NOT-IN-KB")) should be (false) // not in KB
    (isGeneNamePrefix("not-in-kb")) should be (false) // not in KB
    (isGeneNamePrefix("prefix")) should be (false)    // not in KB
    (isGeneNamePrefix("suffix")) should be (false)    // not in KB
    (isGeneNamePrefix("affix")) should be (false)     // not in KB
    (isGeneNamePrefix("uaz")) should be (false)       // not in KB
    (isGeneNamePrefix("activated")) should be (true)  // first entry in list
    (isGeneNamePrefix("ACTIVATED")) should be (true)  // case should not matter
    (isGeneNamePrefix("flag")) should be (true)
    (isGeneNamePrefix("Flag")) should be (true)       // case should not matter
    (isGeneNamePrefix("FLAG")) should be (true)       // case should not matter
    (isGeneNamePrefix("gst")) should be (true)
    (isGeneNamePrefix("GST")) should be (true)        // case should not matter
    (isGeneNamePrefix("phospho")) should be (true)
    (isGeneNamePrefix("phosphor")) should be (true)
    (isGeneNamePrefix("phosphorylated")) should be (true)
    (isGeneNamePrefix("shrna")) should be (true)      // last entry in list
    (isGeneNamePrefix("shRNA")) should be (true)      // last entry in list
  }

}
