package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import org.clulab.reach.TestUtils._
import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachMiscLookups._

/**
  * Unit tests of additional lookup tables and knowledge bases.
  *   Written by: Tom Hicks. 7/10/2016.
  *   Last Modified: Update for separation of gene name affixes.
  */
class TestMiscLookups extends FlatSpec with Matchers {

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
    (pds.contains("AAA")) should be (false)       // entries are lowercase
    (pds.contains("aaa")) should be (true)
    (pds.contains("AICARFT_IMPCHas")) should be (false) // entries are lowercase
    (pds.contains("aicarft_impchas")) should be (true)
    (pds.contains("HAT")) should be (false)       // entries are lowercase
    (pds.contains("hat")) should be (true)
    (pds.contains("ZU5")) should be (false)       // entries are lowercase
    (pds.contains("zu5")) should be (true)        // last entry in list
    (pds.contains("Germane")) should be (false)    // entries are lowercase
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
    (gnp.contains("ACTIVATED")) should be (false) // entries are lowercase
    (gnp.contains("flag")) should be (true)
    (gnp.contains("Flag")) should be (false)      // entries are lowercase
    (gnp.contains("FLAG")) should be (false)      // entries are lowercase
    (gnp.contains("gst")) should be (true)
    (gnp.contains("GST")) should be (false)       // entries are lowercase
    (gnp.contains("phospho")) should be (true)
    (gnp.contains("phosphor")) should be (true)
    (gnp.contains("phosphorylated")) should be (true)
    (gnp.contains("yfp")) should be (true)        // last entry in list
    (gnp.contains("YFP")) should be (false)       // entries are lowercase
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
    (isGeneNamePrefix("yfp")) should be (true)        // last entry in list
    (isGeneNamePrefix("YFP")) should be (true)        // last entry in list
  }

  // Tests of the singleton Gene Name Suffixes set:
  val gns = GeneNameSuffixes

  "Gene Name Suffixes" should "test that contains method works" in {
    (gns.contains("NOT-IN-KB")) should be (false) // not in KB
    (gns.contains("not-in-kb")) should be (false) // not in KB
    (gns.contains("prefix")) should be (false)    // not in KB
    (gns.contains("suffix")) should be (false)    // not in KB
    (gns.contains("affix")) should be (false)     // not in KB
    (gns.contains("uaz")) should be (false)       // not in KB
    (gns.contains("egfp")) should be (true)       // first entry in list
    (gns.contains("EFGP")) should be (false)      // entries are lowercase
    (gns.contains("gfp")) should be (true)        // last entry in list
    (gns.contains("GFP")) should be (false)       // entries are lowercase
  }

  "Gene Name Suffixes" should "test that isGeneNameSuffix method works" in {
    (isGeneNameSuffix("NOT-IN-KB")) should be (false) // not in KB
    (isGeneNameSuffix("not-in-kb")) should be (false) // not in KB
    (isGeneNameSuffix("prefix")) should be (false)    // not in KB
    (isGeneNameSuffix("suffix")) should be (false)    // not in KB
    (isGeneNameSuffix("affix")) should be (false)     // not in KB
    (isGeneNameSuffix("uaz")) should be (false)       // not in KB
    (isGeneNameSuffix("egfp")) should be (true)       // first entry in list
    (isGeneNameSuffix("EGFP")) should be (true)       // case should not matter
    (isGeneNameSuffix("gfp")) should be (true)        // last entry in list
    (isGeneNameSuffix("GFP")) should be (true)        // last entry in list
  }

}

// Save: useful for testing reverse lookup KBs (TBD):
// pds.theKB.foreach { case (k, entries) =>              // for DEBUGGING
//   println(s"${k} => ${entries.toString()}") }         // for DEBUGGING
