package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachRLKBLookups._

/**
  * Unit tests of reverse lookup knowledge bases.
  *   Written by: Tom Hicks. 7/10/2016.
  *   Last Modified: Initial creation.
  */
class TestReverseLookupKBs extends FlatSpec with Matchers {

  // Tests of the singleton ReverseLookup Protein Kinases KB:
  val pkl = ProteinKinasesLookup

  // pkl.theKB.foreach { case (k, entries) =>              // for DEBUGGING
  //   println(s"${k} => ${entries.toString()}") }         // for DEBUGGING

  "ProteinKinases KB" should "show that containId works" in {
    (pkl.containsId("NOT-IN-KB")) should be (false) // not in KB
    (pkl.containsId("not-in-kb")) should be (false) // not in KB
    (pkl.containsId("P00000")) should be (false)    // not in KB
    (pkl.containsId("Q1")) should be (false)        // not in KB
    (pkl.containsId("O08560")) should be (true)     // first entry in file
    (pkl.containsId("P31750")) should be (true)     // AKT1
    (pkl.containsId("Q8AYK6")) should be (true)     // last entry in file
    (pkl.containsId("Q0VQM6")) should be (true)
  }

  // "ProteinKinases" should "" in {
  //   (pkl.lookupId("NOT-IN-KB").isDefined) should be (false) // not in KB
  //   (pkl.lookupId("not-in-kb").isDefined) should be (false) // not in KB
  //   (pkl.lookupId("P00000").isDefined) should be (false)
  //   (pkl.lookupId("Q1").isDefined) should be (false)
  //   (pkl.lookupId("P31750").isDefined) should be (true)
  //   (pkl.lookupId("apoptosis").isDefined) should be (true)
  //   (pkl.lookupId("nadph").isDefined) should be (true)
  //   (pkl.lookupId("ros").isDefined) should be (true)
  // }

}
