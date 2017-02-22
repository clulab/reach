package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.KBLookupSet._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests of KBLookupSets.
  *   Written by: Tom Hicks. 1/24/2017.
  *   Last Modified: Initial creation.
  */
class TestKBLookupSet extends FlatSpec with Matchers {

  // create empty lookup set with one canonical key transform:
  val canonKTs = Seq( canonicalKT _)
  val lus1 = new KBLookupSet(canonKTs)

  // create empty lookup set with several types of storage key transforms:
  val multiKTs = Seq( identityKT _, lowercaseKT _, canonicalKT _)
  val lus2 = new KBLookupSet(multiKTs)

  "Empty LUS 1" should "not be able to lookup anything while empty" in {
    (lus1.contains("")) should be (false)
    (lus1.contains("abc")) should be (false)
    (lus1.contains("ABC")) should be (false)
    (lus1.contains("a b c")) should be (false)
    (lus1.contains("A B C")) should be (false)
    (lus1.contains("a/b/c")) should be (false)
    (lus1.contains("A/B-C DE")) should be (false)
  }

  "Empty LUS 1" should "add unique items only" in {
    lus1.addEntries(Seq("abc", "ABC", "a b c", "A B C", "A-b-C", "A/b-C", "a/b/c"))
    (lus1.size) should be (1)               // texts should all map to same item
    // lus1.dump                             // DEBUGGING
  }

  "Empty LUS 1" should "be able to resolve lookups based on previous filling" in {
    // lus1.dump                              // DEBUGGING
    (lus1.contains("")) should be (false)
    (lus1.contains("abc")) should be (true)
    (lus1.contains("ABC")) should be (true)
    (lus1.contains("a b c")) should be (true)
    (lus1.contains("A B C")) should be (true)
    (lus1.contains("A-b-C")) should be (true)
    (lus1.contains("A/b-C")) should be (true)
    (lus1.contains("a/b/c")) should be (true)
    (lus1.contains("a/b")) should be (false)
    (lus1.contains("a/b/d")) should be (false)
    (lus1.contains("A/B-C D")) should be (false)
  }


  "Empty KB 2" should "add unique keys only" in {
    lus2.addEntries(Seq(
      "abc", "a-b-c", "ABC", "abC", "Abc",
      "a/b/c/", "AB/C", "A-B/c", "ABC", "a-B/C",
      "XYZ", "X-y-Z", "x y z", "XY-Z", "x/Y/Z"
    ))
    (lus2.size) should be (20)
    // lus2.dump                             // DEBUGGING
  }

  "Empty KB 2" should "be able to resolve lookups based on previous filling" in {
    // lus2.dump                              // DEBUGGING
    (lus2.contains("")) should be (false)
    (lus2.contains("abc")) should be (true)
    (lus2.contains("Abc")) should be (true)
    (lus2.contains("aBc")) should be (true)
    (lus2.contains("abC")) should be (true)
    (lus2.contains("ABc")) should be (true)
    (lus2.contains("aBC")) should be (true)
    (lus2.contains("ABC")) should be (true)
    (lus2.contains("a b c")) should be (true)
    (lus2.contains("A B C")) should be (true)
    (lus2.contains("A-B/C")) should be (true)
    (lus2.contains("A-b-C")) should be (true)
    (lus2.contains("A/B-C")) should be (true)
    (lus2.contains("A/B/C")) should be (true)
    (lus2.contains("A/b-C")) should be (true)
    (lus2.contains("a/b/c")) should be (true)
    (lus2.contains("xyz")) should be (true)
    (lus2.contains("XYZ")) should be (true)
    (lus2.contains("x y z")) should be (true)
    (lus2.contains("X Y Z")) should be (true)
    (lus2.contains("X-Y/Z")) should be (true)
    (lus2.contains("X/Y-Z")) should be (true)
    (lus2.contains("XY Z")) should be (true)
    (lus2.contains("x-y-z")) should be (true)
    (lus2.contains("x/y/z")) should be (true)
    (lus2.contains("a/b")) should be (false)
    (lus2.contains("a/b/d")) should be (false)
    (lus2.contains("A/B-C D")) should be (false)
    (lus2.contains("cba")) should be (false)
  }
}
