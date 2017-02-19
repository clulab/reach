package org.clulab.reach

import org.github.jamm._

import org.clulab.reach.grounding._

/**
  * Test of grounding data structure sizes by reachability.
  *   Last modified: Add measure of reverse species lookup.
  */
object ShowMemReachable extends App {

  val meter: MemoryMeter = new MemoryMeter

  println(s"Creating ReachIMKBMentionLookups object...")
  val riml = ReachIMKBMentionLookups
  println(s"Creating ReachMiscLookups object...")
  val rsl = ReachMiscLookups

  println(s"Sizing ReachIMKBMentionLookups object...")
  val rimlSize = meter.measureDeep(riml)
  println(s"RIML=${rimlSize}")

  println(s"Sizing Reverse Species Lookup object...")
  val rslSize = meter.measureDeep(rsl.ReverseSpeciesLookup)
  println(s"RSL=${rslSize}")

  println(s"TOTAL=${rimlSize + rslSize}")

}
