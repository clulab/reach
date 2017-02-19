package org.clulab.reach

import org.github.jamm._

import org.clulab.reach.grounding._

/**
 * Test of grounding data structure sizes by reachability.
 */
object ShowMemReachable extends App {

  val meter: MemoryMeter = new MemoryMeter

  println(s"Creating ReachIMKBMentionLookups object...")
  val riml = ReachIMKBMentionLookups

  println(s"Sizing ReachIMKBMentionLookups object...")
  println(s"${meter.measureDeep(riml)}")

}
