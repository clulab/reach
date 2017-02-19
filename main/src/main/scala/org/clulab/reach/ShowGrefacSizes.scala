package org.clulab.reach

// import java.lang.instrument._
// import java.lang.Runtime
import org.github.jamm._

import org.clulab.reach.grounding._

/**
 * Test of data structure sizes in new grounding refactoring.
 */
object ShowGrefacSizes extends App {

  val meter: MemoryMeter = new MemoryMeter

  println(s"Creating ReachIMKBMentionLookups object...")
  val riml = ReachIMKBMentionLookups

  println(s"Sizing ReachIMKBMentionLookups object...")
  println(s"${meter.measureDeep(riml)}")

}
