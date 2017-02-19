package org.clulab.reach

import java.lang.Runtime

import org.clulab.reach.grounding._

/**
 * Test of grounding data structure sizes by allocation.
 */
object ShowMemGC extends App {

  System.gc()
  System.gc()
  System.gc()

  val start = Runtime.getRuntime().freeMemory();
  println(s"Starting FreeMemory=${start}")

  println(s"Creating ReachIMKBMentionLookups object...")
  val riml = ReachIMKBMentionLookups

  System.gc()
  System.gc()

  val stop = Runtime.getRuntime().freeMemory();
  println(s"Ending FreeMemory=${stop}")
  println(s"Memory Used=${start - stop}")

}
