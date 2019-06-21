package org.clulab.reach.context.context_exec

import org.clulab.context.utils.AggregatedContextInstance

class CrossValBySentDist(testAggrRows: Seq[AggregatedContextInstance]) {

  def performCrossVal(): Unit = {
    println(testAggrRows.size)
    if(testAggrRows.size > 0)
      println("Will set up cross val soon, doing this just to make sure I didn't break anything.")
  }

}
