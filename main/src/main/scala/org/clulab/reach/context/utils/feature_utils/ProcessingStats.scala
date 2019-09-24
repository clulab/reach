package org.clulab.reach.context.feature_utils

class ProcessingStats {

  private var papers: Long = 0L

  /** Sum of paper processing times, in seconds. */
  private var totalDuration: Long = 0L

  /** Global start time in nanoseconds for this processing run. */
  val startNS: Long = System.nanoTime

  /** Return the number of papers successfully processed. */
  def count: Long = papers

  /** Update the statistics with the processing duration, in seconds, for another
    * completed paper.
    * Returns a sequence of paper count and average processing time, in seconds.
    */
  def update (duration:Long): Seq[Long] = {
    this.synchronized {
      papers += 1
      totalDuration += duration
      val avg = totalDuration / papers
      return Seq(papers, avg)
    }
  }

}
