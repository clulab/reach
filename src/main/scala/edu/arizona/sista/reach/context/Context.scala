package edu.arizona.sista.reach.context

import edu.arizona.sista.struct.Interval

trait GlobalContext{
  /**
   * ix is the index of the sentence relative to the document
   * Rreturns a map, where the key is the context type and the value a Sequence
   * of tuples where the first element is a grounded id and the second the interval of the
   * sentence where it appears
   */
  def query(ix:Int):Map[String, Seq[(String, Interval)]]
}

trait ContextEngine{
  def extractContext
  def integrateContext
}
