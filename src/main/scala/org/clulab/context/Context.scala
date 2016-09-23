package org.clulab.context

trait Context {

  var context: Option[Map[String, Seq[String]]] = None

  /** Tell whether context map exists and is non-empty or not. */
  def hasContext (): Boolean = context.exists(_.nonEmpty)

}
