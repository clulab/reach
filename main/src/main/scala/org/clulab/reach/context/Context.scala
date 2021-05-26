package org.clulab.reach.context

import scala.collection.{MapLike, immutable}


//trait ContextMetaData
//
//object DefaultContext extends ContextMetaData

trait Context {

  var context: Option[ContextMap] = None
  var contextMetaData: Option[ContextMetaData] = None

  /** Tell whether context map exists and is non-empty or not. */
  def hasContext (): Boolean = context.exists(_.nonEmpty)

}
