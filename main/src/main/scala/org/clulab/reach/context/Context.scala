package org.clulab.reach.context

import scala.collection.{MapLike, immutable}


//trait ContextMetaData
//
//object DefaultContext extends ContextMetaData

trait Context {

  var context: Option[ContextMap]
  var contextMetaData: Option[ContextMetaData]

  /** Tell whether context map exists and is non-empty or not. */
  def hasContext (): Boolean = context.exists(_.nonEmpty)

}
