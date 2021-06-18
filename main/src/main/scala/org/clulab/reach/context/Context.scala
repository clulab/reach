package org.clulab.reach.context

import scala.collection.{MapLike, immutable}


//trait ContextMetaData
//
//object DefaultContext extends ContextMetaData

trait Context {

  var contextOpt: Option[ContextMap]
  var contextMetaDataOpt: Option[ContextMetaData]

  /** Tell whether context map exists and is non-empty or not. */
  def hasContext (): Boolean = contextOpt.exists(_.nonEmpty)

}
