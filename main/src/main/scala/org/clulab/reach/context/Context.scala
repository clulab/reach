package org.clulab.reach.context

import scala.collection.{MapLike, immutable}


//trait ContextMetaData
//
//object DefaultContext extends ContextMetaData

trait Context {

  var contextOpt: Option[ContextMap] = None
  var contextMetaDataOpt: Option[ContextMetaData] = None

  /** Tell whether context map exists and is non-empty or not. */
  def hasContext (): Boolean = contextOpt.exists(_.nonEmpty)

  def hasContextMetaData(): Boolean = contextMetaDataOpt.exists(_.nonEmpty)

  def setContext(contextMap: ContextMap): Unit =
      contextOpt = if (contextMap.nonEmpty) Some(contextMap) else None

  def setContextMetaData(contextMetaData: ContextMetaData): Unit =
      contextMetaDataOpt = if (contextMetaData.nonEmpty) Some(contextMetaData) else None
}
