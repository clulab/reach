package org.clulab.reach.context.context_exec

import com.typesafe.config.ConfigFactory

object CrossValOldDatasetUsingEventsFile extends App {
  val config = ConfigFactory.load()
  val parentDirForRows = config.getString("polarityContext.aggrRowWrittenToFilePerPaper")
}
