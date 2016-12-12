package org.clulab.reach.export.cmu

import org.clulab.odin.Mention
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.export.{AssemblyExporter, Row, ExportFilters}
import org.clulab.reach.assembly.sieves.{AssemblySieve, DeduplicationSieves}

/**
  * Yet another tabular format
  * This follows the specification from CMU, to integrate nicely with the DyCE model
  * Created by mihais on 11/23/16.
  */
object CMUExporter {

  def cmuFilter(rows: Set[Row]): Set[Row] = rows.filter { r =>
    // remove unseen
    (r.seen > 0) &&
    // keep only the events
    ExportFilters.isEvent(r) &&
    // CMU only cares about events that have a controller! (or Translocations)
    ExportFilters.hasController(r)
  }

  def tabularOutput(mentions: Seq[Mention]): String = {
    val ae = createExporter(mentions)
    ae.rowsToString(AssemblyExporter.CMU_COLUMNS, AssemblyExporter.SEP, cmuFilter)
  }

  def createExporter(mentions: Seq[Mention]): AssemblyExporter = {
    // perform deduplication
    val dedup = new DeduplicationSieves()
    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions)
    val am: AssemblyManager = orderedSieves.apply(mentions)
    val ae = new AssemblyExporter(am)

    ae
  }
}
