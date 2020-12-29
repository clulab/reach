package org.clulab.reach.export.arizona

import org.clulab.odin.Mention
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.export.{ AssemblyExporter, ExportFilters, AssemblyRow }
import org.clulab.reach.assembly.sieves.{ AssemblySieve, DeduplicationSieves }


/**
  * Outputter for Arizona's tabular format
  */
object ArizonaOutputter {

  val columns = Seq(
    AssemblyExporter.INPUT,
    AssemblyExporter.OUTPUT,
    AssemblyExporter.CONTROLLER,
    AssemblyExporter.EVENT_ID,
    AssemblyExporter.EVENT_LABEL,
    AssemblyExporter.NEGATED,
    AssemblyExporter.INDIRECT,
    // context
    AssemblyExporter.CONTEXT_SPECIES,
    AssemblyExporter.CONTEXT_ORGAN,
    AssemblyExporter.CONTEXT_CELL_LINE,
    AssemblyExporter.CONTEXT_CELL_TYPE,
    AssemblyExporter.CONTEXT_CELLULAR_COMPONENT,
    AssemblyExporter.CONTEXT_TISSUE_TYPE,
    // translocations
    AssemblyExporter.TRANSLOCATION_SOURCE,
    AssemblyExporter.TRANSLOCATION_DESTINATION,
    // triggers
    AssemblyExporter.TRIGGERS,
    // evidence
    AssemblyExporter.SEEN,
    AssemblyExporter.EVIDENCE,
    AssemblyExporter.SEEN_IN
  )

  def arizonaFilter(rows: Seq[AssemblyRow]): Seq[AssemblyRow] = rows.filter { r =>
    // remove unseen
    (r.seen > 0) &&
      // keep only the events
      ExportFilters.isEvent(r)
  }.distinct

  def tabularOutput(mentions: Seq[Mention]): String = {
    val ae = createExporter(mentions)
    ae.rowsToString(columns, AssemblyExporter.SEP, arizonaFilter)
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
