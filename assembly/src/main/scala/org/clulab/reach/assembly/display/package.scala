package org.clulab.reach.assembly

import org.clulab.reach.assembly.export.{AssemblyExporter, AssemblyRow, ExportFilters}
import org.clulab.odin._


package object display {

  def displayPrecedence(mentions: Seq[Mention]): Unit = {
    val am = Assembler.applySieves(mentions)
    val ae = new AssemblyExporter(am)
    val rows = ae.getRows
    val so = shellOutput(rows)
    println(so)
  }

  def shellOutput(rows: Seq[AssemblyRow]): String = {
    val rowsForOutput = rows.filter { row =>
      ExportFilters.seenAtLeastOnce(row) && ExportFilters.isEvent(row)
    }.distinct

    // validate output
    AssemblyExporter.validateOutput(rowsForOutput)

    val text =
    // only events
      rowsForOutput
        .sortBy(r => r.eerID)
        .map(_.toShellRow)
        .mkString
    text + "=" * 50
  }

  implicit class RowOps(row: AssemblyRow) {
    def toShellRow: String = {
      val precedingEvents = row.precededBy.toSeq.sorted.mkString(", ")
      s"""${row.eerID}:\t${if(row.negated) "! " else ""}${row.input} """ +
        s"""==${if (row.controller.nonEmpty) "[" + row.controller + "]" else ""}==> """ +
        s"""${row.output}""" +
        s"""${if (precedingEvents.nonEmpty) s"\n\t\tPreceding => $precedingEvents" else ""}\n\n"""
    }
  }
}
