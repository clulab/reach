package org.clulab.reach.assembly

import org.clulab.reach.assembly.export.{Row, AssemblyExporter}
import org.clulab.odin._


package object display {
  def displayPrecedence(mentions: Seq[Mention]): Unit = {
    val am = Assembler.applySieves(mentions)
    val ae = new AssemblyExporter(am)
    val so = ae.shellOutput((rows: Set[Row]) => rows.filter(_.seen > 0))
    println(so)
  }
}
