package org.clulab.assembly

import org.clulab.assembly.export.{Row, AssemblyExporter}
import org.clulab.odin._
import org.clulab.assembly.AssemblyRunner._

package object display {
  def displayPrecedence(mentions: Seq[Mention]): Unit = {
    val am = applySieves(mentions)
    val ae = new AssemblyExporter(am)
    val so = ae.shellOutput((rows: Set[Row]) => rows.filter(_.seen > 0))
    println(so)
  }
}
