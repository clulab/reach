package edu.arizona.sista.assembly

import edu.arizona.sista.assembly.export.{Row, AssemblyExporter}
import edu.arizona.sista.odin._
import edu.arizona.sista.assembly.AssemblyRunner._

package object display {
  def displayPrecedence(mentions: Seq[Mention]): Unit = {
    val am = applySieves(mentions)
    val ae = new AssemblyExporter(am)
    val so = ae.shellOutput((rows: Set[Row]) => rows.filter(_.seen > 0))
    println(so)
  }
}