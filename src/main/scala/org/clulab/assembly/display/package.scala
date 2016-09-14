package org.clulab.assembly

import com.typesafe.scalalogging.LazyLogging
import org.clulab.assembly.export.{AssemblyExporter, Row}
import org.clulab.odin._


package object display extends LazyLogging {
  def displayPrecedence(mentions: Seq[Mention]): Unit = {
    val am = Assembler.applySieves(mentions)
    val ae = new AssemblyExporter(am)
    val so = ae.shellOutput((rows: Set[Row]) => rows.filter(_.seen > 0))
    logger.info(so)
  }
}
