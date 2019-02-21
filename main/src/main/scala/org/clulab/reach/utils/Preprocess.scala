package org.clulab.reach.utils

import org.clulab.utils.ScienceUtils

class Preprocess {
  val scienceUtils = new ScienceUtils

  def preprocessText(text:String):String = {
    val textWithoutUnicode = scienceUtils.replaceUnicodeWithAscii(text)
    val textWithoutBibRefs = scienceUtils.removeBibRefs(textWithoutUnicode)
    val textWithoutFigRefs = scienceUtils.removeFigureAndTableReferences(textWithoutBibRefs)
    textWithoutFigRefs
  }
}
