package org.clulab.reach.nxml

import ai.lum.nxmlreader.NxmlDocument
import ai.lum.nxmlreader.standoff.{ Tree => NxmlStandoff }


case class FriesEntry(
  name: String,
  chunkId: String,
  sectionId: String,
  sectionName: String,
  isTitle: Boolean,
  text: String
) {

  override def toString(): String =  s"$chunkId\t$sectionName\t$sectionId\t${if(isTitle) 1 else 0}\t$text"

  def this(nxmldoc: NxmlDocument) = this(
    name = nxmldoc.pmc,
    // use standoff hashcode as the chunkId
    chunkId = nxmldoc.standoff.hashCode.toString,
    sectionId = nxmldoc.standoff.path,
    sectionName = "",
    false,
    nxmldoc.standoff.text
  )

  def this(paperId: String, standoff: NxmlStandoff) = this(
    name = paperId,
    chunkId = standoff.hashCode.toString,
    sectionId = standoff.path,
    sectionName = "",
    isTitle = false,
    text = standoff.text
  )
}
