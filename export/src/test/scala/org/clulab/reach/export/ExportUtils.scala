package org.clulab.reach.export

import org.clulab.odin.Mention
import org.clulab.reach.mentions._


/**
  * Isolates flattening to export subproject
  */
object ExportUtils {

  import org.clulab.reach.TestUtils._

  def getFlattenedBioMentionsFromText(text: String): Seq[BioMention] = for {
    m <- getMentionsFromText(text)
  } yield OutputDegrader.flattenMention(m).toBioMention

  def getMentionsForFriesOutput(text: String): Seq[BioMention] = {
    val mentions = getMentionsFromText(text)
    OutputDegrader.prepareForOutput(mentions)
  }

  def getMentionsForFriesOutput(mns: Seq[Mention]): Seq[BioMention] = OutputDegrader.prepareForOutput(mns)

}
