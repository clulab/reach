package org.clulab.assembly

import org.clulab.odin.Mention
import org.clulab.reach.PaperReader
import org.clulab.reach.mentions._

/**
  * Utility methods for the tests in this directory
  */
object TestUtils {

  val reader = PaperReader.rs
  val bioprocessor = reader.processor

  def getMentionsFromText(text: String): Seq[Mention] = PaperReader.getMentionsFromText(text)

  def getCorefmentionsFromText(text: String): Seq[CorefMention] = for {
    m <- getMentionsFromText(text)
  } yield m.toCorefMention
}
