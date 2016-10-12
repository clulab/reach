package org.clulab.reach

import org.scalatest.{FlatSpec, Matchers}
import java.io.File


class TestPaperReader extends FlatSpec with Matchers {

  val examplePaper = new File(getClass.getResource("/inputs/plaintext/test.txt").toURI)

  "PaperReader" should "read plain text files" in {
    val mentions = PaperReader.getMentionsFromPaper(examplePaper)
    mentions should not be empty
  }

}
