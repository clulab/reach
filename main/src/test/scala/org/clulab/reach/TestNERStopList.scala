package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // do not remove: needed for debugging
import TestUtils._
import io.Source

/**
  * Created by enoriega on 6/8/16.
  * Last Modified: Remove obsolete comments. Uncomment XREF tests.
  */
class TestNERStopList extends FlatSpec with Matchers {

  val entries = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("org/clulab/reach/kb/ner_stoplist.txt")).getLines.filter(_(0) != '#')

  for(entry <- entries){
    entry should "not have a mention" in {
      val mentions = getBioMentions(entry)
      mentions.isEmpty should be (true)
      // printMentions(Try(mentions), true)      // DEBUGGING
    }
  }

  val sent1 = "XREF_FIG and XREF_BIBR are not proteins!"
  sent1 should "not produce any named entities" in {
    val mentions = getBioMentions(sent1)
    mentions.size should be (0)
  }

}
