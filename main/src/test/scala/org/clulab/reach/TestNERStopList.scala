package org.clulab.reach

import scala.io.Source
import scala.util.Try                       // do not remove: needed for debugging

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

/**
  * Created by enoriega on 6/8/16.
  */
class TestNERStopList extends FlatSpec with Matchers {

  // TODO: Read the contents of the stoplist from the bioresources jar
  val entries = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("org/clulab/reach/kb/ner_stoplist.txt")).getLines.filter(_(0) != '#')

  for(entry <- entries){
    entry should "not have a mention" in {
      val mentions = getBioMentions(entry)
      mentions.isEmpty should be (true)
      // printMentions(Try(mentions), true)      // DEBUGGING
    }
  }

  // TODO: uncomment after switching to processors 5.9.5
  /*
  val sent1 = "XREF_FIG and XREF_BIBR are not proteins!"
  sent1 should "not produce any named entities" in {
    val mentions = getBioMentions(sent1)
    mentions.size should be (0)
  }
  */

}
