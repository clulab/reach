package org.clulab.reach

import org.clulab.odin._
import org.clulab.reach.mentions._
import org.clulab.utils.Serializer

import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try
import TestUtils._
import io.Source

/**
  * Created by enoriega on 6/8/16.
  */
class TestNERStopList extends FlatSpec with Matchers {

  // TODO: Read the contents of the stoplist from the bioresources jar
  //val entries:Seq[String] = Seq("cell", "cells")
  val entries = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("org/clulab/reach/kb/ner_stoplist.txt")).getLines.filter(_(0) != '#')

  for(entry <- entries){
    entry should "not have a mention" in {
      val mentions = getBioMentions(entry)
      mentions.isEmpty should be (true)
      // printMentions(Try(mentions), true)      // DEBUGGING
    }
  }

}
