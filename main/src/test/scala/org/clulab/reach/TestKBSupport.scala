package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests to ensure grounding is working properly
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Update for hiding of KB entry class.
  */
class TestKBSupport extends FlatSpec with Matchers {

  // test Speciated
  "isHumanSpecies(human)" should "work" in {
    (Speciated.isHumanSpecies("human")) should be (true)
    (Speciated.isHumanSpecies("Human")) should be (true)
    (Speciated.isHumanSpecies("HUMAN")) should be (true)
    (Speciated.isHumanSpecies("Homo Sapiens")) should be (true)
    (Speciated.isHumanSpecies("HOMO SAPIENS")) should be (true)
  }

  "isHumanSpecies()" should "fail" in {
    (Speciated.isHumanSpecies("")) should be (false)
    (Speciated.isHumanSpecies("rana pipiens")) should be (false)
    (Speciated.isHumanSpecies("homo erectus")) should be (false)
    (Speciated.isHumanSpecies("Human being")) should be (false)
  }

  "containsHumanSpecies" should "should work" in {
    (Speciated.containsHumanSpecies(Seq("human"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("homo sapiens"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("human","homo sapiens"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "human", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("human", "bat", "ant", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "ant", "human"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "homo sapiens", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("homo sapiens", "bat", "ant", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "ant", "homo sapiens"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("Human"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("Homo Sapiens"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("HUMAN","HOMO SAPIENS"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "Human", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("HUMAN", "bat", "ant", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "ant", "Human"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "HOMO SAPIENS", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("Homo Sapiens", "bat", "ant", "cat"))) should be (true)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "ant", "Homo sapiens"))) should be (true)
  }

  "containsHumanSpecies" should "should fail" in {
    (Speciated.containsHumanSpecies(Seq[String]())) should be (false)
    (Speciated.containsHumanSpecies(Seq("BAT"))) should be (false)
    (Speciated.containsHumanSpecies(Seq("bat"))) should be (false)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "ant"))) should be (false)
    (Speciated.containsHumanSpecies(Seq("ant", "bat", "homo erectus"))) should be (false)
    (Speciated.containsHumanSpecies(Seq("human being"))) should be (false)
    (Speciated.containsHumanSpecies(Seq("Human Being"))) should be (false)
  }


  // test KBUtils
  "makePathInKBDir(testFile)" should "return complete filepath in KB directory" in {
    val expected = ReachKBConstants.KBDirResourcePath + java.io.File.separator + "testFile"
    val path = ReachKBUtils.makePathInKBDir("testFile")
  }

}
