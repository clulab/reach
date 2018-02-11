package org.clulab.reach

import org.clulab.reach.utils.DependencyUtils._
import org.clulab.struct.Interval
import org.scalatest._

/**
  * Tests dependency utilities such as findHeadStrict
  * Date: 1/25/16
  * Last Modified: Update for processing annotators.
  */

class TestFindHeads extends FlatSpec with Matchers {

  val text2 = "The docking protein Gab1 is the primary mediator of EGF-stimulated activation of the PI-3K/Akt cell survival pathway"
  val annotator = TestUtils.procAnnotator
  val doc2 = annotator.annotate(text2, keepText = true)
  val sent2 = doc2.sentences.head
  
  text2 should "find the heads despite loops" in {
    val heads = findHeadsStrict(Interval(0, 20), sent2)
    
    // The exact values should only be used for the BioCluProcessor
    //heads should be (Seq(7, 10, 11))
  }
}
