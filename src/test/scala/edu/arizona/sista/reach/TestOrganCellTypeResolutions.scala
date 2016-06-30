package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBConstants._

/**
  * Unit tests to ensure alternate resolutions are working for KB grounding.
  *   Written by: Tom Hicks. 12/20/2015.
  *   Last Modified: Update for use of Uberon as organ KB.
  */
class TestOrganCellTypeResolutions extends FlatSpec with Matchers {

  val imkbOCT = new TestOctKBL              // defined after this class (LOOK BELOW)

  "OctKBL resolve" should "fail because not in the KB" in {
    // keys not in KB:
    (imkbOCT.resolve("NOTINKB").isDefined) should be (false)
    (imkbOCT.resolve("notinkb").isDefined) should be (false)
    (imkbOCT.resolve("notinkb cell").isDefined) should be (false)
    (imkbOCT.resolve("notinkb cells").isDefined) should be (false)
    (imkbOCT.resolve("notinkb tissue").isDefined) should be (false)
    (imkbOCT.resolve("notinkb tissues").isDefined) should be (false)
    (imkbOCT.resolve("notinkb fluid").isDefined) should be (false)
    (imkbOCT.resolve("notinkb fluids").isDefined) should be (false)
  }

  "OctKBL resolve" should "work" in {
    (imkbOCT.resolve("blood plasm").isDefined) should be (true)
    (imkbOCT.resolve("Brevis Fossa").isDefined) should be (true)
    (imkbOCT.resolve("liver parenchyma").isDefined) should be (true)
    (imkbOCT.resolve("liver lobe").isDefined) should be (true)
    (imkbOCT.resolve("liver cell plate").isDefined) should be (true)
    (imkbOCT.resolve("mesometrium").isDefined) should be (true)
  }

  "OctKBL resolve" should "work via alternate lookups" in {
    (imkbOCT.resolve("blood plasm cell").isDefined) should be (true)
    (imkbOCT.resolve("blood plasm cells").isDefined) should be (true)
    (imkbOCT.resolve("blood plasm tissue").isDefined) should be (true)
    (imkbOCT.resolve("blood plasm tissues").isDefined) should be (true)
    (imkbOCT.resolve("Brevis Fossa cell").isDefined) should be (true)
    (imkbOCT.resolve("Liver parenchyma cell").isDefined) should be (true)
    (imkbOCT.resolve("Liver parenchyma cells").isDefined) should be (true)
    (imkbOCT.resolve("Liver Parenchyma tissue").isDefined) should be (true)
    (imkbOCT.resolve("Liver Parenchyma tissues").isDefined) should be (true)
    (imkbOCT.resolve("liver lobe cell").isDefined) should be (true)
    (imkbOCT.resolve("liver lobe cells").isDefined) should be (true)
    (imkbOCT.resolve("liver lobe fluid").isDefined) should be (true)
    (imkbOCT.resolve("liver lobe fluids").isDefined) should be (true)
    (imkbOCT.resolve("liver lobe tissue").isDefined) should be (true)
    (imkbOCT.resolve("liver lobe tissues").isDefined) should be (true)
    (imkbOCT.resolve("liver cell plate cell").isDefined) should be (true)
    (imkbOCT.resolve("liver cell plate cells").isDefined) should be (true)
    (imkbOCT.resolve("liver cell plate tissue").isDefined) should be (true)
    (imkbOCT.resolve("liver cell plate tissues").isDefined) should be (true)
    (imkbOCT.resolve("mesometrium cell").isDefined) should be (true)
    (imkbOCT.resolve("mesometrium cells").isDefined) should be (true)
    (imkbOCT.resolve("mesometrium tissue").isDefined) should be (true)
    (imkbOCT.resolve("mesometrium tissues").isDefined) should be (true)
    (imkbOCT.resolve("mesometrium fluid").isDefined) should be (true)
    (imkbOCT.resolve("mesometrium fluids").isDefined) should be (true)
  }

}


// Protein family KB using alternate protein resolutions
class TestOctKBL extends IMKBOrganCellTypeLookup {
  memoryKB = (new TsvIMKBFactory).make(ContextOrganFilename)
}
