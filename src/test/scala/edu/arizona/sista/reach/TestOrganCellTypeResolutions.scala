package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Unit tests to ensure alternate resolutions are working for KB grounding.
  *   Written by: Tom Hicks. 12/20/2015.
  *   Last Modified: Update for IMKB ctor changes.
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

  "OctKBL resolve" should "work without using alternate lookups" in {
    (imkbOCT.resolve("plasma cell").isDefined) should be (true)
    (imkbOCT.resolve("Plasma cell").isDefined) should be (true)
    (imkbOCT.resolve("Plasma cells").isDefined) should be (true)
    (imkbOCT.resolve("band cell").isDefined) should be (true)
    (imkbOCT.resolve("Band Cells").isDefined) should be (true)
    (imkbOCT.resolve("muscle tissue").isDefined) should be (true)
    (imkbOCT.resolve("Muscle tissues").isDefined) should be (true)
    (imkbOCT.resolve("MUSCLE TISSUES").isDefined) should be (true)
    (imkbOCT.resolve("Pericardial fluid").isDefined) should be (true)
    (imkbOCT.resolve("pericardial fluids").isDefined) should be (true)
    (imkbOCT.resolve("Cerebral Spinal Fluid").isDefined) should be (true)
    (imkbOCT.resolve("cerebral spinal fluids").isDefined) should be (true)
  }

  "OctKBL resolve" should "work with alternate lookups" in {
    (imkbOCT.resolve("HAIR").isDefined) should be (true)
    (imkbOCT.resolve("hair").isDefined) should be (true)
    (imkbOCT.resolve("hair cell").isDefined) should be (true)
    (imkbOCT.resolve("hair cells").isDefined) should be (true)
    (imkbOCT.resolve("hair tissue").isDefined) should be (true)
    (imkbOCT.resolve("hair tissues").isDefined) should be (true)
    (imkbOCT.resolve("hair fluid").isDefined) should be (true)
    (imkbOCT.resolve("hair fluids").isDefined) should be (true)
    (imkbOCT.resolve("thrombocyte cell").isDefined) should be (true)
    (imkbOCT.resolve("macrophage cell").isDefined) should be (true)
    (imkbOCT.resolve("muscle tissue cells").isDefined) should be (true)
  }

}


// Protein family KB using alternate protein resolutions
class TestOctKBL extends IMKBOrganCellTypeLookup {
  val memoryKB = new InMemoryKB(ContextCellTypeFilename)
}
