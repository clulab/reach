package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.grounding._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Unit tests to ensure alternate resolutions are working for KB grounding.
  *   Written by: Tom Hicks. 12/20/2015.
  *   Last Modified: Correct to use Organ KB for testing.
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
    (imkbOCT.resolve("plasma cell").isDefined) should be (true)
    (imkbOCT.resolve("Plasma cell").isDefined) should be (true)
    (imkbOCT.resolve("Plasma cells").isDefined) should be (true)
    (imkbOCT.resolve("liver cell").isDefined) should be (true)
    (imkbOCT.resolve("Liver Cells").isDefined) should be (true)
    (imkbOCT.resolve("Muscle Cell").isDefined) should be (true)
    (imkbOCT.resolve("muscle cells").isDefined) should be (true)
    (imkbOCT.resolve("muscle tissue").isDefined) should be (true)
    (imkbOCT.resolve("Muscle tissues").isDefined) should be (true)
    (imkbOCT.resolve("MUSCLE TISSUES").isDefined) should be (true)
    (imkbOCT.resolve("Spinal Cord").isDefined) should be (true)
    (imkbOCT.resolve("spinal Cords").isDefined) should be (true)
    (imkbOCT.resolve("Lymphatic").isDefined) should be (true)
    (imkbOCT.resolve("Lymphatics").isDefined) should be (true)
    (imkbOCT.resolve("Nerve").isDefined) should be (true)
    (imkbOCT.resolve("nerves").isDefined) should be (true)
    (imkbOCT.resolve("Lymphatic Cell").isDefined) should be (true)
    (imkbOCT.resolve("lymphatic cells").isDefined) should be (true)
    (imkbOCT.resolve("synovial joint fluid").isDefined) should be (true)
    (imkbOCT.resolve("synovial joint fluids").isDefined) should be (true)
    (imkbOCT.resolve("Spinal Cord Fluid").isDefined) should be (true)
    (imkbOCT.resolve("spinal Cord fluids").isDefined) should be (true)
    (imkbOCT.resolve("Nerve Cell").isDefined) should be (true)
    (imkbOCT.resolve("nerve cells").isDefined) should be (true)
    (imkbOCT.resolve("Breast Tissue").isDefined) should be (true)
    (imkbOCT.resolve("breast tissue").isDefined) should be (true)
  }

  "OctKBL resolve" should "work with alternate lookups" in {
  }

}


// Protein family KB using alternate protein resolutions
class TestOctKBL extends IMKBOrganCellTypeLookup {
  memoryKB = (new TsvIMKBFactory).make(ContextOrganFilename)
}
