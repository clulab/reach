package org.clulab.reach

import org.scalatest.{FlatSpec, Matchers}
import TestUtils._
import com.typesafe.config.ConfigFactory
import org.clulab.reach.grounding._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Unit tests to ensure alternate resolutions are working for KB grounding.
  *   Written by: Tom Hicks. 12/20/2015.
  *   Last Modified: Update for changed arguments of KB key transforms group.
  */
class TestOrganResolutions extends FlatSpec with Matchers {

  val imkbOCT = new TestOctKBL              // defined after this class (LOOK BELOW)
  // imkbOCT.memoryKB.dump                     // DEBUGGING

  "OctKBL resolve" should "fail because not in the KB" in {
    // keys not in KB:
    (imkbOCT.resolve("NOTINKB")) should be (empty)
    (imkbOCT.resolve("notinkb")) should be (empty)
    (imkbOCT.resolve("notinkb cell")) should be (empty)
    (imkbOCT.resolve("notinkb cells")) should be (empty)
    (imkbOCT.resolve("notinkb tissue")) should be (empty)
    (imkbOCT.resolve("notinkb tissues")) should be (empty)
    (imkbOCT.resolve("notinkb fluid")) should be (empty)
    (imkbOCT.resolve("notinkb fluids")) should be (empty)
  }

  "OctKBL resolve" should "work" in {
    (imkbOCT.resolve("blood plasm")) should be (defined)
    (imkbOCT.resolve("Brevis Fossa")) should be (defined)
    (imkbOCT.resolve("liver parenchyma")) should be (defined)
    (imkbOCT.resolve("liver lobe")) should be (defined)
    (imkbOCT.resolve("liver cell plate")) should be (defined)
    (imkbOCT.resolve("mesometrium")) should be (defined)
  }

  "OctKBL resolve" should "work via alternate lookups" in {
    (imkbOCT.resolve("blood plasm cell")) should be (defined)
    (imkbOCT.resolve("blood plasm cells")) should be (defined)
    (imkbOCT.resolve("blood plasm tissue")) should be (defined)
    (imkbOCT.resolve("blood plasm tissues")) should be (defined)
    (imkbOCT.resolve("Brevis Fossa cell")) should be (defined)
    (imkbOCT.resolve("Liver parenchyma cell")) should be (defined)
    (imkbOCT.resolve("Liver parenchyma cells")) should be (defined)
    (imkbOCT.resolve("Liver Parenchyma tissue")) should be (defined)
    (imkbOCT.resolve("Liver Parenchyma tissues")) should be (defined)
    (imkbOCT.resolve("liver lobe cell")) should be (defined)
    (imkbOCT.resolve("liver lobe cells")) should be (defined)
    (imkbOCT.resolve("liver lobe fluid")) should be (defined)
    (imkbOCT.resolve("liver lobe fluids")) should be (defined)
    (imkbOCT.resolve("liver lobe tissue")) should be (defined)
    (imkbOCT.resolve("liver lobe tissues")) should be (defined)
    (imkbOCT.resolve("liver cell plate cell")) should be (defined)
    (imkbOCT.resolve("liver cell plate cells")) should be (defined)
    (imkbOCT.resolve("liver cell plate tissue")) should be (defined)
    (imkbOCT.resolve("liver cell plate tissues")) should be (defined)
    (imkbOCT.resolve("mesometrium cell")) should be (defined)
    (imkbOCT.resolve("mesometrium cells")) should be (defined)
    (imkbOCT.resolve("mesometrium tissue")) should be (defined)
    (imkbOCT.resolve("mesometrium tissues")) should be (defined)
    (imkbOCT.resolve("mesometrium fluid")) should be (defined)
    (imkbOCT.resolve("mesometrium fluids")) should be (defined)
  }
}

// Protein family KB using alternate protein resolutions
class TestOctKBL extends IMKBLookup {
  private val conf = ConfigFactory.load()
  private val path = conf.getString("KnowledgeBases.ContextOrgan.path")

  val meta = new IMKBMetaInfo(kbFilename = Some(path))
  val keyTransforms = KBKeyTransformsGroup(DefaultKeyTransforms, OrganAuxKeyTransforms, DefaultKeyTransforms)
  memoryKB = (new TsvIMKBFactory).make(meta, keyTransforms)
}
