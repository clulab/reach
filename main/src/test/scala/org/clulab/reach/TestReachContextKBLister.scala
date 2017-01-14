package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import org.clulab.reach.grounding.ReachContextKBLister
import org.clulab.reach.grounding.ReachContextKBLister._


/**
  * Unit tests of the Context KBs lister object.
  *   Last Modified: Update for removal of key field from KB resolution.
  */
class TestReachContextKBLister extends FlatSpec with Matchers {

  val ctxList = ReachContextKBLister.listContextKBs

  def hasId (cg:ContextGrounding, id:String): Boolean = cg.id.contains(id)
  def hasText (cg:ContextGrounding, text:String): Boolean = cg.text.contains(text)

  "Context KBs list" should "have at least 1000 entries" in {
    (ctxList.size > 1000) should be (true)
  }

  "Context KBs list" should "have cell line entries" in {
    val clines = ctxList.filter(cg => cg.id.contains("CVCL"))
    // System.err.println(s"CELL-LINES.size=${clines.size}")
    clines should not be empty
    (clines.count(cg => hasId(cg, "CVCL_E548"))) should be (3)
    // clines.filter(cg => hasText(cg, "Rat1")).foreach(System.err.println(_))
    (clines.count(cg => hasText(cg, "RATV-NRK"))) should be (1)
  }

  "Context KBs list" should "have cell type entries" in {
    val ctypes = ctxList.filter(cg => cg.id.contains("CL:"))
    // System.err.println(s"CELL-TYPES.size=${ctypes.size}"
    ctypes should not be empty
    (ctypes.count(cg => hasText(cg, "granulocyte"))) should be (22)
    (ctypes.count(cg => hasId(cg, "CL:0000557"))) should be (14)
    (ctypes.count(cg => hasText(cg, "hair"))) should be (50)
    // ctypes.filter(cg => hasText(cg, "")).foreach(System.err.println(_))
  }

  "Context KBs list" should "have organ entries" in {
    val organs = ctxList.filter(cg => cg.id.contains("UBERON:"))
    // System.err.println(s"ORGANS.size=${organs.size}")
    organs should not be empty
    (organs.count(cg => hasText(cg, "abducens nerve"))) should be (12)
    (organs.count(cg => hasText(cg, "zygomaticus"))) should be (12)
    (organs.count(cg => hasId(cg, "UBERON:0008960"))) should be (1)
    // organs.filter(cg => hasText(cg, "Abducens Nerve")).foreach(System.err.println(_))
  }

  "Context KBs list" should "have species entries" in {
    val species = ctxList.filter(cg => cg.namespace.contains("taxonomy"))
    // System.err.println(s"SPECIES.size=${species.size}")
    species should not be empty
    (species.count(cg => hasText(cg, "biovar"))) should be (4)
    (species.count(cg => hasText(cg, "occidentalis"))) should be (6)
    // species.filter(cg => hasText(cg, "Zymomonas")).foreach(System.err.println(_))
  }

}
