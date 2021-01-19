package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import org.clulab.reach.grounding.ReachContextKBLister
import org.clulab.reach.grounding.ReachContextKBLister._


/**
  * Unit tests of the Context KBs lister object.
  *   Last Modified: Update test to agree with data.
  */
class TestReachContextKBLister extends FlatSpec with Matchers {

  val ctxList = ReachContextKBLister.listContextKBs
  // ctxList.foreach(System.err.println(_))    // DEBUGGING

  def hasId (cg:ContextGrounding, id:String): Boolean = cg.id.contains(id)
  def hasText (cg:ContextGrounding, text:String): Boolean = cg.text.contains(text)

  "Context KBs list" should "have at least 1000 entries" in {
    (ctxList.size > 1000) should be (true)
  }

  "Context KBs list" should "have cell line entries" in {
    val clines = ctxList.filter(cg => cg.id.contains("CVCL"))
    // System.err.println(s"CELL-LINES.size=${clines.size}") // DEBUGGING
    clines should not be empty
    // clines.filter(cg => hasText(cg, "Rat1")).foreach(System.err.println(_)) // DEBUGGING
    (clines.count(cg => hasId(cg, "CVCL_E548"))) should be (3)
    (clines.count(cg => hasText(cg, "RATV-NRK"))) should be (1)
  }

  "Context KBs list" should "have cell type entries" in {
    val ctypes = ctxList.filter(cg => cg.id.contains("CL:"))
    // System.err.println(s"CELL-TYPES.size=${ctypes.size}" // DEBUGGING
    ctypes should not be empty
    // ctypes.filter(cg => hasText(cg, "")).foreach(System.err.println(_)) // DEBUGGING
    (ctypes.count(cg => hasText(cg, "granulocyte"))) should be (26)
    (ctypes.count(cg => hasId(cg, "CL:0000557"))) should be (14)
    (ctypes.count(cg => hasText(cg, "hair"))) should be (54)
  }

  "Context KBs list" should "have organ entries" in {
    val organs = ctxList.filter(cg => cg.id.contains("UBERON:"))
    // System.err.println(s"ORGANS.size=${organs.size}") // DEBUGGING
    organs should not be empty
    // organs.filter(cg => hasText(cg, "Abducens Nerve")).foreach(System.err.println(_)) // DEBUGGING
    (organs.count(cg => hasText(cg, "abducens nerve"))) should be (12)
    (organs.count(cg => hasText(cg, "zygomaticus"))) should be (12)
    (organs.count(cg => hasId(cg, "UBERON:0008960"))) should be (1)
  }

  "Context KBs list" should "have species entries" in {
    val species = ctxList.filter(cg => cg.namespace.contains("taxonomy"))
    // System.err.println(s"SPECIES.size=${species.size}") // DEBUGGING
    species should not be empty
    // species.filter(cg => hasText(cg, "occidentalis")).foreach(System.err.println(_)) // DEBUGGING
    (species.count(cg => hasText(cg, "biovar"))) should be (6)
    (species.count(cg => hasText(cg, "occidentalis"))) should be (9)
    (species.count(cg => hasId(cg, "210"))) should be (15)
  }

}
