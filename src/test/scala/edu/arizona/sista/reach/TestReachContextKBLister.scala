package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.reach.grounding.ReachContextKBLister
import edu.arizona.sista.reach.grounding.ReachContextKBLister._

import TestUtils._

/**
 * Unit tests of the Context KBs lister object.
 */
class TestReachContextKBLister extends FlatSpec with Matchers {

  val ctxList = ReachContextKBLister.listContextKBs

  def hasId (cg:ContextGrounding, id:String): Boolean = cg.id.contains(id)
  def hasKey (cg:ContextGrounding, key:String): Boolean = cg.key.contains(key)
  def hasText (cg:ContextGrounding, text:String): Boolean = cg.text.contains(text)

  "Context KBs list" should "have at least 1000 entries" in {
    (ctxList.size > 1000) should be (true)
  }

  "Context KBs list" should "have cell line entries" in {
    val clines = ctxList.filter(cg => cg.id.contains("CLine"))
    // System.err.println(s"CELL-LINES.size=${clines.size}")
    (clines.size > 0) should be (true)
    (clines.count(cg => hasId(cg, "UA-CLine-100060"))) should be (1)
    // clines.filter(cg => hasText(cg, "Rat1")).foreach(System.err.println(_))
    (clines.count(cg => hasText(cg, "Rat1"))) should be (4)
  }

  "Context KBs list" should "have cell type entries" in {
    val ctypes = ctxList.filter(cg => cg.id.contains("CT"))
    // System.err.println(s"CELL-TYPES.size=${ctypes.size}")
    (ctypes.size > 0) should be (true)
    (ctypes.count(cg => hasText(cg, "granulocytes"))) should be (2)
    (ctypes.count(cg => hasId(cg, "UA-CT-200001"))) should be (1)
    (ctypes.count(cg => hasText(cg, "hair"))) should be (4)
    // ctypes.filter(cg => hasText(cg, "")).foreach(System.err.println(_))
  }

  "Context KBs list" should "have organ entries" in {
    val organs = ctxList.filter(cg => cg.id.contains("ORG"))
    // System.err.println(s"ORGANS.size=${organs.size}")
    (organs.size > 0) should be (true)
    (organs.count(cg => hasText(cg, "Abducens Nerve"))) should be (2)
    (organs.count(cg => hasText(cg, "Male"))) should be (4)
    (organs.count(cg => hasId(cg, "ORG-1092"))) should be (2)
    // organs.filter(cg => hasText(cg, "Abducens Nerve")).foreach(System.err.println(_))
  }

  "Context KBs list" should "have species entries" in {
    val species = ctxList.filter(cg => cg.namespace.contains("taxonomy"))
    // System.err.println(s"SPECIES.size=${species.size}")
    (species.size > 0) should be (true)
    (species.count(cg => hasText(cg, "biovar"))) should be (4)
    (species.count(cg => hasText(cg, "occidentalis"))) should be (6)
    (species.count(cg => hasKey(cg, "baumannii"))) should be (2)
    // species.filter(cg => hasText(cg, "Zymomonas")).foreach(System.err.println(_))
  }

}
