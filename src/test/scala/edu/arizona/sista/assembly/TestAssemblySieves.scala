package edu.arizona.sista.assembly

import edu.arizona.sista.assembly.AssemblyRunner._
import edu.arizona.sista.reach.TestUtils._
import org.scalatest.{Matchers, FlatSpec}


/**
  * Created by dane on 6/9/16.
  */
class TestAssemblySieves extends FlatSpec with Matchers {
  val intraSent1 = "Together these data demonstrate that E2-induced SRC-3 phosphorylation is dependent on a direct " +
    "interaction between SRC-3 and ERα and can occur outside of the nucleus."

  intraSent1 should "be annotated with the binding preceding the phosphorylation" in {
    val doc = createDoc(intraSent1, "intra1-test")
    val mentions = testReach.extractFrom(doc)
    val am = applySieves(mentions)

    val bRep = am.distinctSimpleEvents("Binding").head
    val pRep = am.distinctRegulations(AssemblyManager.positive).head

    am.distinctPredecessorsOf(pRep).size should be(1)
    val pr = am.getPrecedenceRelations(pRep).head
    pr.before == bRep.equivalenceHash should be (true)
    pr.after == pRep.equivalenceHash should be (true)

  }

  // Sieve tests

  val tamSent1 = "Once BEF had been phosphorylated, AFT was ubiquitinated"

  tamSent1 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val doc = createDoc(tamSent1, "tam1-test")
    val mentions = testReach.extractFrom(doc)
    val am = applySieves(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelations(uRep).head
    pr.before == pRep.equivalenceHash should be (true)
    pr.after == uRep.equivalenceHash should be (true)
  }

  val tamSent2 = "AFT will be ubiquitinated only if BEF is first phosphorylated"

  tamSent2 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val doc = createDoc(tamSent2, "tam2-test")
    val mentions = testReach.extractFrom(doc)
    val am = applySieves(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelations(uRep).head
    pr.before == pRep.equivalenceHash should be (true)
    pr.after == uRep.equivalenceHash should be (true)
  }

  val tamSent3 = "AFT was ubiquitinated when BEF had been phosphorylated"

  tamSent3 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val doc = createDoc(tamSent3, "tam3-test")
    val mentions = testReach.extractFrom(doc)
    val am = applySieves(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelations(uRep).head
    pr.before == pRep.equivalenceHash should be (true)
    pr.after == uRep.equivalenceHash should be (true)
  }

  val interSent1 = "BEF was phosphorylated. Then, AFT was ubiquitinated."

  interSent1 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val doc = createDoc(interSent1, "inter1-test")
    val mentions = testReach.extractFrom(doc)
    val am = applySieves(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelations(uRep).head
    pr.before == pRep.equivalenceHash should be (true)
    pr.after == uRep.equivalenceHash should be (true)
  }

  val interSent2 = "BEF was phosphorylated. Subsequently AFT was ubiquitinated."

  interSent2 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val doc = createDoc(interSent2, "inter2-test")
    val mentions = testReach.extractFrom(doc)
    val am = applySieves(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelations(uRep).head
    pr.before == pRep.equivalenceHash should be (true)
    pr.after == uRep.equivalenceHash should be (true)
  }

  val interSent3 = "AFT was ubiquitinated. Prior to this, BEF was phosphorylated."

  interSent3 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val doc = createDoc(interSent3, "inter3-test")
    val mentions = testReach.extractFrom(doc)
    val am = applySieves(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelations(uRep).head
    pr.before == pRep.equivalenceHash should be (true)
    pr.after == uRep.equivalenceHash should be (true)
  }

  val interSent4 = "AFT was ubiquitinated. Previously, BEF was phosphorylated."

  interSent4 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val doc = createDoc(interSent4, "inter4-test")
    val mentions = testReach.extractFrom(doc)
    val am = applySieves(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelations(uRep).head
    pr.before == pRep.equivalenceHash should be (true)
    pr.after == uRep.equivalenceHash should be (true)
  }


  // Play it safe by ignoring cues not at the beginning of the sentence.
  val interSent5 = "AFT was ubiquitinated. There is intervening material and, previously, BEF was phosphorylated."

  interSent5 should "have no precedence relations" in {
    val doc = createDoc(interSent5, "inter5-test")
    val mentions = testReach.extractFrom(doc)
    val am = applySieves(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(0)
  }

}
