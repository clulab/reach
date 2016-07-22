package org.clulab.assembly

import org.clulab.assembly.AssemblyRunner._
import org.clulab.reach.TestUtils._
import org.scalatest.{Matchers, FlatSpec}


/**
  * Created by dane on 6/9/16.
  */
class TestAssemblySieves extends FlatSpec with Matchers {
  val intraSent1 = "Together these data demonstrate that E2-induced SRC-3 phosphorylation is dependent on a direct " +
    "interaction between SRC-3 and ERα and can occur outside of the nucleus."

  intraSent1 should "be annotated with the binding preceding the phosphorylation" in {
    val mentions = getMentionsFromText(intraSent1)
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
    val mentions = getMentionsFromText(tamSent1)
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
    val mentions = getMentionsFromText(tamSent2)
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
    val mentions = getMentionsFromText(tamSent3)
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
    val mentions = getMentionsFromText(interSent1)
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
    val mentions = getMentionsFromText(interSent2)
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
    val mentions = getMentionsFromText(interSent3)
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
    val mentions = getMentionsFromText(interSent4)
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
    val mentions = getMentionsFromText(interSent5)
    val am = applySieves(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(0)
  }

}
