package org.clulab.reach.assembly

import org.scalatest.{ FlatSpec, Matchers }
import org.clulab.reach.assembly.TestUtils.{ jsonStringToDocument, getMentionsFromText }
import org.clulab.reach.assembly.sieves.{ AssemblySieve, DeduplicationSieves, PrecedenceSieves }


/**
  * Assembly sieve tests
  */
class TestAssemblySieves extends FlatSpec with Matchers {

  // Reichenbach rules

  val tamSent1 = "Once BEF had been phosphorylated, AFT was ubiquitinated"

  tamSent1 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val mentions = getMentionsFromText(tamSent1)

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
        // find precedence using TAM rules
        AssemblySieve(precedence.reichenbachPrecedence)

    val am: AssemblyManager = orderedSieves.apply(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelationsFor(uRep).head
    pr.before.equivalenceHash(ignoreMods = false) == pRep.equivalenceHash(ignoreMods = false) should be (true)
    pr.before.equivalenceHash(ignoreMods = true) == pRep.equivalenceHash(ignoreMods = true) should be (true)
    pr.after.equivalenceHash(ignoreMods = false) == uRep.equivalenceHash(ignoreMods = false) should be (true)
    pr.after.equivalenceHash(ignoreMods = true) == uRep.equivalenceHash(ignoreMods = true) should be (true)
  }

  val tamSent2 = "AFT will be ubiquitinated only if BEF is first phosphorylated"

  tamSent2 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val mentions = getMentionsFromText(tamSent2)

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
        // find precedence using TAM rules
        AssemblySieve(precedence.reichenbachPrecedence)

    val am: AssemblyManager = orderedSieves.apply(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelationsFor(uRep).head
    pr.before.isEquivalentTo(pRep, ignoreMods = false) should be (true)
    pr.before.isEquivalentTo(pRep, ignoreMods = true) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = false) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = true) should be (true)
  }

  val tamSent3 = "AFT was ubiquitinated when BEF had been phosphorylated"

  tamSent3 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val mentions = getMentionsFromText(tamSent3)

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
        // find precedence using TAM rules
        AssemblySieve(precedence.reichenbachPrecedence)

    val am: AssemblyManager = orderedSieves.apply(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelationsFor(uRep).head
    pr.before.isEquivalentTo(pRep, ignoreMods = false) should be (true)
    pr.before.isEquivalentTo(pRep, ignoreMods = true) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = false) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = true) should be (true)
  }


  // Intrasential Odin rules

  val intraSent1 = "Together these data demonstrate that E2-induced SRC-3 phosphorylation is dependent on a direct " +
    "interaction between SRC-3 and ERα and can occur outside of the nucleus."

  intraSent1 should "be annotated with the binding preceding the phosphorylation" in {
    val mentions = getMentionsFromText(intraSent1)

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
        // find precedence using intrasentential Odin rules
        AssemblySieve(precedence.intrasententialRBPrecedence)

    val am: AssemblyManager = orderedSieves.apply(mentions)

    val bRep = am.distinctSimpleEvents("Binding").head
    val pRep = am.distinctRegulations(AssemblyManager.positive).head

    am.distinctPredecessorsOf(pRep).size should be(1)
    val pr = am.getPrecedenceRelationsFor(pRep).head
    pr.before.equivalenceHash(ignoreMods = false) == bRep.equivalenceHash(ignoreMods = false) should be (true)
    pr.before.equivalenceHash(ignoreMods = true) == bRep.equivalenceHash(ignoreMods = true) should be (true)
    pr.after.equivalenceHash(ignoreMods = false) == pRep.equivalenceHash(ignoreMods = false) should be (true)
    pr.after.equivalenceHash(ignoreMods = true) == pRep.equivalenceHash(ignoreMods = true) should be (true)

  }

  // Intersentential rules

  val interSent1 = "BEF was phosphorylated. Then, AFT was ubiquitinated."

  interSent1 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val mentions = getMentionsFromText(interSent1)

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
        // find precedence using intersentential Odin rules
        AssemblySieve(precedence.intersententialRBPrecedence)

    val am: AssemblyManager = orderedSieves.apply(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelationsFor(uRep).head
    pr.before.isEquivalentTo(pRep, ignoreMods = false) should be (true)
    pr.before.isEquivalentTo(pRep, ignoreMods = true) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = true) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = false) should be (true)
  }

  val interSent2 = "BEF was phosphorylated. Subsequently AFT was ubiquitinated."

  interSent2 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val mentions = getMentionsFromText(interSent2)

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
        // find precedence using intersentential Odin rules
        AssemblySieve(precedence.intersententialRBPrecedence)

    val am: AssemblyManager = orderedSieves.apply(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelationsFor(uRep).head
    pr.before.isEquivalentTo(pRep, ignoreMods = false) should be (true)
    pr.before.isEquivalentTo(pRep, ignoreMods = true) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = false) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = true) should be (true)
  }

  val interSent3 = "AFT was ubiquitinated. Prior to this, BEF was phosphorylated."

  interSent3 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val mentions = getMentionsFromText(interSent3)

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
        // find precedence using intersentential Odin rules
        AssemblySieve(precedence.intersententialRBPrecedence)

    val am: AssemblyManager = orderedSieves.apply(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelationsFor(uRep).head
    pr.before.isEquivalentTo(pRep, ignoreMods = false) should be (true)
    pr.before.isEquivalentTo(pRep, ignoreMods = true) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = false) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = true) should be (true)
  }

  val interSent4 = "AFT was ubiquitinated. Previously, BEF was phosphorylated."

  interSent4 should "be annotated with the phosphorylation preceding the ubiquitination" in {
    val mentions = getMentionsFromText(interSent4)

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
        // find precedence using intersentential Odin rules
        AssemblySieve(precedence.intersententialRBPrecedence)

    val am: AssemblyManager = orderedSieves.apply(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep).size should be(1)
    val pr = am.getPrecedenceRelationsFor(uRep).head
    pr.before.isEquivalentTo(pRep, ignoreMods = false) should be (true)
    pr.before.isEquivalentTo(pRep, ignoreMods = true) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = false) should be (true)
    pr.after.isEquivalentTo(uRep, ignoreMods = true) should be (true)
  }


  // Play it safe by ignoring cues not at the beginning of the sentence.
  val interSent5 = "AFT was ubiquitinated. There is intervening material and, previously, BEF was phosphorylated."

  interSent5 should "have no precedence relations" in {
    val mentions = getMentionsFromText(interSent5)

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val orderedSieves =
    // track relevant mentions
      AssemblySieve(dedup.trackMentions) andThen
        // find precedence using intersentential Odin rules
        AssemblySieve(precedence.intersententialRBPrecedence)

    val am: AssemblyManager = orderedSieves.apply(mentions)

    val uRep = am.distinctSimpleEvents("Ubiquitination").head
    val pRep = am.distinctSimpleEvents("Phosphorylation").head

    am.distinctPredecessorsOf(uRep) should have size (0)
  }

}
