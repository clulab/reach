package org.clulab.reach.assembly

import org.scalatest.{ FlatSpec, Matchers }
import org.clulab.reach.assembly.TestUtils.{ jsonStringToDocument, getMentionsFromDocument }
import org.clulab.reach.assembly.sieves.{ AssemblySieve, DeduplicationSieves, PrecedenceSieves }


/**
  * Assembly sieve tests
  */
class TestAssemblySieves extends FlatSpec with Matchers {

  // Reichenbach rules

  val tamSent1 = "Once BEF had been phosphorylated, AFT was ubiquitinated"

  tamSent1 should "be annotated with the phosphorylation preceding the ubiquitination" in {

    val doc = jsonStringToDocument(""" {"id":"text","text":"Once BEF had been phosphorylated, AFT was ubiquitinated","sentences":[{"words":["Once","BEF","had","been","phosphorylated",",","AFT","was","ubiquitinated"],"startOffsets":[0,5,9,13,18,32,34,38,42],"endOffsets":[4,8,12,17,32,33,37,41,55],"raw":["Once","BEF","had","been","phosphorylated",",","AFT","was","ubiquitinated"],"tags":["RB","NNP","VBD","VBN","VBN",",","NNP","VBD","VBN"],"lemmas":["once","BEF","have","be","phosphorylate",",","AFT","be","ubiquitinate"],"entities":["O","B-Gene_or_gene_product","O","O","O","O","B-Gene_or_gene_product","O","O"],"chunks":["O","B-NP","B-VP","I-VP","I-VP","O","B-NP","B-VP","I-VP"],"graphs":{"universal-enhanced":{"edges":[{"source":4,"destination":2,"relation":"aux"},{"source":8,"destination":4,"relation":"advcl"},{"source":8,"destination":6,"relation":"nsubjpass"},{"source":4,"destination":0,"relation":"advmod"},{"source":8,"destination":7,"relation":"auxpass"},{"source":4,"destination":3,"relation":"auxpass"},{"source":4,"destination":1,"relation":"nsubjpass"}],"roots":[8]},"universal-basic":{"edges":[{"source":4,"destination":2,"relation":"aux"},{"source":8,"destination":4,"relation":"advcl"},{"source":8,"destination":6,"relation":"nsubjpass"},{"source":4,"destination":0,"relation":"advmod"},{"source":8,"destination":7,"relation":"auxpass"},{"source":4,"destination":3,"relation":"auxpass"},{"source":4,"destination":1,"relation":"nsubjpass"}],"roots":[8]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

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

    val doc = jsonStringToDocument(""" {"id":"text","text":"AFT will be ubiquitinated only if BEF is first phosphorylated","sentences":[{"words":["AFT","will","be","ubiquitinated","only","if","BEF","is","first","phosphorylated"],"startOffsets":[0,4,9,12,26,31,34,38,41,47],"endOffsets":[3,8,11,25,30,33,37,40,46,61],"raw":["AFT","will","be","ubiquitinated","only","if","BEF","is","first","phosphorylated"],"tags":["NNP","MD","VB","VBN","RB","IN","NN","VBZ","JJ","VBN"],"lemmas":["AFT","will","be","ubiquitinate","only","if","bef","be","first","phosphorylate"],"entities":["B-Gene_or_gene_product","O","O","O","O","O","B-Gene_or_gene_product","O","O","O"],"chunks":["B-NP","B-VP","I-VP","I-VP","B-SBAR","I-SBAR","B-NP","B-VP","B-ADJP","I-ADJP"],"graphs":{"universal-enhanced":{"edges":[{"source":3,"destination":2,"relation":"auxpass"},{"source":9,"destination":7,"relation":"auxpass"},{"source":3,"destination":1,"relation":"aux"},{"source":9,"destination":6,"relation":"nsubjpass"},{"source":3,"destination":9,"relation":"advcl_if"},{"source":3,"destination":0,"relation":"nsubjpass"},{"source":9,"destination":8,"relation":"advmod"},{"source":9,"destination":5,"relation":"mark"},{"source":9,"destination":4,"relation":"advmod"}],"roots":[3]},"universal-basic":{"edges":[{"source":3,"destination":2,"relation":"auxpass"},{"source":9,"destination":7,"relation":"auxpass"},{"source":3,"destination":1,"relation":"aux"},{"source":9,"destination":6,"relation":"nsubjpass"},{"source":3,"destination":0,"relation":"nsubjpass"},{"source":9,"destination":8,"relation":"advmod"},{"source":9,"destination":5,"relation":"mark"},{"source":9,"destination":4,"relation":"advmod"},{"source":3,"destination":9,"relation":"advcl"}],"roots":[3]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

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

    val doc = jsonStringToDocument(""" {"id":"text","text":"AFT was ubiquitinated when BEF had been phosphorylated","sentences":[{"words":["AFT","was","ubiquitinated","when","BEF","had","been","phosphorylated"],"startOffsets":[0,4,8,22,27,31,35,40],"endOffsets":[3,7,21,26,30,34,39,54],"raw":["AFT","was","ubiquitinated","when","BEF","had","been","phosphorylated"],"tags":["NNP","VBD","VBN","WRB","NNP","VBD","VBN","VBN"],"lemmas":["AFT","be","ubiquitinate","when","BEF","have","be","phosphorylate"],"entities":["B-Gene_or_gene_product","O","O","O","B-Gene_or_gene_product","O","O","O"],"chunks":["B-NP","B-VP","I-VP","B-ADVP","B-NP","B-VP","I-VP","I-VP"],"graphs":{"universal-enhanced":{"edges":[{"source":7,"destination":4,"relation":"nsubjpass"},{"source":2,"destination":7,"relation":"advcl"},{"source":7,"destination":5,"relation":"aux"},{"source":7,"destination":6,"relation":"auxpass"},{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":7,"destination":3,"relation":"advmod"}],"roots":[2]},"universal-basic":{"edges":[{"source":7,"destination":4,"relation":"nsubjpass"},{"source":2,"destination":7,"relation":"advcl"},{"source":7,"destination":5,"relation":"aux"},{"source":7,"destination":6,"relation":"auxpass"},{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":7,"destination":3,"relation":"advmod"}],"roots":[2]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

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

  val intraSent1 = "Together these data demonstrate that E2-induced SRC-3 phosphorylation is dependent on a direct interaction between SRC-3 and ERα and can occur outside of the nucleus."

  intraSent1 should "be annotated with the binding preceding the phosphorylation" in {

    val doc = jsonStringToDocument(""" {"id":"text","text":"Together these data demonstrate that E2-induced SRC-3 phosphorylation is dependent on a direct interaction between SRC-3 and ERα and can occur outside of the nucleus.","sentences":[{"words":["Together","these","data","demonstrate","that","E2","induced","SRC-3","phosphorylation","is","dependent","on","a","direct","interaction","between","SRC-3","and","ER","alpha","and","can","occur","outside","of","the","nucleus","."],"startOffsets":[0,9,15,20,32,37,40,48,54,70,73,83,86,88,95,107,115,121,125,127,129,133,137,143,151,154,158,165],"endOffsets":[8,14,19,31,36,39,47,53,69,72,82,85,87,94,106,114,120,124,127,128,132,136,142,150,153,157,165,166],"raw":["Together","these","data","demonstrate","that","E2","induced","SRC-3","phosphorylation","is","dependent","on","a","direct","interaction","between","SRC-3","and","ER","α","and","can","occur","outside","of","the","nucleus","."],"tags":["RB","DT","NNS","VBP","IN","NN","VBD","NN","NN","VBZ","JJ","IN","DT","JJ","NN","IN","NN","CC","NN","NN","CC","MD","VB","IN","IN","DT","NN","."],"lemmas":["together","these","datum","demonstrate","that","e2","induce","src-3","phosphorylation","be","dependent","on","a","direct","interaction","between","src-3","and","er","alpha","and","can","occur","outside","of","the","nucleus","."],"entities":["O","O","O","O","O","B-Simple_chemical","O","B-Gene_or_gene_product","O","O","O","O","O","O","O","O","B-Gene_or_gene_product","O","B-Gene_or_gene_product","O","O","O","O","O","O","O","B-Cellular_component","O"],"chunks":["B-ADVP","B-NP","I-NP","B-VP","B-SBAR","B-NP","B-VP","B-NP","I-NP","B-VP","B-ADJP","B-PP","B-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","I-NP","O","B-VP","I-VP","B-ADVP","B-PP","B-NP","I-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":14,"destination":16,"relation":"nmod_between"},{"source":10,"destination":8,"relation":"nsubj"},{"source":16,"destination":17,"relation":"cc"},{"source":2,"destination":1,"relation":"det"},{"source":26,"destination":24,"relation":"case"},{"source":3,"destination":6,"relation":"ccomp"},{"source":14,"destination":18,"relation":"nmod_between"},{"source":6,"destination":4,"relation":"mark"},{"source":19,"destination":20,"relation":"cc"},{"source":14,"destination":13,"relation":"amod"},{"source":22,"destination":26,"relation":"nmod_of"},{"source":22,"destination":21,"relation":"aux"},{"source":16,"destination":18,"relation":"conj_and"},{"source":8,"destination":7,"relation":"compound"},{"source":10,"destination":14,"relation":"nmod_on"},{"source":10,"destination":9,"relation":"cop"},{"source":19,"destination":22,"relation":"conj_and"},{"source":3,"destination":0,"relation":"advmod"},{"source":22,"destination":8,"relation":"nsubj:xsubj"},{"source":16,"destination":15,"relation":"case"},{"source":22,"destination":23,"relation":"advmod"},{"source":3,"destination":2,"relation":"nsubj"},{"source":10,"destination":19,"relation":"xcomp"},{"source":14,"destination":11,"relation":"case"},{"source":14,"destination":12,"relation":"det"},{"source":10,"destination":22,"relation":"xcomp"},{"source":26,"destination":25,"relation":"det"},{"source":6,"destination":5,"relation":"nsubj"},{"source":6,"destination":10,"relation":"ccomp"}],"roots":[3]},"universal-basic":{"edges":[{"source":10,"destination":14,"relation":"nmod"},{"source":10,"destination":8,"relation":"nsubj"},{"source":16,"destination":17,"relation":"cc"},{"source":2,"destination":1,"relation":"det"},{"source":10,"destination":9,"relation":"cop"},{"source":26,"destination":24,"relation":"case"},{"source":3,"destination":6,"relation":"ccomp"},{"source":3,"destination":0,"relation":"advmod"},{"source":6,"destination":4,"relation":"mark"},{"source":19,"destination":20,"relation":"cc"},{"source":19,"destination":22,"relation":"conj"},{"source":14,"destination":13,"relation":"amod"},{"source":16,"destination":15,"relation":"case"},{"source":16,"destination":18,"relation":"conj"},{"source":14,"destination":16,"relation":"nmod"},{"source":22,"destination":23,"relation":"advmod"},{"source":22,"destination":26,"relation":"nmod"},{"source":3,"destination":2,"relation":"nsubj"},{"source":10,"destination":19,"relation":"xcomp"},{"source":14,"destination":11,"relation":"case"},{"source":14,"destination":12,"relation":"det"},{"source":22,"destination":21,"relation":"aux"},{"source":26,"destination":25,"relation":"det"},{"source":8,"destination":7,"relation":"compound"},{"source":6,"destination":5,"relation":"nsubj"},{"source":6,"destination":10,"relation":"ccomp"}],"roots":[3]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

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

    val doc = jsonStringToDocument(""" {"id":"text","text":"BEF was phosphorylated. Then, AFT was ubiquitinated.","sentences":[{"words":["BEF","was","phosphorylated","."],"startOffsets":[0,4,8,22],"endOffsets":[3,7,22,23],"raw":["BEF","was","phosphorylated","."],"tags":["NN","VBD","VBN","."],"lemmas":["bef","be","phosphorylate","."],"entities":["B-Gene_or_gene_product","O","O","O"],"chunks":["B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]}}},{"words":["Then",",","AFT","was","ubiquitinated","."],"startOffsets":[24,28,30,34,38,51],"endOffsets":[28,29,33,37,51,52],"raw":["Then",",","AFT","was","ubiquitinated","."],"tags":["RB",",","NNP","VBD","VBN","."],"lemmas":["then",",","AFT","be","ubiquitinate","."],"entities":["O","O","B-Gene_or_gene_product","O","O","O"],"chunks":["B-ADVP","O","B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":4,"destination":2,"relation":"nsubjpass"},{"source":4,"destination":0,"relation":"advmod"},{"source":4,"destination":3,"relation":"auxpass"}],"roots":[4]},"universal-basic":{"edges":[{"source":4,"destination":2,"relation":"nsubjpass"},{"source":4,"destination":0,"relation":"advmod"},{"source":4,"destination":3,"relation":"auxpass"}],"roots":[4]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

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

    val doc = jsonStringToDocument(""" {"id":"text","text":"BEF was phosphorylated. Subsequently AFT was ubiquitinated.","sentences":[{"words":["BEF","was","phosphorylated","."],"startOffsets":[0,4,8,22],"endOffsets":[3,7,22,23],"raw":["BEF","was","phosphorylated","."],"tags":["NN","VBD","VBN","."],"lemmas":["bef","be","phosphorylate","."],"entities":["B-Gene_or_gene_product","O","O","O"],"chunks":["B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]}}},{"words":["Subsequently","AFT","was","ubiquitinated","."],"startOffsets":[24,37,41,45,58],"endOffsets":[36,40,44,58,59],"raw":["Subsequently","AFT","was","ubiquitinated","."],"tags":["NNP","NNP","VBD","VBN","."],"lemmas":["Subsequently","AFT","be","ubiquitinate","."],"entities":["O","B-Gene_or_gene_product","O","O","O"],"chunks":["B-NP","I-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":3,"destination":2,"relation":"auxpass"},{"source":3,"destination":1,"relation":"nsubjpass"},{"source":1,"destination":0,"relation":"compound"}],"roots":[3]},"universal-basic":{"edges":[{"source":3,"destination":2,"relation":"auxpass"},{"source":3,"destination":1,"relation":"nsubjpass"},{"source":1,"destination":0,"relation":"compound"}],"roots":[3]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

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

    val doc = jsonStringToDocument(""" {"id":"text","text":"AFT was ubiquitinated. Prior to this, BEF was phosphorylated.","sentences":[{"words":["AFT","was","ubiquitinated","."],"startOffsets":[0,4,8,21],"endOffsets":[3,7,21,22],"raw":["AFT","was","ubiquitinated","."],"tags":["NNP","VBD","VBN","."],"lemmas":["AFT","be","ubiquitinate","."],"entities":["B-Gene_or_gene_product","O","O","O"],"chunks":["B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]}}},{"words":["Prior","to","this",",","BEF","was","phosphorylated","."],"startOffsets":[23,29,32,36,38,42,46,60],"endOffsets":[28,31,36,37,41,45,60,61],"raw":["Prior","to","this",",","BEF","was","phosphorylated","."],"tags":["RB","TO","DT",",","NN","VBD","VBN","."],"lemmas":["prior","to","this",",","bef","be","phosphorylate","."],"entities":["O","O","O","O","B-Gene_or_gene_product","O","O","O"],"chunks":["B-ADVP","B-PP","B-NP","O","B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":0,"relation":"advmod"},{"source":6,"destination":5,"relation":"auxpass"},{"source":2,"destination":1,"relation":"case"},{"source":6,"destination":4,"relation":"nsubjpass"},{"source":6,"destination":2,"relation":"advcl_to"}],"roots":[6]},"universal-basic":{"edges":[{"source":2,"destination":0,"relation":"advmod"},{"source":6,"destination":5,"relation":"auxpass"},{"source":2,"destination":1,"relation":"case"},{"source":6,"destination":4,"relation":"nsubjpass"},{"source":6,"destination":2,"relation":"advcl"}],"roots":[6]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

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

    val doc = jsonStringToDocument(""" {"id":"text","text":"AFT was ubiquitinated. Previously, BEF was phosphorylated.","sentences":[{"words":["AFT","was","ubiquitinated","."],"startOffsets":[0,4,8,21],"endOffsets":[3,7,21,22],"raw":["AFT","was","ubiquitinated","."],"tags":["NNP","VBD","VBN","."],"lemmas":["AFT","be","ubiquitinate","."],"entities":["B-Gene_or_gene_product","O","O","O"],"chunks":["B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]}}},{"words":["Previously",",","BEF","was","phosphorylated","."],"startOffsets":[23,33,35,39,43,57],"endOffsets":[33,34,38,42,57,58],"raw":["Previously",",","BEF","was","phosphorylated","."],"tags":["RB",",","NN","VBD","VBN","."],"lemmas":["previously",",","bef","be","phosphorylate","."],"entities":["O","O","B-Gene_or_gene_product","O","O","O"],"chunks":["B-ADVP","O","B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":4,"destination":2,"relation":"nsubjpass"},{"source":4,"destination":0,"relation":"advmod"},{"source":4,"destination":3,"relation":"auxpass"}],"roots":[4]},"universal-basic":{"edges":[{"source":4,"destination":2,"relation":"nsubjpass"},{"source":4,"destination":0,"relation":"advmod"},{"source":4,"destination":3,"relation":"auxpass"}],"roots":[4]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

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

    val doc = jsonStringToDocument(""" {"id":"text","text":"AFT was ubiquitinated. There is intervening material and, previously, BEF was phosphorylated.","sentences":[{"words":["AFT","was","ubiquitinated","."],"startOffsets":[0,4,8,21],"endOffsets":[3,7,21,22],"raw":["AFT","was","ubiquitinated","."],"tags":["NNP","VBD","VBN","."],"lemmas":["AFT","be","ubiquitinate","."],"entities":["B-Gene_or_gene_product","O","O","O"],"chunks":["B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]}}},{"words":["There","is","intervening","material","and",",","previously",",","BEF","was","phosphorylated","."],"startOffsets":[23,29,32,44,53,56,58,68,70,74,78,92],"endOffsets":[28,31,43,52,56,57,68,69,73,77,92,93],"raw":["There","is","intervening","material","and",",","previously",",","BEF","was","phosphorylated","."],"tags":["EX","VBZ","VBG","NN","CC",",","RB",",","NN","VBD","VBN","."],"lemmas":["there","be","intervene","material","and",",","previously",",","bef","be","phosphorylate","."],"entities":["O","O","O","O","O","O","O","O","B-Gene_or_gene_product","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","O","O","B-ADVP","O","B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":4,"relation":"cc"},{"source":2,"destination":0,"relation":"expl"},{"source":2,"destination":10,"relation":"conj_and"},{"source":2,"destination":3,"relation":"dobj"},{"source":10,"destination":9,"relation":"auxpass"},{"source":10,"destination":6,"relation":"advmod"},{"source":10,"destination":8,"relation":"nsubjpass"},{"source":2,"destination":1,"relation":"aux"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":4,"relation":"cc"},{"source":2,"destination":0,"relation":"expl"},{"source":2,"destination":10,"relation":"conj"},{"source":2,"destination":3,"relation":"dobj"},{"source":10,"destination":9,"relation":"auxpass"},{"source":10,"destination":6,"relation":"advmod"},{"source":10,"destination":8,"relation":"nsubjpass"},{"source":2,"destination":1,"relation":"aux"}],"roots":[2]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

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
