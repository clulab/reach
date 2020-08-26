package org.clulab.reach.assembly

import org.clulab.reach.assembly.representations._
import org.scalatest.{ FlatSpec, Matchers }
import org.clulab.reach.assembly.TestUtils.{ jsonStringToDocument, getMentionsFromDocument }


class TestAssemblyManager extends FlatSpec with Matchers {

  val text1 = "Ras is phosphorylated."
  val text2 = "Ras was phosphorylated."
  val text3 = "Ras is phosphorylated at Ser123."
  val text4 = "Ras is phosphorylated by MEK at Ser123."
  val text5 = "Mek binds with Ras."
  val text6 = "Ras binds with MEK."
  // translocations
  val text7 = "ASPP2 is transported from the membrane to the nucleus and cytosol"

  // the assembly manager is not intimidated by multiple documents
  //  val doc = jsonStringToDocument("""  """)
  //  val mentions = getMentionsFromDocument(doc)
  val doc1 = jsonStringToDocument("""  {"id":"text","text":"Ras is phosphorylated.","sentences":[{"words":["Ras","is","phosphorylated","."],"startOffsets":[0,4,7,21],"endOffsets":[3,6,21,22],"raw":["Ras","is","phosphorylated","."],"tags":["NN","VBZ","VBN","."],"lemmas":["ras","be","phosphorylate","."],"entities":["B-Family","O","O","O"],"chunks":["B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]}}}]} """)
  val mentions1 = getMentionsFromDocument(doc1)
  val doc2 = jsonStringToDocument(""" {"id":"text","text":"Ras was phosphorylated.","sentences":[{"words":["Ras","was","phosphorylated","."],"startOffsets":[0,4,8,22],"endOffsets":[3,7,22,23],"raw":["Ras","was","phosphorylated","."],"tags":["NN","VBD","VBN","."],"lemmas":["ras","be","phosphorylate","."],"entities":["B-Family","O","O","O"],"chunks":["B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]}}}]} """)
  val mentions2 = getMentionsFromDocument(doc2)
  val doc3 = jsonStringToDocument(""" {"id":"text","text":"Ras is phosphorylated at Ser123.","sentences":[{"words":["Ras","is","phosphorylated","at","Ser123","."],"startOffsets":[0,4,7,22,25,31],"endOffsets":[3,6,21,24,31,32],"raw":["Ras","is","phosphorylated","at","Ser123","."],"tags":["NN","VBZ","VBN","IN","NN","."],"lemmas":["ras","be","phosphorylate","at","ser123","."],"entities":["B-Family","O","O","O","O","O"],"chunks":["B-NP","B-VP","I-VP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":4,"relation":"nmod_at"},{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]},"universal-basic":{"edges":[{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":2,"destination":4,"relation":"nmod"}],"roots":[2]}}}]} """)
  val mentions3 = getMentionsFromDocument(doc3)
  val doc4 = jsonStringToDocument(""" {"id":"text","text":"Ras is phosphorylated by MEK at Ser123.","sentences":[{"words":["Ras","is","phosphorylated","by","MEK","at","Ser123","."],"startOffsets":[0,4,7,22,25,29,32,38],"endOffsets":[3,6,21,24,28,31,38,39],"raw":["Ras","is","phosphorylated","by","MEK","at","Ser123","."],"tags":["NN","VBZ","VBN","IN","NN","IN","NN","."],"lemmas":["ras","be","phosphorylate","by","mek","at","ser123","."],"entities":["B-Family","O","O","O","B-Family","O","O","O"],"chunks":["B-NP","B-VP","I-VP","B-PP","B-NP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"auxpass"},{"source":6,"destination":5,"relation":"case"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":2,"destination":4,"relation":"nmod_agent"},{"source":2,"destination":6,"relation":"nmod_at"}],"roots":[2]},"universal-basic":{"edges":[{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"auxpass"},{"source":6,"destination":5,"relation":"case"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":2,"destination":4,"relation":"nmod"},{"source":2,"destination":6,"relation":"nmod"}],"roots":[2]}}}]} """)
  val mentions4 = getMentionsFromDocument(doc4)
  val doc5 = jsonStringToDocument(""" {"id":"text","text":"Mek binds with Ras.","sentences":[{"words":["Mek","binds","with","Ras","."],"startOffsets":[0,4,10,15,18],"endOffsets":[3,9,14,18,19],"raw":["Mek","binds","with","Ras","."],"tags":["NN","VBZ","IN","NN","."],"lemmas":["mek","bind","with","ras","."],"entities":["B-Family","O","O","B-Family","O"],"chunks":["B-NP","B-VP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":1,"destination":3,"relation":"nmod_with"},{"source":1,"destination":0,"relation":"nsubj"},{"source":3,"destination":2,"relation":"case"}],"roots":[1]},"universal-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":3,"destination":2,"relation":"case"},{"source":1,"destination":3,"relation":"nmod"}],"roots":[1]}}}]} """)
  val mentions5 = getMentionsFromDocument(doc5)
  val doc6 = jsonStringToDocument(""" {"id":"text","text":"Ras binds with MEK.","sentences":[{"words":["Ras","binds","with","MEK","."],"startOffsets":[0,4,10,15,18],"endOffsets":[3,9,14,18,19],"raw":["Ras","binds","with","MEK","."],"tags":["NN","VBZ","IN","NN","."],"lemmas":["ras","bind","with","mek","."],"entities":["B-Family","O","O","B-Family","O"],"chunks":["B-NP","B-VP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":1,"destination":3,"relation":"nmod_with"},{"source":1,"destination":0,"relation":"nsubj"},{"source":3,"destination":2,"relation":"case"}],"roots":[1]},"universal-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":3,"destination":2,"relation":"case"},{"source":1,"destination":3,"relation":"nmod"}],"roots":[1]}}}]} """)
  val mentions6 = getMentionsFromDocument(doc6)
  // translocations
  val doc7 = jsonStringToDocument(""" {"id":"text","text":"ASPP2 is transported from the membrane to the nucleus and cytosol","sentences":[{"words":["ASPP2","is","transported","from","the","membrane","to","the","nucleus","and","cytosol"],"startOffsets":[0,6,9,21,26,30,39,42,46,54,58],"endOffsets":[5,8,20,25,29,38,41,45,53,57,65],"raw":["ASPP2","is","transported","from","the","membrane","to","the","nucleus","and","cytosol"],"tags":["NN","VBZ","VBN","IN","DT","NN","TO","DT","NN","CC","NN"],"lemmas":["aspp2","be","transport","from","the","membrane","to","the","nucleus","and","cytosol"],"entities":["B-Gene_or_gene_product","O","O","O","O","B-Cellular_component","O","O","B-Cellular_component","O","B-Cellular_component"],"chunks":["B-NP","B-VP","I-VP","B-PP","B-NP","I-NP","B-PP","B-NP","I-NP","I-NP","I-NP"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":10,"relation":"nmod_to"},{"source":5,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"auxpass"},{"source":8,"destination":6,"relation":"case"},{"source":2,"destination":5,"relation":"nmod_from"},{"source":2,"destination":8,"relation":"nmod_to"},{"source":8,"destination":7,"relation":"det"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":5,"destination":4,"relation":"det"},{"source":8,"destination":9,"relation":"cc"},{"source":8,"destination":10,"relation":"conj_and"}],"roots":[2]},"universal-basic":{"edges":[{"source":5,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"auxpass"},{"source":8,"destination":6,"relation":"case"},{"source":8,"destination":7,"relation":"det"},{"source":2,"destination":8,"relation":"nmod"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":5,"destination":4,"relation":"det"},{"source":8,"destination":10,"relation":"conj"},{"source":8,"destination":9,"relation":"cc"},{"source":2,"destination":5,"relation":"nmod"}],"roots":[2]}}}]} """)
  val mentions7 = getMentionsFromDocument(doc7)

  //
  // SimpleEntity tests
  //

  s"$text1 $text2 $text3" should "contain a SimpleEntity representation for Ras without a PTM" in {
    val am = AssemblyManager()

    am.trackMentions(mentions1 ++ mentions2 ++ mentions3)

    val ras = mentions1.filter(m => (m matches "Entity") && (m.text.toLowerCase == "ras")).head
    val se = am.getSimpleEntity(ras)

    se.getPTMs should have size(0)
  }

  it should "have 3 mentions as evidence" in {
    val am = AssemblyManager()

    am.trackMentions(mentions1 ++ mentions2 ++ mentions3)

    val ras = mentions1.filter(m => (m matches "Entity") && (m.text.toLowerCase == "ras")).head

    val se = am.getEER(ras)

    se.evidence should have size(3)
  }

  //
  // SimpleEvent tests
  //

  it should "have three SimpleEvent representations (non-distinct) for the Phosphorylation of Ras" in {
    val am = AssemblyManager()

    am.trackMentions(mentions1 ++ mentions2 ++ mentions3)

    val phosEvents = am.getSimpleEvents("Phosphorylation")

    phosEvents should have size(3)
  }

  it should "have two distinct SimpleEvent representations for the Phosphorylation of Ras" in {
    val am = AssemblyManager()

    am.trackMentions(mentions1 ++ mentions2 ++ mentions3)

    val distinctPhosEvents = am.distinctSimpleEvents("Phosphorylation")

    distinctPhosEvents should have size(2)
  }

  text3 should "have a SimpleEntity representing Ras w/ PTM(phos + site) as output to a phosphorylation" in {
    val am = AssemblyManager()

    am.trackMentions(mentions3)

    val p = mentions3.filter(_ matches "Phosphorylation").head

    val phos = am.getSimpleEvent(p)

    phos.evidence should have size(1)

    phos.output should have size(1)

    val outputEntity = phos.output.head.asInstanceOf[SimpleEntity]

    val ptms = outputEntity.getPTMs

    ptms should have size(1)

    val ptm = ptms.head

    ptm.label should be("Phosphorylation")

    ptm.site.isDefined should be(true)
  }

  //
  // Translocation tests
  //

  s"$text7" should "contain 2 Translocation events" in {
    val am = AssemblyManager(mentions7)

    val translocationMentions = mentions7 filter( _ matches "Translocation" )

    // there should only be two translocations
    am.distinctSimpleEvents count ( _.label == "Translocation" ) should be(2)

    val t1 = am.getSimpleEvent(translocationMentions.head)
    // the input and output of the translocations should differ in terms of modifications
    t1.I != t1.O should be(true)
    // however, the grounding ids of both entities should be the same
    t1.I.size should be(1)
    t1.I.head.asInstanceOf[SimpleEntity].grounding == t1.O.head.asInstanceOf[SimpleEntity].grounding should be(true)
    t1.I.head.asInstanceOf[SimpleEntity].withSameGrounding contains t1.O.head.asInstanceOf[SimpleEntity] should be(true)

    val t2 = am.getSimpleEvent(translocationMentions.last)
    // the input and output of the translocations should differ in terms of modifications
    t2.I != t2.O should be(true)
    // however, the grounding ids of both entities should be the same
    t2.I should have size (1)
    t2.I.head.asInstanceOf[SimpleEntity].grounding == t2.O.head.asInstanceOf[SimpleEntity].grounding should be(true)
    t2.I.head.asInstanceOf[SimpleEntity].withSameGrounding contains t2.O.head.asInstanceOf[SimpleEntity] should be(true)
  }


  //
  // Complex tests
  //

  s"$text5 $text6" should "contain 2 Complexes (one for each mention) representing Mek-Ras binding" in {
    val am = AssemblyManager()

    am.trackMentions(mentions5 ++ mentions6)

    val complexes = am.getComplexes

    complexes should have size(2)
  }

  it should "have 2 equivalent Complexes representing Mek-Ras binding" in {
    val am = AssemblyManager()

    am.trackMentions(mentions5 ++ mentions6)

    val complexes = am.getComplexes

    val (c1, c2) = (complexes.head, complexes.last)
    c1.isEquivalentTo(c2, ignoreMods = false) should be(true)
    c1.isEquivalentTo(c2, ignoreMods = true) should be(true)
  }

  it should "have 2 mentions as evidence" in {
    val am = AssemblyManager()

    am.trackMentions(mentions5 ++ mentions6)

    val b = mentions5.filter(_ matches "Binding").head

    val complex = am.getSimpleEvent(b)

    val evidence = complex.evidence

    evidence contains b should be(true)
    evidence should have size(2)
  }

  //
  // RemovalEvent tests
  //

  val dePhos = "Mek was dephosphorylated."
  dePhos should "produce a Dephosphorylation event with a Phosphorylation on the input" in {

    val doc = jsonStringToDocument(""" {"id":"text","text":"Mek was dephosphorylated.","sentences":[{"words":["Mek","was","dephosphorylated","."],"startOffsets":[0,4,8,24],"endOffsets":[3,7,24,25],"raw":["Mek","was","dephosphorylated","."],"tags":["NN","VBD","VBN","."],"lemmas":["mek","be","dephosphorylate","."],"entities":["B-Family","O","O","O"],"chunks":["B-NP","B-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"}],"roots":[2]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

    val am = AssemblyManager(mentions)

    val m = mentions.filter(_ matches "Dephosphorylation").head

    val rem = am.getSimpleEvent(m)

    val themes = rem.input("theme")
    themes should have size (1)

    val theme = themes.head.asInstanceOf[SimpleEntity]

    val inputPTMs = theme.getPTMs
    inputPTMs should have size (1)

    val phosInputPTMs = theme.getPTMs("Phosphorylation")
    phosInputPTMs should have size (1)

    val output = rem.output
    output should have size (1)

    val outputTheme = output.head.asInstanceOf[SimpleEntity]
    val outputPTMs = outputTheme.getPTMs
    outputPTMs should have size (0)

    val phosOutputPTMs = outputTheme.getPTMs("Phosphorylation")
    phosOutputPTMs should have size (0)
  }

  //
  // Regulation tests
  //

  val regText1 = "AFT is phosphorylated by BEF."
  regText1 should "produce one Regulation representation" in {

    val doc = jsonStringToDocument(""" {"id":"text","text":"AFT is phosphorylated by BEF.","sentences":[{"words":["AFT","is","phosphorylated","by","BEF","."],"startOffsets":[0,4,7,22,25,28],"endOffsets":[3,6,21,24,28,29],"raw":["AFT","is","phosphorylated","by","BEF","."],"tags":["NNP","VBZ","VBN","IN","NN","."],"lemmas":["AFT","be","phosphorylate","by","bef","."],"entities":["B-Gene_or_gene_product","O","O","O","B-Gene_or_gene_product","O"],"chunks":["B-NP","B-VP","I-VP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":2,"destination":4,"relation":"nmod_agent"}],"roots":[2]},"universal-basic":{"edges":[{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"auxpass"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":2,"destination":4,"relation":"nmod"}],"roots":[2]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

    val am = AssemblyManager(mentions)

    am.getRegulations should have size (1)
    am.distinctRegulations should have size (1)
  }

  val regText2 = "Akt inhibits the phosphorylation of AFT by BEF."
  regText2 should "produce two Regulation representations (one with nesting)" in {

    val doc = jsonStringToDocument(""" {"id":"text","text":"Akt inhibits the phosphorylation of AFT by BEF.","sentences":[{"words":["Akt","inhibits","the","phosphorylation","of","AFT","by","BEF","."],"startOffsets":[0,4,13,17,33,36,40,43,46],"endOffsets":[3,12,16,32,35,39,42,46,47],"raw":["Akt","inhibits","the","phosphorylation","of","AFT","by","BEF","."],"tags":["NN","VBZ","DT","NN","IN","NN","IN","NN","."],"lemmas":["akt","inhibit","the","phosphorylation","of","aft","by","bef","."],"entities":["B-Family","O","O","O","O","B-Gene_or_gene_product","O","B-Gene_or_gene_product","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","B-PP","B-NP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":1,"destination":3,"relation":"dobj"},{"source":3,"destination":2,"relation":"det"},{"source":3,"destination":5,"relation":"nmod_of"},{"source":5,"destination":4,"relation":"case"},{"source":1,"destination":0,"relation":"nsubj"},{"source":7,"destination":6,"relation":"case"},{"source":1,"destination":7,"relation":"nmod_by"}],"roots":[1]},"universal-basic":{"edges":[{"source":1,"destination":3,"relation":"dobj"},{"source":3,"destination":2,"relation":"det"},{"source":1,"destination":7,"relation":"nmod"},{"source":5,"destination":4,"relation":"case"},{"source":1,"destination":0,"relation":"nsubj"},{"source":3,"destination":5,"relation":"nmod"},{"source":7,"destination":6,"relation":"case"}],"roots":[1]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

    val am = AssemblyManager(mentions)

    val regs: Set[Regulation] = am.distinctRegulations

    am.getRegulations should have size (2)
    regs should have size (2)

    regs.count(r => r.controlled.head.isInstanceOf[Regulation]) should be(1)
  }

  //
  // Negation tests
  //

  val negPhos = "Mek was not phosphorylated."
  val docNegPhos = jsonStringToDocument(""" {"id":"text","text":"Mek was not phosphorylated.","sentences":[{"words":["Mek","was","not","phosphorylated","."],"startOffsets":[0,4,8,12,26],"endOffsets":[3,7,11,26,27],"raw":["Mek","was","not","phosphorylated","."],"tags":["NN","VBD","RB","VBN","."],"lemmas":["mek","be","not","phosphorylate","."],"entities":["B-Family","O","O","O","O"],"chunks":["B-NP","B-VP","I-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":3,"destination":1,"relation":"auxpass"},{"source":3,"destination":0,"relation":"nsubjpass"},{"source":3,"destination":2,"relation":"neg"}],"roots":[3]},"universal-basic":{"edges":[{"source":3,"destination":1,"relation":"auxpass"},{"source":3,"destination":0,"relation":"nsubjpass"},{"source":3,"destination":2,"relation":"neg"}],"roots":[3]}}}]} """)
  negPhos should "have a negated SimpleEvent representation for the Phosphorylation mention" in {

    val mentions = getMentionsFromDocument(docNegPhos)

    val am = AssemblyManager(mentions)

    val m = mentions.filter(_ matches "Phosphorylation").head

    val phos = am.getSimpleEvent(m)

    phos.negated should be(true)
  }

  val negReg = "Mek was not phosphorylated by Ras."
  val docNegReg = jsonStringToDocument(""" {"id":"text","text":"Mek was not phosphorylated by Ras.","sentences":[{"words":["Mek","was","not","phosphorylated","by","Ras","."],"startOffsets":[0,4,8,12,27,30,33],"endOffsets":[3,7,11,26,29,33,34],"raw":["Mek","was","not","phosphorylated","by","Ras","."],"tags":["NN","VBD","RB","VBN","IN","NN","."],"lemmas":["mek","be","not","phosphorylate","by","ras","."],"entities":["B-Family","O","O","O","O","B-Family","O"],"chunks":["B-NP","B-VP","I-VP","I-VP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":3,"destination":5,"relation":"nmod_agent"},{"source":3,"destination":1,"relation":"auxpass"},{"source":5,"destination":4,"relation":"case"},{"source":3,"destination":0,"relation":"nsubjpass"},{"source":3,"destination":2,"relation":"neg"}],"roots":[3]},"universal-basic":{"edges":[{"source":3,"destination":1,"relation":"auxpass"},{"source":5,"destination":4,"relation":"case"},{"source":3,"destination":0,"relation":"nsubjpass"},{"source":3,"destination":2,"relation":"neg"},{"source":3,"destination":5,"relation":"nmod"}],"roots":[3]}}}]} """)
  negReg should "have a negated Regulation representation" in {

    val mentions = getMentionsFromDocument(docNegReg)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val m = mentions.filter(_ matches "Regulation").head

    val reg = am.getRegulation(m)

    reg.negated should be(true)
  }

  it should "not have a negated SimpleEvent representation for the Phosphorylation" in {

    val mentions = getMentionsFromDocument(docNegReg)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val m = mentions.filter(_ matches "Phosphorylation").head

    val phos = am.getSimpleEvent(m)

    phos.negated should be(false)
  }

  // test PrecedenceRelations

  it should "not have any precedence relations for the phosphorylation" in {

    val mentions = getMentionsFromDocument(docNegReg)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val p = mentions.filter(_ matches "Phosphorylation").head

    // test AssemblyManager's methods
    am.predecessorsOf(p) should have size (0)
    am.distinctPredecessorsOf(p) should have size (0)
    am.successorsOf(p) should have size (0)
    am.distinctSuccessorsOf(p) should have size (0)

    val se = am.getSimpleEvent(p)

    // test Event methods
    se.predecessors should have size (0)
    se.distinctPredecessors should have size (0)
    se.successors should have size (0)
    se.distinctSuccessors should have size (0)
  }

  val precedenceText = "Ras is phosphorylated by Mek after Mek is bound to p53."
  precedenceText should "have a PrecedenceRelation showing the Phosphorylation following the Binding" in {

    val doc = jsonStringToDocument(""" {"id":"text","text":"Ras is phosphorylated by Mek after Mek is bound to p53.","sentences":[{"words":["Ras","is","phosphorylated","by","Mek","after","Mek","is","bound","to","p53","."],"startOffsets":[0,4,7,22,25,29,35,39,42,48,51,54],"endOffsets":[3,6,21,24,28,34,38,41,47,50,54,55],"raw":["Ras","is","phosphorylated","by","Mek","after","Mek","is","bound","to","p53","."],"tags":["NN","VBZ","VBN","IN","NN","IN","NN","VBZ","VBN","TO","NN","."],"lemmas":["ras","be","phosphorylate","by","mek","after","mek","be","bind","to","p53","."],"entities":["B-Family","O","O","O","B-Family","O","B-Family","O","O","O","B-Gene_or_gene_product","O"],"chunks":["B-NP","B-VP","I-VP","B-PP","B-NP","B-SBAR","B-NP","B-VP","I-VP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"auxpass"},{"source":10,"destination":9,"relation":"case"},{"source":2,"destination":8,"relation":"advcl_after"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":8,"destination":10,"relation":"nmod_to"},{"source":2,"destination":4,"relation":"nmod_agent"},{"source":8,"destination":6,"relation":"nsubjpass"},{"source":8,"destination":7,"relation":"auxpass"},{"source":8,"destination":5,"relation":"mark"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":8,"relation":"advcl"},{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"auxpass"},{"source":8,"destination":10,"relation":"nmod"},{"source":10,"destination":9,"relation":"case"},{"source":2,"destination":0,"relation":"nsubjpass"},{"source":2,"destination":4,"relation":"nmod"},{"source":8,"destination":6,"relation":"nsubjpass"},{"source":8,"destination":7,"relation":"auxpass"},{"source":8,"destination":5,"relation":"mark"}],"roots":[2]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val p = mentions.filter(_ matches "Phosphorylation").head
    val b = mentions.filter(_ matches "Binding").head

    // test mention-based methods
    am.storePrecedenceRelation(p, b, foundBy = "mention-based-test")

    am.getPrecedenceRelationsFor(p) should have size (1)
    am.predecessorsOf(p) should have size (0)
    am.distinctPredecessorsOf(p) should have size (0)
    am.successorsOf(p) should have size (1)
    am.distinctSuccessorsOf(p) should have size (1)

    am.getPrecedenceRelationsFor(b) should have size (1)
    am.predecessorsOf(b) should have size (1)
    am.distinctPredecessorsOf(b) should have size (1)
    am.successorsOf(b) should have size (0)
    am.distinctSuccessorsOf(b) should have size (0)

    // test eer-based methods
    val pSE = am.getSimpleEvent(p)
    val bSE = am.getSimpleEvent(b)

    // test mention-based methods
    am.storePrecedenceRelation(p, b, foundBy = "mention-based-test")

    pSE.precedenceRelations should have size (1)
    pSE.predecessors should have size (0)
    pSE.distinctPredecessors should have size (0)
    pSE.successors should have size (1)
    pSE.distinctSuccessors should have size (1)

    bSE.precedenceRelations should have size (1)
    bSE.predecessors should have size (1)
    bSE.distinctPredecessors should have size (1)
    bSE.successors should have size (0)
    bSE.distinctSuccessors should have size (0)

    // test distinct v. non-distinct
    am.storePrecedenceRelation(p, b, foundBy = "mention-based-test2")
    // this is a set, but the difference in
    // foundBy means "mention-based-test2" won't get collapsed
    pSE.precedenceRelations should have size (2)
    pSE.predecessors should have size (0)
    pSE.distinctPredecessors should have size (0)
    pSE.successors should have size (1)
    pSE.distinctSuccessors should have size (1)
    // this is a set, but the difference in
    // foundBy means "mention-based-test2" won't get collapsed
    bSE.precedenceRelations should have size (2)
    bSE.predecessors should have size (1)
    bSE.distinctPredecessors should have size (1)
    bSE.successors should have size (0)
    bSE.distinctSuccessors should have size (0)
  }

  "AssemblyManager" should s"not contain any EERs for '$negPhos' if all EERs referencing Mek are removed" in {

    val mentions = getMentionsFromDocument(docNegPhos)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val m = mentions.filter(_ matches "Entity").head

    am.removeEntriesContainingIDofMention(m)

    am.EERs should have size (0)
  }

  it should "safely handle mentions in any order" in {
    val text = "EHT1864 inhibited AKT phosphorylation induced by LPA and S1P, but not EGF or PDGF"

    val doc = jsonStringToDocument(""" {"id":"text","text":"EHT1864 inhibited AKT phosphorylation induced by LPA and S1P, but not EGF or PDGF","sentences":[{"words":["EHT1864","inhibited","AKT","phosphorylation","induced","by","LPA","and","S1P",",","but","not","EGF","or","PDGF"],"startOffsets":[0,8,18,22,38,46,49,53,57,60,62,66,70,74,77],"endOffsets":[7,17,21,37,45,48,52,56,60,61,65,69,73,76,81],"raw":["EHT1864","inhibited","AKT","phosphorylation","induced","by","LPA","and","S1P",",","but","not","EGF","or","PDGF"],"tags":["NN","VBD","NN","NN","VBN","IN","NN","CC","NN",",","CC","RB","NN","CC","NN"],"lemmas":["eht1864","inhibit","akt","phosphorylation","induce","by","lpa","and","s1p",",","but","not","egf","or","pdgf"],"entities":["B-Simple_chemical","O","B-Family","O","O","O","B-Simple_chemical","O","B-Gene_or_gene_product","O","O","O","B-Gene_or_gene_product","O","B-Family"],"chunks":["B-NP","B-VP","B-NP","I-NP","B-VP","B-PP","B-NP","I-NP","I-NP","B-NP","I-NP","I-NP","I-NP","I-NP","I-NP"],"graphs":{"universal-enhanced":{"edges":[{"source":3,"destination":11,"relation":"cc"},{"source":1,"destination":3,"relation":"dobj"},{"source":3,"destination":14,"relation":"conj_negcc"},{"source":3,"destination":12,"relation":"conj_negcc"},{"source":11,"destination":10,"relation":"cc"},{"source":6,"destination":8,"relation":"conj_and"},{"source":12,"destination":13,"relation":"cc"},{"source":6,"destination":7,"relation":"cc"},{"source":12,"destination":14,"relation":"conj_or"},{"source":3,"destination":2,"relation":"compound"},{"source":1,"destination":12,"relation":"dobj"},{"source":4,"destination":8,"relation":"nmod_by"},{"source":6,"destination":5,"relation":"case"},{"source":4,"destination":6,"relation":"nmod_by"},{"source":1,"destination":0,"relation":"nsubj"},{"source":3,"destination":4,"relation":"acl"}],"roots":[1]},"universal-basic":{"edges":[{"source":3,"destination":11,"relation":"cc"},{"source":1,"destination":3,"relation":"dobj"},{"source":3,"destination":2,"relation":"compound"},{"source":6,"destination":8,"relation":"conj"},{"source":6,"destination":5,"relation":"case"},{"source":11,"destination":10,"relation":"cc"},{"source":3,"destination":12,"relation":"conj"},{"source":12,"destination":14,"relation":"conj"},{"source":1,"destination":0,"relation":"nsubj"},{"source":4,"destination":6,"relation":"nmod"},{"source":3,"destination":4,"relation":"acl"},{"source":12,"destination":13,"relation":"cc"},{"source":6,"destination":7,"relation":"cc"}],"roots":[1]}}}]} """)
    val mentions = getMentionsFromDocument(doc)

    val am1 = AssemblyManager(mentions)
    val am2 = AssemblyManager(mentions.sortBy(_.label))
    val am3 = AssemblyManager(mentions.sortBy(_.label).reverse)

    def hasEquivalentEERs(manager1: AssemblyManager, manager2: AssemblyManager): Boolean = {
      val eers1 = manager1.distinctEERs.map(_.equivalenceHash(ignoreMods = false))
      val eers2 = manager2.distinctEERs.map(_.equivalenceHash(ignoreMods = false))
      val entities1 = manager1.distinctEntities.map(_.equivalenceHash(ignoreMods = false))
      val entities2 = manager2.distinctEntities.map(_.equivalenceHash(ignoreMods = false))
      val events1 = manager1.distinctEvents.map(_.equivalenceHash(ignoreMods = false))
      val events2 = manager2.distinctEvents.map(_.equivalenceHash(ignoreMods = false))

      ((eers1 intersect eers2).size == (eers1 union eers2).size) &&
        ((entities1 intersect entities2).size == (entities1 union entities2).size) &&
        ((events1 intersect events2).size == (events1 union events2).size)
    }

    hasEquivalentEERs(am1, am1) should be(true)
    hasEquivalentEERs(am2, am2) should be(true)
    hasEquivalentEERs(am3, am3) should be(true)
    hasEquivalentEERs(am1, am2) should be(true)
    hasEquivalentEERs(am1, am3) should be(true)
    hasEquivalentEERs(am2, am3) should be(true)
  }
}
