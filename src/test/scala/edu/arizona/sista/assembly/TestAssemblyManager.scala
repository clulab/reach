package edu.arizona.sista.assembly

import edu.arizona.sista.assembly.representations.SimpleEntity
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.TestUtils._
import org.scalatest.{Matchers, FlatSpec}


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
  val doc1 = createDoc(text1, "assembly-test1")
  val doc2 = createDoc(text2, "assembly-test2")
  val doc3 = createDoc(text3, "assembly-test3")
  val doc4 = createDoc(text4, "assembly-test4")
  val doc5 = createDoc(text5, "assembly-test5")
  val doc6 = createDoc(text6, "assembly-test6")
  // translocations
  val doc7 = createDoc(text7, "assembly-test7")

  val mentions1 = testReach.extractFrom(doc1)
  val mentions2 = testReach.extractFrom(doc2)
  val mentions3 = testReach.extractFrom(doc3)
  val mentions4 = testReach.extractFrom(doc4)
  val mentions5 = testReach.extractFrom(doc5)
  val mentions6 = testReach.extractFrom(doc6)
  // translocations
  val mentions7 = testReach.extractFrom(doc7)

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

    val outputEntity =  phos.output.head.asInstanceOf[SimpleEntity]

    val ptms = outputEntity.getPTMs

    ptms should have size(1)

    val ptm = ptms.head

    ptm.label should be("Phosphorylation")

    ptm.site.isDefined should be(true)
  }

  // test Translocations
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
    t2.I.size should be(1)
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
    c1 isEquivalentTo c2 should be(true)
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

    val doc = createDoc(dePhos, "assembly-test")

    val mentions = testReach.extractFrom(doc)

    val am = AssemblyManager(mentions)

    val m = mentions.filter(_ matches "Dephosphorylation").head

    val rem = am.getSimpleEvent(m)

    val themes = rem.input("theme")
    themes.size should be(1)

    val theme = themes.head.asInstanceOf[SimpleEntity]

    val inputPTMs = theme.getPTMs
    inputPTMs.size should be(1)

    val phosInputPTMs = theme.getPTMs("Phosphorylation")
    phosInputPTMs.size should be(1)

    val output = rem.output
    output.size should be(1)

    val outputTheme = output.head.asInstanceOf[SimpleEntity]
    val outputPTMs = outputTheme.getPTMs
    outputPTMs.size should be(0)

    val phosOutputPTMs = outputTheme.getPTMs("Phosphorylation")
    phosOutputPTMs.size should be(0)
  }

  //
  // Negation tests
  //

  val negPhos = "Mek was not phosphorylated."
  negPhos should "have a negated SimpleEvent representation for the Phosphorylation mention" in {

    val doc = createDoc(negPhos, "assembly-test")

    val mentions = testReach.extractFrom(doc)

    val am = AssemblyManager(mentions)

    val m = mentions.filter(_ matches "Phosphorylation").head

    val phos = am.getSimpleEvent(m)

    phos.negated should be(true)
  }

  val negReg = "Mek was not phosphorylated by Ras."
  negReg should "have a negated Regulation representation" in {

    val doc = createDoc(negReg, "assembly-test")

    val mentions = testReach.extractFrom(doc)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val m = mentions.filter(_ matches "Regulation").head

    val reg = am.getRegulation(m)

    reg.negated should be(true)
  }

  it should "not have a negated SimpleEvent representation for the Phosphorylation" in {
    val doc = createDoc(negReg, "assembly-test")

    val mentions = testReach.extractFrom(doc)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val m = mentions.filter(_ matches "Phosphorylation").head

    val phos = am.getSimpleEvent(m)

    phos.negated should be(false)
  }

  // test PrecedenceRelations

  it should "not have any precedence relations for the phosphorylation" in {
    val doc = createDoc(negPhos, "assembly-test")

    val mentions = testReach.extractFrom(doc)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val p = mentions.filter(_ matches "Phosphorylation").head

    // test AssemblyManager's methods
    am.predecessorsOf(p).size should be(0)
    am.distinctPredecessorsOf(p).size should be(0)
    am.successorsOf(p).size should be(0)
    am.distinctSuccessorsOf(p).size should be(0)

    val se = am.getSimpleEvent(p)

    // test Event methods
    se.predecessors.size should be(0)
    se.distinctPredecessors.size should be(0)
    se.successors.size should be(0)
    se.distinctSuccessors.size should be(0)
  }

  val precedenceText = "Ras is phosphorylated by Mek after Mek is bound to p53."

  precedenceText should "have a PrecedenceRelation showing the Phosphorylation following the Binding" in {

    val doc = createDoc(precedenceText, "assembly-test")

    val mentions = testReach.extractFrom(doc)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val p = mentions.filter(_ matches "Phosphorylation").head
    val b = mentions.filter(_ matches "Binding").head

    // test mention-based methods
    am.storePrecedenceRelation(p, b, "mention-based-test")

    am.getPrecedenceRelations(p).size should be(1)
    am.predecessorsOf(p).size should be(0)
    am.distinctPredecessorsOf(p).size should be(0)
    am.successorsOf(p).size should be(1)
    am.distinctSuccessorsOf(p).size should be(1)

    am.getPrecedenceRelations(b).size should be(1)
    am.predecessorsOf(b).size should be(1)
    am.distinctPredecessorsOf(b).size should be(1)
    am.successorsOf(b).size should be(0)
    am.distinctSuccessorsOf(b).size should be(0)

    // test eer-based methods
    val pSE = am.getSimpleEvent(p)
    val bSE = am.getSimpleEvent(b)

    // test mention-based methods
    am.storePrecedenceRelation(p, b, "mention-based-test")

    pSE.precedenceRelations.size should be(1)
    pSE.predecessors.size should be(0)
    pSE.distinctPredecessors.size should be(0)
    pSE.successors.size should be(1)
    pSE.distinctSuccessors.size should be(1)

    bSE.precedenceRelations.size should be(1)
    bSE.predecessors.size should be(1)
    bSE.distinctPredecessors.size should be(1)
    bSE.successors.size should be(0)
    bSE.distinctSuccessors.size should be(0)

    // test distinct v. non-distinct
    am.storePrecedenceRelation(p, b, "mention-based-test2")
    // this is a set, but the difference in
    // foundBy means "mention-based-test2" won't get collapsed
    pSE.precedenceRelations.size should be(2)
    pSE.predecessors.size should be(0)
    pSE.distinctPredecessors.size should be(0)
    pSE.successors.size should be(1)
    pSE.distinctSuccessors.size should be(1)
    // this is a set, but the difference in
    // foundBy means "mention-based-test2" won't get collapsed
    bSE.precedenceRelations.size should be(2)
    bSE.predecessors.size should be(1)
    bSE.distinctPredecessors.size should be(1)
    bSE.successors.size should be(0)
    bSE.distinctSuccessors.size should be(0)
  }

  "AssemblyManager" should s"not contain any EERs for '$negPhos' if all EERs referencing Mek are removed" in {
    val doc = createDoc(negPhos, "assembly-test")

    val mentions = testReach.extractFrom(doc)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val m = mentions.filter(_ matches "Entity").head

    am.removeEntriesContainingIDofMention(m)

    am.EERs.size should be(0)
  }

  it should "safely handle mentions in any order" in {
    val text = "EHT1864 inhibited AKT phosphorylation induced by LPA and S1P, but not EGF or PDGF"
    val doc = createDoc(text, "mention-order-test")
    val mentions = testReach.extractFrom(doc)

    val am1 = AssemblyManager(mentions)
    val am2 = AssemblyManager(mentions.sortBy(_.label))
    val am3 = AssemblyManager(mentions.sortBy(_.label).reverse)

    def hasEquivalentEERs(manager1: AssemblyManager, manager2: AssemblyManager): Boolean = {
      val eers1 = manager1.distinctEERs.map(_.equivalenceHash)
      val eers2 = manager2.distinctEERs.map(_.equivalenceHash)
      val entities1 = manager1.distinctEntities.map(_.equivalenceHash)
      val entities2 = manager2.distinctEntities.map(_.equivalenceHash)
      val events1 = manager1.distinctEvents.map(_.equivalenceHash)
      val events2 = manager2.distinctEvents.map(_.equivalenceHash)

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
