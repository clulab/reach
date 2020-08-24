package org.clulab.reach.assembly

import org.clulab.reach.assembly.representations._
import org.scalatest.{FlatSpec, Matchers}
import org.clulab.reach.TestUtils._


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
  val mentions1 = getMentionsFromText(text1)
  val mentions2 = getMentionsFromText(text2)
  val mentions3 = getMentionsFromText(text3)
  val mentions4 = getMentionsFromText(text4)
  val mentions5 = getMentionsFromText(text5)
  val mentions6 = getMentionsFromText(text6)
  // translocations
  val mentions7 = getMentionsFromText(text7)

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

    val mentions = getMentionsFromText(dePhos)

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

    val mentions = getMentionsFromText(regText1)

    val am = AssemblyManager(mentions)

    am.getRegulations should have size (1)
    am.distinctRegulations should have size (1)
  }

  val regText2 = "Akt inhibits the phosphorylation of AFT by BEF."
  regText2 should "produce two Regulation representations (one with nesting)" in {

    val mentions = getMentionsFromText(regText2)

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
  negPhos should "have a negated SimpleEvent representation for the Phosphorylation mention" in {

    val mentions = getMentionsFromText(negPhos)

    val am = AssemblyManager(mentions)

    val m = mentions.filter(_ matches "Phosphorylation").head

    val phos = am.getSimpleEvent(m)

    phos.negated should be(true)
  }

  val negReg = "Mek was not phosphorylated by Ras."
  negReg should "have a negated Regulation representation" in {

    val mentions = getMentionsFromText(negReg)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val m = mentions.filter(_ matches "Regulation").head

    val reg = am.getRegulation(m)

    reg.negated should be(true)
  }

  it should "not have a negated SimpleEvent representation for the Phosphorylation" in {

    val mentions = getMentionsFromText(negReg)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val m = mentions.filter(_ matches "Phosphorylation").head

    val phos = am.getSimpleEvent(m)

    phos.negated should be(false)
  }

  // test PrecedenceRelations

  it should "not have any precedence relations for the phosphorylation" in {

    val mentions =  getMentionsFromText(negReg)

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

    val mentions =  getMentionsFromText(precedenceText)

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

    val mentions =  getMentionsFromText(negPhos)

    val am = AssemblyManager()

    am.trackMentions(mentions)

    val m = mentions.filter(_ matches "Entity").head

    am.removeEntriesContainingIDofMention(m)

    am.EERs should have size (0)
  }

  it should "safely handle mentions in any order" in {
    val text = "EHT1864 inhibited AKT phosphorylation induced by LPA and S1P, but not EGF or PDGF"
    val mentions = getMentionsFromText(text)

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
