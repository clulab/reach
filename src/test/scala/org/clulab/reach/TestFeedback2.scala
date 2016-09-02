package org.clulab.reach

import org.clulab.reach.TestUtils._
import org.clulab.reach.mentions._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Unit tests based on the second round of feedback from MITRE
  * User: mihais
  * Date: 7/5/16
  */
class TestFeedback2 extends FlatSpec with Matchers {
  val s1 = "EGFR activated Ack1, which in turn Tyr phosphorylated and activated AKT"
  // val s1 = "EGFR activated Ack1 which in turn Tyr phosphorylated and activated AKT" NB: This parses horribly.
  s1 should "NOT have AKT as a Controller in any event" in {
    val mentions = getBioMentions(s1)
    mentions.filter(m => m.text == "AKT") should have size (1)
    mentions.filter(m => m.arguments.getOrElse("controller", Nil).map(_.text) contains "AKT") should have size (0)
  }

  s1 should "NOT have Tyr labeled as a GGP" in {
    val mentions = getBioMentions(s1)
    val tyr = mentions.filter(_.text == "Tyr")
    tyr.head.label should equal ("Site")
    tyr.head.label should not equal ("Gene_or_gene_product")
  }

  val s2 = "Cells were additionally stimulated with 10 ng/ml leptin and cell extracts analyzed for ErbB3 tyrosine phosphorylation."
  s2 should "contain a regulation with leptin as controller" in {
    val mentions = getMentionsFromText(s2)
    val regs = mentions.filter(_ matches "Regulation")
    regs should not be empty
    regs should have size 1
    regs.head.arguments("controller").head.text should equal ("leptin")
    regs.head.arguments("controlled").head.label should equal ("Phosphorylation")
    regs.head.arguments("controlled").head.text should equal ("ErbB3 tyrosine phosphorylation")
  }

  val s3 = "Gab1 mutant protein deficient in Shp2 binding enhances EGF-induced activation of the PI-3"
  s3 should "NOT contain Activation(Gab1, EGF)" in {
    val mentions = getBioMentions(s3)
    hasPositiveRegulationByEntity("Gab1", "Positive_activation", List("EGF", "PI-3"), mentions) should be (true)
  }

  val s4 = "ASPP1 and ASPP2 cooperate with RAS to enhance the transcriptional activity of p53"
  s4 should "contain two activations with p53 as Controlled" in {
    // this is just to make sure that the above 2 tests don't break valid activations
    val mentions = getBioMentions(s4)
    mentions.filter(_.label == "Positive_activation") should have size (2)
  }

  val s5 = "We also demonstrate that tyrosine phosphorylation of ErbB3 may lead to recruitment and activation of PI-3 kinase"
  s5 should "contain an activation with a Phosphorylation event serving as Controller" in {
    // we should allow events to serve as controllers in activations
    val mentions = getMentionsFromText(s5)
    val acts = mentions.filter(_ matches "Positive_activation")
    acts should have size 2 // one for ComplexEvent controller and another for SimpleEvent controller
    acts.forall{
      act =>
        val controller = act.arguments("controller").head
        controller.label == "Phosphorylation"
    } should be (true)
    // text for controlled should be AKT in all cases
    acts.forall(act => act.arguments("controlled").head.text == "PI-3") should be (true)
    // test controller flattening ( e.g., Ras_p) for output
    val flattenedMentions: Seq[BioMention] = getMentionsForFriesOutput(s5)
    val flattenedActs = flattenedMentions.filter(_ matches "Positive_activation")
    flattenedActs should have size 1
    val fa = flattenedActs.head
    fa.arguments("controller") should have size 1
    val faController = fa.arguments("controller").head
    faController.text should equal ("ErbB3")
    // check for Phosphorylation PTM
    val ptms = faController.ptms
    ptms should have size 1
    ptms.head.label should equal ("Phosphorylation")
  }

  val s6 = "These results imply that Ack1 mediated Ras phosphorylation results in subsequent AKT activation."
  s6 should "contain an activation with a PosReg(Phosphorylation) event serving as Controller" in {
    val mentions = getMentionsFromText(s6)
    // we should allow regulation events to serve as controllers in activations
    val acts = mentions.filter(_ matches "Positive_activation")
    acts should have size 2 // one for ComplexEvent controller and another for SimpleEvent controller
    acts.exists{
      act =>
        val controller = act.arguments("controller").head
        controller.text == "Ack1 mediated Ras phosphorylation" && controller.label == "Positive_regulation"
    } should be (true)

    acts.exists{
      act =>
        val controller = act.arguments("controller").head
        controller.text == "Ras phosphorylation" && controller.label == "Phosphorylation"
    } should be (true)
    // text for controlled should be AKT in all cases
    acts.forall(act => act.arguments("controlled").head.text == "AKT") should be (true)
    // test controller flattening ( e.g., Ras_p) for output
    val flattenedMentions: Seq[BioMention] = getMentionsForFriesOutput(s6)
    val flattenedActs = flattenedMentions.filter(_ matches "Positive_activation")
    flattenedActs should have size 1 // Flattening controllers (and filtering again) treats the previous two activations as the same
    val fa = flattenedActs.head
    fa.arguments("controller") should have size 1
    val faController = fa.arguments("controller").head
    faController.text should equal ("Ras")
    // check for Phosphorylation PTM
    val ptms = faController.ptms
    ptms should have size 1
    ptms.head.label should equal ("Phosphorylation")
  }

  val s7 = "We observed that endogenous ASPP2 translocates from cell/cell junctions to the cytosol/nucleus following RAS activation"
  s7 should "contain 2 translocation events" in {
    // TODO: we should pick 2 translocation events here - ENRIQUE
    // ENRIQUE: Cell is in the black list of the NER, so there will be only two events
    val mentions = getBioMentions(s7)

    hasEventWithArguments("Translocation", List("ASPP2", "cell junctions", "cytosol"), mentions) should be (true)
    hasEventWithArguments("Translocation", List("ASPP2", "cell junctions", "nucleus"), mentions) should be (true)
  }

  val s9 = "Moreover, an interaction was also observed between endogenous ASPP2 and HRASV12 in a human colon cancer cell line"
  s9 should "contain 1 binding event" in {
    // TODO: missing binding - MARCO
  }

  val s10 = "Akt phosphorylates Ser487 on AMPK-alpha1"
  s10 should "contain 1 phosphorylation event" in {
    val mentions = getBioMentions(s10)
    hasEventWithArguments("Phosphorylation", Seq("AMPK-alpha1", "Ser487"), mentions) should be (true)
    hasPositiveRegulationByEntity("Akt", "Phosphorylation", Seq("AMPK-alpha1", "Ser487"), mentions) should be (true)
  }

  val s11 = "Prior phosphorylation of AMPK-alpha1 by Akt at Ser487"
  s11 should "contain 1 positive regulation" in {
    val mentions = getBioMentions(s11)
    val phos = mentions.filter(_ matches "Phosphorylation")
    phos should have size (1)
    val aphos = phos.head
    aphos.arguments.getOrElse("theme", Nil).map(_.text) should contain ("AMPK-alpha1")
    aphos.arguments.getOrElse("site", Nil).map(_.text) should contain ("Ser487")
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    reg.arguments.getOrElse("controller", Nil).map(_.text) should contain ("Akt")
    reg.arguments.getOrElse("controlled", Nil) should contain (aphos)
  }

  val s12 = "Phosphorylation of Ser487 on AMPK-alpha1 by Akt in HEK-293 cells inhibits subsequent phosphorylation of Thr172"
  s12 should "contain at least 1 phosphorylation" in {
    val mentions = getBioMentions(s12)
    val phos = mentions.filter(_ matches "Phosphorylation")
    phos should have size (1) // TODO: Phosphorylation of Thr172 is on AMPK-alpha1
    val aphos = phos.head
    aphos.arguments.getOrElse("theme", Nil).map(_.text) should contain ("AMPK-alpha1")
    aphos.arguments.getOrElse("site", Nil).map(_.text) should contain ("Ser487")
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    reg.arguments.getOrElse("controller", Nil).map(_.text) should contain ("Akt")
    reg.arguments.getOrElse("controlled", Nil) should contain (aphos)
  }

  /* // ms: skipping these for now
  val s13 = "SAF-1 acts as a transcriptional inducer of H-Ras and K-Ras"
  s13 should "contain activations of genes not proteins" in {
    // TODO: we correctly get 2 activations, but H-Ras and K-Ras are labeled as proteins;
    //   they should be genes due to the "transcriptional" activity
    //   needs global NER - MARCO
  }

  val s14 = "We found that prolonged expression of active Ras resulted in up-regulation of the MKP3 gene via the PI3K/Akt pathway."
  s14 should "contain MKP3 marked as Gene" in {
    // TODO: needs global NER - MARCO
  }

  val s16 = "Up-regulation of MKP3 expression by active Ras expression"
  s16 should "contain MKP3 as a Gene" in {
    // TODO: needs global NER - MARCO
  }
  */

  val s17a = "Here, we provide evidence that RhoA is phosphorylated by ERK on 88S and 100T"
  s17a should "contain phosphorylation at 2 sites" in {
    val mentions = getBioMentions(s17a)
    val phos = mentions.filter(_ matches "Phosphorylation")
    phos should have size (2)
    val themes = phos.flatMap(_.arguments.getOrElse("theme", Nil)).map(_.text)
    themes should be (Seq("RhoA", "RhoA"))
    val sites = phos.flatMap(_.arguments.getOrElse("site", Nil)).map(_.text)
    sites should contain ("100T")
    sites should contain ("88S")
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (2)
    regs.flatMap(_.arguments.getOrElse("controller", Nil)).map(_.text) should be (Seq("ERK", "ERK"))
  }

  // See Issue 258
  val s17b = "Here, we provide evidence that RhoA is phosphorylated by ERK on 88 S and 100 T."
  s17b should "contain phosphorylation at 2 sites" in {
    val mentions = getBioMentions(s17b)
    val phos = mentions.filter(_ matches "Phosphorylation")
    phos should have size (2)
    val themes = phos.flatMap(_.arguments.getOrElse("theme", Nil)).map(_.text)
    themes should be (Seq("RhoA", "RhoA"))
    val sites = phos.flatMap(_.arguments.getOrElse("site", Nil)).map(_.text)
    sites should contain ("100 T.")
    sites should contain ("88 S")
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (2)
    regs.flatMap(_.arguments.getOrElse("controller", Nil)).map(_.text) should be (Seq("ERK", "ERK"))
  }

  val s18 = "EGF treatment did not affect ROCK1 protein expression level; however, it increased MYPT1 phosphorylation on site 853."
  s18 should "contain 1 reg and 1 phospho at site 853" in {
    val mentions = getBioMentions(s18)
    val phos = mentions.filter(_ matches "Phosphorylation")
    phos should have size (1)
    val mphos = phos.head
    mphos.arguments.getOrElse("theme", Nil).map(_.text) should contain ("MYPT1")
    mphos.arguments.getOrElse("site", Nil).map(_.text) should contain ("853")
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    reg.arguments.getOrElse("controller", Nil).map(m => m.toCorefMention.antecedentOrElse(m).text) should contain ("EGF")
    reg.arguments.getOrElse("controlled", Nil) should contain (mphos)
  }
}
