package org.clulab.reach

import org.clulab.reach.TestUtils._
import org.clulab.reach.mentions.BioMention
import org.scalatest.{FlatSpec, Matchers}

/**
  * Unit tests based on the second round of feedback from MITRE
  * User: mihais
  * Date: 7/5/16
  */
class TestFeedback2 extends FlatSpec with Matchers {
  val s1 = "EGFR activated Ack1 which in turn Tyr phosphorylated and activated AKT"
  s1 should "NOT have AKT as a Controller in any event" in {
    // TODO: DANE
  }

  s1 should "NOT have Tyr labeled as a GGP" in {
    // TODO (for all major sites aminoacids): TOM, GUS please double check the site rules
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
    // TODO: disable activations when the Controlled dep path goes through the trigger of another event (e.g., "activation") - DANE, GUS
    // ms: maybe this not too bad? MITRE seems to prefer this!
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

  s7 should "contain 1 positive regulation" in {
    // TODO: need new reg pattern: "A following B activation", "A following activation by B" - MIHAI
  }

  val s8 = "p53–ASPP2 complex in these cells following RAS activation"
  s8 should "contain 1 binding and 1 positive regulation event" in {
    // TODO: matches the binding, but needs the above reg patterns - MIHAI
  }

  val s9 = "Moreover, an interaction was also observed between endogenous ASPP2 and HRASV12 in a human colon cancer cell line"
  s9 should "contain 1 binding event" in {
    // TODO: missing binding - MARCO
  }

  val s10 = "Akt phosphorylates Ser487 on AMPK-alpha1"
  s10 should "contain 1 phosphorylation event" in {
    // TODO: missing phospho - GUS, DANE
  }

  val s11 = "Prior phosphorylation of AMPK-alpha1 by Akt at Ser487"
  s11 should "contain 1 positive regulation" in {
    // TODO: we have the phospho, but miss the reg - GUS, DANE
  }

  val s12 = "Phosphorylation of Ser487 on AMPK-alpha1 by Akt in HEK-293 cells inhibits subsequent phosphorylation of Thr172"
  s12 should "contain at least 1 phosphorylation" in {
    // TODO: we miss both phosphos; we should get at least the 1st, which doesn't need coref - GUS, DANE
  }

  val s13 = "SAF-1 acts as a transcriptional inducer of H-Ras and K-Ras"
  s13 should "contain activations of genes not proteins" in {
    // TODO: we correctly get 2 activations, but H-Ras and K-Ras are labeled as proteins;
    //   they should be genes due to the "transcriptional" activity
    //   needs global NER - MARCO
  }

  val s14 = "We found that prolonged expression of active Ras resulted in up-regulation of the MKP3 gene via the PI3K/Akt pathway."
  s14 should "contain 1 activation of MKP3" in {
    // TODO: need new activation pattern: "A results in up-regulation of B" - MIHAI
  }

  s14 should "contain MKP3 marked as Gene" in {
    // TODO: needs global NER - MARCO
  }

  val s15 = "Up-regulation of MKP3 expression by active Ras expression"
  s15 should "contain 1 activation" in {
    // TODO: needs new activation pattern: "up-regulation of A by B" - MIHAI

  }

  val s16 = "We found that prolonged expression of active Ras resulted in up-regulation of the MKP3 gene via the PI3K/Akt pathway."
  s16 should "contain 1 activation pattern" in {
    // TODO: needs above activation pattern: "A results in up-regulation of B" - MIHAI
  }

  s16 should "contain MKP3 as a Gene" in {
    // TODO: needs global NER - MARCO
  }

  val s17 = "Here, we provide evidence that RhoA is phosphorylated by ERK on 88S and 100T"
  s17 should "contain phosphorylation at 2 sites" in {
    // TODO: we get the phospho but miss the sites - GUS, DANE
  }

  val s18 = "EGF treatment did not affect ROCK1 protein expression level; however, it increased MYPT1 phosphorylation on site 853."
  s18 should "contain 1 reg and 1 phospho at site 853" in {
    // TODO: we get both reg and phospho but miss the site - GUS, DANE
  }
}
