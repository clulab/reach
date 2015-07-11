package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.bionlp.mentions._
import TestUtils._

/**
 * Unit tests to ensure Regulation (both Pos and Neg) rules are matching correctly
 * Date: 5/19/15
 */
class TestRegulationEvents extends FlatSpec with Matchers {
  val sent1 = "Phosphorylation of ASPP2 by MAPK is required for RAS induced increased binding to p53 and increased transactivation of pro-apoptotic genes."
  sent1 should "have an up-regulated phosphorylation" in {
    val doc = testReach.mkDoc(sent1, "testdoc")
    val mentions = testReach extractFrom doc
    assert(hasPositiveRegulationByEntity("MAPK", "Phosphorylation", Seq("ASPP2", "MAPK"), mentions))
  }

  // there is an implicit regulation in the example text
  // it is the cause of the phosphorylation
  val sent2 = "The ubiquitinated Ras protein phosphorylates AKT."
  sent2 should "contain a regulation" in {
    val mentions = testReach.extractFrom(sent2, "testdoc", "1")
    val reg = mentions.find(_.label == "Positive_regulation")
    reg.isDefined should be (true)
    reg.get.arguments.contains("controlled") should be (true)
    reg.get.arguments.contains("controller") should be (true)
    reg.get.arguments("controlled").head.label == "Phosphorylation" should be (true)
    reg.get.arguments("controller").head.text.contains("Ras") should be (true)
  }

  val sent3 = "Interestingly, we observed two conserved putative MAPK phosphorylation sites in ASPP1 and ASPP2"
  sent3 should "contain 2 phosphorylations and 2 regulations" in {
    val mentions = parseSentence(sent3)
    hasEntity("MAPK", mentions) should be (true)
    hasEntity("ASPP1", mentions) should be (true)
    hasEntity("ASPP2", mentions) should be (true)

    hasEventWithArguments("Phosphorylation", List("ASPP1"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)

    hasPositiveRegulationByEntity("MAPK", "Phosphorylation", List("ASPP1"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent4 = "We thus tested whether RAS activation may regulate ASPP2 phosphorylation"
  sent4 should "contain 1 phosphorylation and no regulation" in {
    val mentions = parseSentence(sent4)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    // we don't do unspecified regulations anymore, only + and -
    hasPositiveRegulationByEntity("RAS", "Phosphorylation", List("ASPP2"), mentions) should be (false)
  }

  val sent5 = "MAPK1 was clearly able to phosphorylate the ASPP2 fragment in vitro"
  sent5 should "contain 1 regulation" in {
    val mentions = parseSentence(sent5)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent6 = "Under the same conditions, ASPP2 (693-1128) fragment phosphorylated by p38 SAPK had very low levels of incorporated 32P"
  sent6 should "contain 1 regulation" in {
    val mentions = parseSentence(sent6)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    // TODO: This is failing because we're missing SAPK in "p38 SAPK"; we only get p38, but we used to get "p38 SAPK"
    hasPositiveRegulationByEntity("p38 SAPK", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent7 = "The phosphorylated ASPP2 fragment by MAPK1 was digested by trypsin and fractioned on a high performance liquid chromatography."
  sent7 should "contain 1 regulation" in {
    val mentions = parseSentence(sent7)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent8 = "Hence ASPP2 can be phosphorylated at serine 827 by MAPK1 in vitro."
  sent8 should "contain 1 regulation" in {
    val mentions = parseSentence(sent8)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent10 = "ASPP1 fails to upregulate the phosphorylation of ASPP2."
  sent10 should "contains 1 regulation and 1 phosphorylation event" in {
    val mentions = parseSentence(sent10)
    // this matches over negative verbal triggers such as "fails"
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent11 = "ASPP1 fails to downregulate the phosphorylation of ASPP2."
  sent11 should "contains 1 downregulation and 1 phosphorylation event" in {
    val mentions = parseSentence(sent11)
    // this matches over negative verbal triggers such as "fails"
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent12 = "ASPP1 downregulates the phosphorylation of ASPP2."
  sent12 should "contains 1 downregulation and 1 phosphorylation event" in {
    val mentions = parseSentence(sent12)
    // this matches over negative verbal triggers such as "fails"
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent13 = "The inhibition of ASPP1 increases the phosphorylation of ASPP2."
  sent13 should "contain 1 downregulation and NO upregulation events" in {
    val mentions = parseSentence(sent13)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (false)
  }

  val sent14 = "the phosphorylation of ASPP2 is increased by the inhibition of ASPP1."
  sent14 should "contain 1 downregulation and NO upregulation events" in {
    val mentions = parseSentence(sent14, verbose = false)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (false)
  }

  val sent15 = "We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D)."
  sent15 should "contain 1 negative regulation and NO positive activation or regulation events" in {
    val mentions = parseSentence(sent15)
    hasNegativeRegulationByEntity("MEK", "Binding", List("ERBB3", "PI3K"), mentions) should be (true)
    mentions.filter(_.label == "Negative_activation") should have size (0)
    mentions.filter(_.label == "Positive_activation") should have size (0)
  }

  val sent16 = "the inhibition of ASPP1 decreases ASPP2 phosphorylation."
  sent16 should "contain 1 positive regulation, and NO negative regulations or activations" in {
    val mentions = parseSentence(sent16)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (false)
    mentions.filter(_.label.contains("activation")) should have size (0)
  }

  val sent17 = "ASPP1 is an activator of the ubiquitination of ASPP2"
  sent17 should "contain 1 positive regulation, and NO negative regulations or activations" in {
    val mentions = parseSentence(sent17)
    hasPositiveRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (false)
    mentions.filter(_.label.contains("activation")) should have size (0)
  }

  val sent18 = "ASPP1 is an inhibitor of the ubiquitination of ASPP2"
  sent18 should "contain 1 negative regulation, and NO positive regulations or activations" in {
    val mentions = parseSentence(sent18)
    hasNegativeRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (false)
    mentions.filter(_.label.contains("activation")) should have size (0)
  }

  val sent19 = "The phosphorylation of ASPP1 inhibits the ubiquitination of ASPP2"
  sent19 should "contain a controller with a PTM" in {
    val mentions = parseSentence(sent19)
    val reg = mentions.find(_ matches "Negative_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.text should be ("ASPP1")
    controller.modifications should have size (1)
    controller.modifications.head.label should be ("phosphorylated")
    controlled.labels should contain ("Ubiquitination")
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key ("cause")
    controlled.arguments("theme").head.text should be ("ASPP2")
  }

  val sent20 = "The binding of ASPP1 and ASPP2 promotes the phosphorylation of MEK"
  sent20 should "contain a controller with a complex" in {
    val mentions = parseSentence(sent20)
    val reg = mentions.find(_ matches "Positive_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.text should be ("ASPP1 and ASPP2")
    controller.labels should contain ("Complex")
    controlled.labels should contain ("Phosphorylation")
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key ("cause")
    controlled.arguments("theme").head.text should be ("MEK")
  }

  val sent21 = "Human deoxycytidine kinase is phosphorylated by ASPP2 on serine 128."
  sent21 should "contain exactly one positive regulation and one phosphorylation with site" in {
    val mentions = parseSentence(sent21)
    mentions.filter(_ matches "Positive_regulation") should have size (1)
    val reg = mentions.find(_ matches "Positive_regulation")
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.text should be ("ASPP2")
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key ("cause")
    controlled.arguments("theme").head.text should be ("deoxycytidine kinase")
    controlled.arguments should contain key ("site")
    controlled.arguments("site").head.text should be ("serine 128")
  }

  val sent22 = "Human deoxycytidine kinase is phosphorylated on serine 128 by ASPP2."
  sent22 should "contain exactly one positive regulation and one phosphorylation with site" in {
    val mentions = parseSentence(sent22)
    mentions.filter(_ matches "Positive_regulation") should have size (1)
    val reg = mentions.find(_ matches "Positive_regulation")
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.text should be("ASPP2")
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key("cause")
    controlled.arguments("theme").head.text should be("deoxycytidine kinase")
    controlled.arguments should contain key ("site")
    controlled.arguments("site").head.text should be("serine 128")
  }

  // a weird text from the PMC384* where we used to overmatch
  val sent23 = "histone 2B phosphorylated by p38 SAPK had high levels of incorporated 32P, suggesting that p38 SAPK was active; while under the same conditions, ASPP2 (693-1128) fragment"
  sent23 should "contain 1 phosphorylation and 1 positive regulation" in {
    val mentions = parseSentence(sent23)
    mentions.filter(_.label == "Positive_regulation") should have size (1)
    mentions.filter(_.label == "Phosphorylation") should have size (1)
  }

  val sent24 = "The binding of BS1 and BS2 promotes the phosphorylation of MEK"
  sent24 should "contain one positive regulation" in {
    val mentions = parseSentence(sent24)
    val posReg = mentions.filter(_ matches "Positive_regulation")
    posReg should have size (1)
    posReg.head.arguments should contain key ("controller")
    posReg.head.arguments should contain key ("controlled")
    posReg.head.arguments("controller") should have size (1)
    posReg.head.arguments("controlled") should have size (1)
    val controller = posReg.head.arguments("controller").head
    val controlled = posReg.head.arguments("controlled").head
    controller.matches("Complex") should be (true)
    controlled.matches("Phosphorylation") should be (true)
  }

  val sent25 = "ASPP1 aids in the translocation of Kras to the membrane"
  sent25 should "contain one positive regulation" in {
    val mentions = parseSentence(sent25)
    hasPositiveRegulationByEntity("ASPP1", "Translocation", List("Kras"), mentions) should be (true)
  }

/*  val sent26 = "oncogenic Kras promotes tumorigenesis by inducing expression of NRF2"
  sent26 should "contain 1 positive regulation and 1 transcription" in {
    val mentions = parseSentence(sent26)
    hasPositiveRegulationByEntity("Kras", "Transcription", List("NRF2"), mentions) should be (true)
  }*/

  val sent27 = "rapamycin blocked the serum-stimulated phosphorylation of ERK"
  sent27 should "contain one regulation controlled by rapamycin" in {
    val mentions = parseSentence(sent27)
    hasNegativeRegulationByEntity("rapamycin", "Phosphorylation", List("ERK"), mentions) should be (true)
  }

  val sent28 = "rapamycin inhibition of the phosphorylation of ERK"
  sent28 should "contain one regulation controlled by rapamycin" in {
    val mentions = parseSentence(sent28)
    hasNegativeRegulationByEntity("rapamycin", "Phosphorylation", List("ERK"), mentions) should be (true)
  }

  val sent29 = "B-Raf phosphorylates MEK1 and MEK2 on Ser217 and Ser221"
  sent29 should "contain 4 phosphorylations and 4 regulations (GUS)" in {
    val mentions = parseSentence(sent29)
    mentions.filter(_.label == "Positive_regulation") should have size (4)
    mentions.filter(_.label == "Phosphorylation") should have size (4)
  }

  val sent30 = "Note that only K650M and K650E-FGFR3 mutants cause STAT1 phosphorylation"
  sent30 should "contain 1 phospho and 2 pos reg" in {
    val mentions = parseSentence(sent30)
    mentions.filter(_.label == "Positive_regulation") should have size (2)
    mentions.filter(_.label == "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("FGFR3", "Phosphorylation", List("STAT1"), mentions) should be (true)
  }

  val sent31 = "Note that only K650M, K660M, and K650E-FGFR3 mutants cause STAT1 phosphorylation on Y123 and T546"
  sent31 should "contain 2 phospho and 6 pos reg" in {
    val mentions = parseSentence(sent31)
    mentions.filter(_.label == "Positive_regulation") should have size (6)
    mentions.filter(_.label == "Phosphorylation") should have size (2)
    hasPositiveRegulationByEntity("FGFR3", "Phosphorylation", List("STAT1", "Y123"), mentions) should be (true)
    hasPositiveRegulationByEntity("FGFR3", "Phosphorylation", List("STAT1", "T546"), mentions) should be (true)
  }

  val sent32 = "p53-phosphorylation of ERK"
  sent32 should "contain 1 phospho and 1 pos reg" in {
    val mentions = parseSentence(sent32)
    mentions.filter(_.label == "Positive_regulation") should have size (1)
    mentions.filter(_.label == "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("p53", "Phosphorylation", List("ERK"), mentions) should be (true)
  }

  val sent33 = "Phosphorylation of PIMT by the RAF/ERK2 complex potentiates Med1 dependent transcriptional activity."
  sent33 should "contain 1 binding, 1 phospho, and 1 pos reg controlled by the complex (GUS)" in {
    val mentions = parseSentence(sent33)
    // TODO: this fails because we pick 2 controllers (RAF and ERK2) instead of the complex
    mentions.filter(_.label == "Positive_regulation") should have size (1)
    mentions.filter(_.label == "Phosphorylation") should have size (1)
    mentions.filter(_.label == "Binding") should have size (1)
  }

  val sent34 = "ATR and ATM phosphorylate p53 at Ser37 and Ser46 , respectively"
  sent34 should "contain only 2 (not 4!) pos regulations because of \"respectively\"" in {
    val mentions = parseSentence(sent34)
    // TODO: this fails because we generate 4 regs instead of 2, as "respectively" should enforce
    mentions.filter(_.label == "Positive_regulation") should have size (2)
  }

  val sent35 = "p53 can be acetylated by p300 and CBP at multiple lysine residues ( K164 , 370 , 372 , 373 , 381 , 382 and 386 ) and by PCAF at K320 ."
  sent35 should  "contain 15 positive regulations due to the multiple controllers and multiple sites" in {
    val mentions = parseSentence(sent35)
    // TODO: hard. We should get 7 pos regs by p300, 7 by CBP, and 1 by PCAF
    // This is a great example for handling enumerations
    mentions.filter(_.label == "Positive_regulation") should have size (15)
  }

  val sent36 = "Taken together , these data suggest that decreased PTPN13 expression enhances EphrinB1 and Erk1 and phosphorylation in epithelial cells ."
  sent36 should "contain 2 negative regulations (not positive)" in {
    val mentions = parseSentence(sent36)
    // TODO: we incorrectly label the regs as positive because we miss "decreased"
    mentions.filter(_.label == "Negative_regulation") should have size (2)
  }

  val sent37 = "First , while the extent of PTPN13 knock-down was not very efficient , it was enough to increase EphrinB1 phosphorylation"
  sent37 should "contain 1 negative regulation (not positive)" in {
    val mentions = parseSentence(sent37)
    // TODO: very hard. we we incorrectly label the reg as positive because we miss "knock-down", but that is missed because of the coref by "it"
    mentions.filter(_.label == "Negative_regulation") should have size (1)
    hasNegativeRegulationByEntity("PTPN13", "Phosphorylation", List("EphrinB1"), mentions) should be (true)
  }

  val sent38 = "These data are consistent with EphrinB1 being a PTPN13 phosphatase substrate and suggest that decreased PTPN13 expression in BL breast cancer cell lines increases phosphorylation of EphrinB1 ."
  sent38 should "contain 1 negative regulation (not positive)" in {
    val mentions = parseSentence(sent38)
    // TODO: we incorrectly label the regs as positive because we miss "decreased"
    mentions.filter(_.label == "Negative_regulation") should have size (1)
  }

}
