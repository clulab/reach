package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import org.clulab.reach.mentions._
import TestUtils._

/**
  * Unit tests to ensure Regulation (both Pos and Neg) rules are matching correctly
  * Date: 5/19/15
  * Last Modified: Update tests for issue #538.
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
    reg.get.arguments("controlled").head.asInstanceOf[BioEventMention].isDirect should be (true)
    reg.get.arguments("controller").head.text.contains("Ras") should be (true)
  }

  val sent3 = "Interestingly, we observed two conserved putative MAPK phosphorylation sites in ASPP1 and ASPP2"
  sent3 should "contain 2 phosphorylations and 2 regulations" in {
    val mentions = getBioMentions(sent3)
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
    val mentions = getBioMentions(sent4)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    // we don't do unspecified regulations anymore, only + and -
    hasPositiveRegulationByEntity("RAS", "Phosphorylation", List("ASPP2"), mentions) should be (false)
  }

  val sent5 = "MAPK1 was clearly able to phosphorylate the ASPP2 fragment in vitro"
  sent5 should "contain 1 regulation" in {
    val mentions = getBioMentions(sent5)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent6 = "Under the same conditions, ASPP2 (693-1128) fragment phosphorylated by AKT1 had very low levels of incorporated 32P"
  sent6 should "contain 1 regulation" in {
    val mentions = getBioMentions(sent6)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("AKT1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent7 = "The phosphorylated ASPP2 fragment by MAPK1 was digested by trypsin and fractioned on a high performance liquid chromatography."
  sent7 should "contain 1 regulation" in {
    val mentions = getBioMentions(sent7)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent8 = "Hence ASPP2 can be phosphorylated at serine 827 by MAPK1 in vitro."
  sent8 should "contain 1 regulation" in {
    val mentions = getBioMentions(sent8)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("MAPK1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent10 = "ASPP1 fails to upregulate the phosphorylation of ASPP2."
  sent10 should "contains 1 regulation and 1 phosphorylation event" in {
    val mentions = getBioMentions(sent10)
    // this matches over negative verbal triggers such as "fails"
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent11 = "ASPP1 fails to downregulate the phosphorylation of ASPP2."
  sent11 should "contains 1 downregulation and 1 phosphorylation event" in {
    val mentions = getBioMentions(sent11)
    // this matches over negative verbal triggers such as "fails"
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent12 = "ASPP1 downregulates the phosphorylation of ASPP2."
  sent12 should "contains 1 downregulation and 1 phosphorylation event" in {
    val mentions = getBioMentions(sent12)
    // this matches over negative verbal triggers such as "fails"
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
  }

  val sent13 = "The inhibition of ASPP1 increases the phosphorylation of ASPP2."
  sent13 should "contain 1 downregulation and NO upregulation events" in {
    val mentions = getBioMentions(sent13)

    println("==========")
    for (mention <- mentions){
      println("-----------")
      println(s"sentence:${mention.sentenceObj.words.toList.mkString(" ")}")
      println(s"event text:${mention.text}")
      println(s"class:${mention.getClass}")
      scala.io.StdIn.readLine()
    }

    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (false)
  }

  val sent14 = "the phosphorylation of ASPP2 is increased by the inhibition of ASPP1."
  sent14 should "contain 1 downregulation and NO upregulation events" in {
    val mentions = getBioMentions(sent14, verbose = false)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (false)
  }

  val sent15 = "We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D)."
  sent15 should "contain 1 negative regulation and NO positive activation or regulation events" in {
    val mentions = getBioMentions(sent15)
    hasNegativeRegulationByEntity("MEK", "Binding", List("ERBB3", "PI3K"), mentions) should be (true)
    mentions.filter(_.label == "Negative_activation") should have size (0)
    mentions.filter(_.label == "Positive_activation") should have size (0)
  }

  val sent16 = "the inhibition of ASPP1 decreases ASPP2 phosphorylation."
  sent16 should "contain 1 positive regulation, and NO negative regulations or activations" in {
    val mentions = getBioMentions(sent16)
    hasPositiveRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Phosphorylation", List("ASPP2"), mentions) should be (false)
    mentions.filter(_.label.contains("activation")) should have size (0)
  }

  val sent17 = "ASPP1 is an activator of the ubiquitination of ASPP2"
  sent17 should "contain 1 positive regulation, and NO negative regulations or activations" in {
    val mentions = getBioMentions(sent17)
    hasPositiveRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasNegativeRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (false)
    mentions.filter(_.label.contains("activation")) should have size (0)
  }

  val sent18 = "ASPP1 is an inhibitor of the ubiquitination of ASPP2"
  sent18 should "contain 1 negative regulation, and NO positive regulations or activations" in {
    val mentions = getBioMentions(sent18)
    hasNegativeRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (true)
    hasPositiveRegulationByEntity("ASPP1", "Ubiquitination", List("ASPP2"), mentions) should be (false)
    mentions.filter(_.label.contains("activation")) should have size (0)
  }

  val sent19 = "The phosphorylation of ASPP1 inhibits the ubiquitination of ASPP2"
  sent19 should "contain a controller with a PTM" in {
    val mentions = getBioMentions(sent19)
    val reg = mentions.find(_ matches "Negative_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.label should equal ("Phosphorylation")
    controlled.labels should contain ("Ubiquitination")
    controlled.asInstanceOf[BioEventMention].isDirect should be (false)
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key ("cause")
    controlled.arguments("theme").head.text should be ("ASPP2")
  }

  val sent20 = "The binding of ASPP1 and ASPP2 promotes the phosphorylation of MEK"
  sent20 should "contain a controller with a complex" in {
    val mentions = getBioMentions(sent20)
    val reg = mentions.find(_ matches "Positive_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.label should equal ("Binding")
    controlled.labels should contain ("Phosphorylation")
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key ("cause")
    controlled.arguments("theme").head.text should be ("MEK")
    controlled.asInstanceOf[BioEventMention].isDirect should be (false)
  }

  val sent21 = "Human deoxycytidine kinase is phosphorylated by ASPP2 on serine 128."
  sent21 should "contain exactly one positive regulation and one phosphorylation with site" in {
    val mentions = getBioMentions(sent21)
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
    controlled.asInstanceOf[BioEventMention].isDirect should be (true)
  }

  val sent22 = "Human deoxycytidine kinase is phosphorylated on serine 128 by ASPP2."
  sent22 should "contain exactly one positive regulation and one phosphorylation with site" in {
    val mentions = getBioMentions(sent22)
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
    controlled.asInstanceOf[BioEventMention].isDirect should be (true)
  }

  // a weird text from the PMC384* where we used to overmatch
  val sent23 = "histone 2B phosphorylated by AKT1 had high levels of incorporated 32P, suggesting that AKT1 was active; while under the same conditions, ASPP2 (693-1128) fragment"
  sent23 should "contain 1 phosphorylation and 1 positive regulation" in {
    val mentions = getBioMentions(sent23)
    mentions.filter(_.label == "Positive_regulation") should have size (1)
    mentions.filter(_.label == "Phosphorylation") should have size (1)
  }

  val sent24 = "The binding of BS1 and BS2 promotes the phosphorylation of MEK"
  sent24 should "contain one positive regulation" in {
    val mentions = getBioMentions(sent24)
    val posReg = mentions.filter(_ matches "Positive_regulation")
    posReg should have size (1)
    posReg.head.arguments should contain key ("controller")
    posReg.head.arguments should contain key ("controlled")
    posReg.head.arguments("controller") should have size (1)
    posReg.head.arguments("controlled") should have size (1)
    val controller = posReg.head.arguments("controller").head.toBioMention
    val controlled = posReg.head.arguments("controlled").head.toBioMention
    controller.matches("Binding") should be (true)
    controlled.matches("Phosphorylation") should be (true)
    controlled.asInstanceOf[BioEventMention].isDirect should be (false)
  }

  val sent25 = "ASPP1 aids in the translocation of Kras to the membrane"
  sent25 should "contain one positive regulation" in {
    val mentions = getBioMentions(sent25)
    hasPositiveRegulationByEntity("ASPP1", "Translocation", List("Kras"), mentions) should be (true)
  }

/*  val sent26 = "oncogenic Kras promotes tumorigenesis by inducing expression of NRF2"
  sent26 should "contain 1 positive regulation and 1 transcription" in {
    val mentions = getBioMentions(sent26)
    hasPositiveRegulationByEntity("Kras", "Transcription", List("NRF2"), mentions) should be (true)
  }*/

  val sent27 = "rapamycin blocked the serum-stimulated phosphorylation of ERK"
  sent27 should "contain one regulation controlled by rapamycin" in {
    val mentions = getBioMentions(sent27)
    hasNegativeRegulationByEntity("rapamycin", "Phosphorylation", List("ERK"), mentions) should be (true)
  }

  val sent28 = "rapamycin inhibition of the phosphorylation of ERK"
  sent28 should "contain one regulation controlled by rapamycin" in {
    val mentions = getBioMentions(sent28)
    hasNegativeRegulationByEntity("rapamycin", "Phosphorylation", List("ERK"), mentions) should be (true)
  }

  val sent29 = "B-Raf phosphorylates MEK2 and MEK1 on Ser221 and Ser217"
  sent29 should "contain 4 phosphorylations and 4 regulations (GUS)" in {
    val mentions = getBioMentions(sent29)
    mentions.filter(_.label == "Positive_regulation") should have size (4)
    mentions.filter(_.label == "Phosphorylation") should have size (4)
  }

  val sent30 = "Note that only K650M and K650E-FGFR3 mutants cause STAT1 phosphorylation"
  sent30 should "contain 1 phospho and 2 pos reg" in {
    val mentions = getBioMentions(sent30)
    mentions.filter(_.label == "Positive_regulation") should have size (2)
    mentions.filter(_.label == "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("FGFR3", "Phosphorylation", List("STAT1"), mentions) should be (true)
  }

  val sent31 = "Note that only K650M, K660M, and K650E-FGFR3 mutants cause STAT1 phosphorylation on Y123 and T546"
  sent31 should "contain 2 phospho and 6 pos reg" in {
    val mentions = getBioMentions(sent31)
    mentions.filter(_.label == "Positive_regulation") should have size (6)
    mentions.filter(_.label == "Phosphorylation") should have size (2)
    hasPositiveRegulationByEntity("FGFR3", "Phosphorylation", List("STAT1", "Y123"), mentions) should be (true)
    hasPositiveRegulationByEntity("FGFR3", "Phosphorylation", List("STAT1", "T546"), mentions) should be (true)
  }

  val sent32 = "p53-phosphorylation of ERK"
  sent32 should "contain 1 phospho and 1 pos reg" in {
    val mentions = getBioMentions(sent32)
    mentions.filter(_.label == "Positive_regulation") should have size (1)
    mentions.filter(_.label == "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("p53", "Phosphorylation", List("ERK"), mentions) should be (true)
  }

  // Unrelated to coref
  /*val sent33 = "Phosphorylation of PIMT by the RAF/ERK2 complex potentiates Med1 dependent transcriptional activity."
  sent33 should "contain 1 binding, 1 phospho, and 1 pos reg controlled by the complex (GUS)" in {
    val mentions = getBioMentions(sent33)
    // TODO: this fails because we pick 2 controllers (RAF and ERK2) instead of the complex
    mentions.filter(_.label == "Positive_regulation") should have size (1)
    mentions.filter(_.label == "Phosphorylation") should have size (1)
    mentions.filter(_.label == "Binding") should have size (1)
  }*/

  // Unrelated to coref...
  /*val sent34 = "ATR and ATM phosphorylate p53 at Ser37 and Ser46 , respectively"
  sent34 should "contain only 2 (not 4!) pos regulations because of \"respectively\"" in {
    val mentions = getBioMentions(sent34)
    // TODO: this fails because we generate 4 regs instead of 2, as "respectively" should enforce
    mentions.filter(_.label == "Positive_regulation") should have size (2)
  }*/

  val sent35 = "p53 can be acetylated by p300 and CBP at multiple lysine residues ( K164 , 370 , 372 , 373 , 381 , 382 and 386 ) ."
  sent35 should  "contain 16 positive regulations due to the multiple controllers and multiple sites" in {
    val mentions = getBioMentions(sent35)
    // TODO: needs coref for sites ("residues"). We should get 7 pos regs by p300 and 7 by CBP
    // This is a great example for handling enumerations
    mentions.filter(_.label == "Positive_regulation") should have size (16)
  }

  val sent36 = "Taken together , these data suggest that decreased PTPN13 expression enhances EphrinB1 and Erk1 and phosphorylation in epithelial cells ."
  sent36 should "contain 2 negative regulations (not positive)" in {
    val mentions = getBioMentions(sent36)
    // this tests that we capture amods for elements in the paths; necessary to correctly model semantic negatives
    mentions.filter(_.label == "Negative_regulation") should have size (2)
  }

  /*val sent37 = "First , while the extent of PTPN13 knock-down was not very efficient , it was enough to increase EphrinB1 phosphorylation"
  sent37 should "contain 1 negative regulation (not positive)" in {
    val mentions = getBioMentions(sent37)
    // TODO: very hard. we we incorrectly label the reg as positive because we miss "knock-down", but that is missed because of the coref by "it"
    mentions.filter(_.label == "Negative_regulation") should have size (1)
    hasNegativeRegulationByEntity("PTPN13", "Phosphorylation", List("EphrinB1"), mentions) should be (true)
  }*/

  val sent38 = "These data are consistent with EphrinB1 being a PTPN13 phosphatase substrate and suggest that decreased PTPN13 expression in BL breast cancer cell lines increases phosphorylation of EphrinB1 ."
  sent38 should "contain 1 negative regulation (not positive)" in {
    val mentions = getBioMentions(sent38)
    // this tests that we capture amods for elements in the paths; necessary to correctly model semantic negatives
    mentions.filter(_.label == "Negative_regulation") should have size (1)
  }

  // Problematic because of inclusion of "combination of rapamycin"; "rapamycin is found as the cause
  /*  val sent39 = "inhibition of ERK phosphorylation by combination of rapamycin"
  sent39 should "contain 1 negative regulation and 1 phosphorylation" in {
    val mentions = getBioMentions(sent39)
    mentions.filter(_ matches "Negative_regulation") should have size (1)
    mentions.filter(_ matches "Positive_regulation") should have size (0)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
  }*/

  val sent40 = "GSK-3 promotes the pathway by phosphorylating LRP5"
  ignore should "contain 1 positive regulation and 1 phosphorylation" in {
    val mentions = getBioMentions(sent40)
    mentions.filter(_ matches "Negative_regulation") should have size (0)
    mentions.filter(_ matches "Positive_regulation") should have size (1)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("GSK-3", "Phosphorylation", List("LRP5"), mentions) should be (true)
  }

  val sent41 = "Our model, in which E2-induced SRC-3 phosphorylation occurs in a complex with ER"
  sent41 should "contain 1 positive regulation and 1 phosphorylation" in {
    val mentions = getBioMentions(sent41)
    mentions.filter(_ matches "Positive_regulation") should have size (1)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("E2", "Phosphorylation", List("SRC-3"), mentions) should be (true)
  }

  val sent42 = "Cells expressing ErbB3 show tyrosine phosphorylation in response to treatment with RAS"
  sent42 should "contain 1 positive regulation and 1 phosphorylation" in {
    val mentions = getBioMentions(sent42)
    mentions.filter(_ matches "Positive_regulation") should have size (1)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("RAS", "Phosphorylation", List("ErbB3"), mentions) should be (true)
  }

  val sent43 = "Cells expressing ErbB3 show tyrosine phosphorylation in response to RAS treatment"
  sent43 should "contain 1 positive regulation and 1 phosphorylation" in {
    val mentions = getBioMentions(sent43)
    mentions.filter(_ matches "Positive_regulation") should have size (1)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("RAS", "Phosphorylation", List("ErbB3"), mentions) should be (true)
  }

  val sent44 = "Cells expressing ErbB3 show tyrosine phosphorylation in response to RAS inhibition"
  sent44 should "contain 1 negative regulation and 1 phosphorylation" in {
    val mentions = getBioMentions(sent44)
    mentions.filter(_ matches "Negative_regulation") should have size (1)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasNegativeRegulationByEntity("RAS", "Phosphorylation", List("ErbB3"), mentions) should be (true)
  }

  val sent45 = "Together these data demonstrate that E2-induced SRC-3 phosphorylation is dependent on a direct interaction between SRC-3 and ERalpha and can occur outside of the nucleus."
  sent45 should "contain 1 phosphorylation, 1 positive regulation, and 1 binding" in {
    val mentions = getBioMentions(sent45)
    mentions.filter(_ matches "Positive_regulation") should have size (1)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
    mentions.filter(_ matches "Binding") should have size (1)
    hasPositiveRegulationByEntity("E2", "Phosphorylation", List("SRC-3"), mentions) should be (true)
    hasEventWithArguments("Binding", List("SRC-3", "ERalpha"), mentions) should be (true)
  }

  val sent46 = "Akt inhibits the phosphorylation of AFT by BEF."
  sent46 should "contain a regulation of a regulation" in {
    val mentions = getBioMentions(sent46)
    val inner = mentions.filter(_ matches "Positive_regulation")
    val outer = mentions.filter(_ matches "Negative_regulation")
    inner should have size (1)
    outer should have size (1)
    hasPositiveRegulationByEntity("BEF", "Phosphorylation", List("AFT"), mentions) should be (true)
    outer.head.arguments("controlled").head == inner.head should be (true)
  }

  val sent47 = "The phosphorylation of AFT by BEF is inhibited by the ubiquitination of Akt."
  sent47 should "contain a regulation of a regulation" in {
    val mentions = getBioMentions(sent47)
    val inner = mentions.filter(_ matches "Positive_regulation")
    val outer = mentions.filter(_ matches "Negative_regulation")
    inner should have size (1)
    outer should have size (1)
    hasPositiveRegulationByEntity("BEF", "Phosphorylation", List("AFT"), mentions) should be (true)
    outer.head.arguments("controlled").head == inner.head should be (true)
  }

  // the next 6 tests cover the "in response to" regulation rules
  val sent48 = "We first assayed the ability of the endogenous EGFR to be tyrosine autophosphorylated in response to EGF"
  sent48 should "contain 1 PosReg of a phosphorylation" in {
    val mentions = getBioMentions(sent48)
    // mentions.filter(_ matches "Positive_regulation") should have size (1)
    // mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("EGF", "AutoPhosphorylation", List("EGFR"), mentions) should be (true)
  }

  val sent49 = "the ability of the exogenous ErbB3 receptor to be tyrosine phosphorylated in response to stimulation with either EGF or neuregulin (NRG)"
  sent49 should "contain 2 PosReg of a phosphorylation" in {
    val mentions = getBioMentions(sent49)
    mentions.filter(_ matches "Positive_regulation") should have size (2)
    mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("EGF", "Phosphorylation", List("ErbB3"), mentions) should be (true)
    hasPositiveRegulationByEntity("neuregulin", "Phosphorylation", List("ErbB3"), mentions) should be (true)
  }

  val sent50 = "Both Gab1 and Gab1 F446/472/589 are tyrosine phosphorylated in response to EGF treatment"
  sent50 should "contain 2 PosReg of 2 phosphorylation" in {
    val mentions = getBioMentions(sent50)
    mentions.filter(_ matches "Positive_regulation") should have size (2)
    mentions.filter(_ matches "Phosphorylation") should have size (2)
    hasPositiveRegulationByEntity("EGF", "Phosphorylation", List("Gab1"), mentions) should be (true)
  }

  val sent51 = "The endogenous EGFR is tyrosine phosphorylated in response to EGF in all cell lines."
  sent51 should "contain 1 PosReg of 1 phosphorylation" in {
    val mentions = getBioMentions(sent51)
    // mentions.filter(_ matches "Positive_regulation") should have size (1)
    // mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("EGF", "Phosphorylation", List("EGFR"), mentions) should be (true)
  }

  val sent52 = "As shown in Figure, the endogenous Gab1 present in WT MEFs is tyrosine phosphorylated in response to EGF treatment."
  sent52 should "contain 1 PosReg of 1 phosphorylation" in {
    val mentions = getBioMentions(sent52)
    // mentions.filter(_ matches "Positive_regulation") should have size (1)
    // mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("EGF", "Phosphorylation", List("Gab1"), mentions) should be (true)
  }

  val sent53 = "We first assayed the ability of the mutant Gab1 proteins to become tyrosine phosphorylated in response to EGF."
  sent53 should "contain 1 PosReg of 1 phosphorylation" in {
    val mentions = getBioMentions(sent53)
    // mentions.filter(_ matches "Positive_regulation") should have size (1)
    // mentions.filter(_ matches "Phosphorylation") should have size (1)
    hasPositiveRegulationByEntity("EGF", "Phosphorylation", List("Gab1"), mentions) should be (true)
  }

  // From MITRE's feedback2, 2016 summer eval
  // These 4 tests cover "A following B activation" patterns
  val sent54 = "The phosphorylation of AKT1 following MEK activation."
  sent54 should "contain 1 positive regulation" in {
    val mentions = getBioMentions(sent54)
    hasPositiveRegulationByEntity("MEK", "Phosphorylation", List("AKT1"), mentions) should be (true)   // fails
  }
  val sent54b = "We observed the phosphorylation of AKT1 following activation by MEK."
  sent54b should "contain 1 positive regulation" in {
    val mentions = getBioMentions(sent54b)
    hasPositiveRegulationByEntity("MEK", "Phosphorylation", List("AKT1"), mentions) should be (true)  // fails
  }
  val sent54c = "The phosphorylation of AKT1 following inhibition of MEK."
  sent54c should "contain 1 negative regulation" in {
    val mentions = getBioMentions(sent54c)
    hasNegativeRegulationByEntity("MEK", "Phosphorylation", List("AKT1"), mentions) should be (true)
  }
  val sent54d = "p53–ASPP2 complex in these cells following RAS activation"
  sent54d should "contain 1 binding and 1 positive regulation event" in {
    val mentions = getBioMentions(sent54d)
    hasPositiveRegulationByEntity("RAS", "Binding", List("p53", "ASPP2"), mentions) should be (true)    // fails
  }

  val sent55 = "Apoptosis promotes the phosphorylation of p53."
  sent55 should "contain no regulations" in {
    val mentions = getBioMentions(sent55)
    hasPositiveRegulationByEntity("Apoptosis", "Phosphorylation", List("p53"), mentions) should be (false)
  }

  val sent56 = "RAS1 activates AKT-induced apoptosis"
  sent56 should "contain 1 activation and 1 positive regulation of that activation" in {
    val mentions = getBioMentions(sent56)
    hasPositiveRegulationByEntity("RAS1", "Positive_activation", List("AKT", "apoptosis"), mentions) should be (true)
  }

  val sent57 = "Indeed, expression of RARbeta2 has been shown to restore retinoic acid induced apoptosis"
  sent57 should "contain 1 Transcription and 1 positive activation, and 1 positive regulation" in {
    val mentions = getBioMentions(sent57)
    mentions.filter(_ matches "Transcription") should have size (1)
    mentions.filter(_ matches "Positive_activation") should have size (1)
    mentions.filter(_ matches "Positive_regulation") should have size (1)
  }

  val sent58 = "We observed increased ERBB3 binding to PI3K following MEK inhibition (Figure 1D), and accordingly, MEK inhibition substantially increased tyrosine phosphorylated ERBB3 levels (Figure 1A)."
  sent58 should "contain 1 amount, 1 binding, and 2 negative regulations" in {
    val mentions = getBioMentions(sent58)
    mentions.filter(_.label.contains("Amount")) should have size (1)
    mentions.filter(_.label.contains("Binding")) should have size (1)
    mentions.filter(_.label.contains("Negative_regulation")) should have size (2)
    mentions.filter(_.label.contains("Negative_activation")) should have size (0)
  }

  val sent59 = "Up-regulation of MKP3 expression by active Ras expression"
  sent59 should "contain 1 positive regulation and 2 transcriptions" in {
    val mentions = getBioMentions(sent59)
    mentions.filter(_.label.contains("Transcription")) should have size (2)
    mentions.filter(_.label.contains("Positive_regulation")) should have size (1)
    mentions.filter(_.label.contains("Positive_activation")) should have size (0)
  }

  val sent60 = "ATP reduced GSH depletion"
  sent60 should "recognize depletion as a positive activation" in {
    val mentions = getBioMentions(sent60)
    mentions.filter(_.label == "Positive_activation") should have size (1)
  }

  val sent61 = "ATP can deplete GSH in cells"
  sent61 should "recognize deplete as a negative activation" in {
    val mentions = getBioMentions(sent61)
    mentions.filter(_.label == "Negative_activation") should have size (1)
  }

  val sent62 = "ATP depletes GSH rapidly in cells"
  sent62 should "recognize depletes as a negative activation" in {
    val mentions = getBioMentions(sent62)
    mentions.filter(_.label == "Negative_activation") should have size (1)
  }

}
