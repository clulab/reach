package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import org.clulab.reach.mentions._
import TestUtils._

/**
 * Unit tests to ensure Activation event rules are matching correctly
 * User: mihais
 * Date: 5/19/15
 */
class TestActivationEvents extends FlatSpec with Matchers {
  val sent1 = "Nucleotide free Ras inhibits PI3KC2Beta activity."
  val sent1b = "Nucleotide free Ras inhibits PI3KC2Beta."
  val sent1c = "Nucleotide free Ras inhibits activation of PI3KC2Beta."
  val sent1d = "Addition of Ras inhibits PI3KC2Beta."
  val sent1e = "Increase of Ras dose inhibits PI3KC2Beta."
  sent1 should "contain negative activation patterns" in {
    var mentions = getBioMentions(sent1)
    mentions.filter(_.label == "Negative_activation") should have size (1)

    mentions = getBioMentions(sent1b)
    mentions.filter(_.label == "Negative_activation") should have size (1)

    mentions = getBioMentions(sent1c)
    mentions.filter(_.label == "Negative_activation") should have size (1)

    mentions = getBioMentions(sent1d)
    mentions.filter(_.label == "Negative_activation") should have size (1)

    mentions = getBioMentions(sent1e)
    mentions.filter(_.label == "Negative_activation") should have size (1)
  }

  val sent2 = "Ubiquitinated Ras activates Raf and PI3K."
  val sent2b = "Ubiquitinated Ras increases Raf and PI3K activity."
  sent2 should "contain multiple different positive activations" in {
    var mentions = getBioMentions(sent2)
    mentions.filter(_.label == "Positive_activation") should have size (2)

    mentions = getBioMentions(sent2)
    mentions.filter(_.label == "Positive_activation") should have size (2)
  }

  val sent3 = "the phosphorylation of Ras promotes the ubiquitination of MEK"
  sent3 should "contain NO activation events, and a single positive regulation" in {
    val mentions = getBioMentions(sent3)
    mentions.filter(_.label == "Positive_activation") should have size (0)
    mentions.filter(_.label == "Positive_regulation") should have size (1)
  }

  val sent5 = "the suppression of ASPP1 decreases ASPP2."
  sent5 should "contain 1 positive activation and NO negative activation or regulation events" in {
    val mentions = getBioMentions(sent5)
    hasNegativeActivation("ASPP1", "ASPP2", mentions) should be(false)
    hasPositiveActivation("ASPP1", "ASPP2", mentions) should be(true)
    mentions.filter(_.label.contains("regulation")) should have size (0)
  }

  val sent6 = "ASPP1 is an activator of ASPP2"
  sent6 should "contain 1 positive activation event" in {
    val mentions = getBioMentions(sent6)
    hasNegativeActivation("ASPP1", "ASPP2", mentions) should be(false)
    hasPositiveActivation("ASPP1", "ASPP2", mentions) should be(true)
    mentions.filter(_.label.contains("regulation")) should have size (0)
  }

  val sent7 = "ASPP1 is an inhibitor of ASPP2"
  sent7 should "contain 1 negative activation event" in {
    val mentions = getBioMentions(sent7)
    hasNegativeActivation("ASPP1", "ASPP2", mentions) should be(true)
    hasPositiveActivation("ASPP1", "ASPP2", mentions) should be(false)
    mentions.filter(_.label.contains("regulation")) should have size (0)
  }

  val sent8 = "The ASPP2-binding activity of CREB is, in most cases, constitutive."
  sent8 should "contain a binding but not an activation or regulation event" in {
    val mentions = getBioMentions(sent8)
    mentions.filter(_.label.contains("activation")) should have size (0)
    mentions.filter(_.label.contains("regulation")) should have size (0)
    hasEventWithArguments("Binding", List("ASPP2", "CREB"), mentions) should be(true)
  }

  /*  val sent9 = "HOXB7 overexpression induced a decrease of c-FOS"
  sent9 should "contain 1 negative activation and 0 positive ones" in {
    val mentions = getBioMentions(sent9)
    mentions.filter(_.label.contains("Transcription")) should have size (1)
    mentions.filter(_.label.contains("Negative_activation")) should have size (1)
  }*/

  val sent10 = "The suppression of ASPP1 increases the inhibition of ASPP2."
  sent10 should "contain 1 positive activation and 0 negative ones" in {
    val mentions = getBioMentions(sent10)
    mentions.filter(_.label.contains("Positive_activation")) should have size (1)
    mentions.filter(_.label.contains("Negative_activation")) should have size (0)
  }

  // Controller and Controlled cannot be the same entity
  val sent11 = "MEK activates MEK."
  sent11 should "not contain a positive activation" in {
    val mentions = getBioMentions(sent11)
    mentions.filter(_.label.contains("Positive_activation")) should have size (0)
  }

//  val sent12 = "mTOR inhibitor Rapamycin"
//  sent12 should "contain a negative activation" in {
//    val mentions = getBioMentions(sent12)
//    hasNegativeActivation("Rapamycin", "mTOR", mentions) should be(true)
//  }

  val sent13 = "mTOR activator Rapamycin"
  sent13 should "contain a positive activation" in {
    val mentions = getBioMentions(sent13)
    hasPositiveActivation("Rapamycin", "mTOR", mentions) should be(true)
  }

  val sent14 = "Rapamycin, an inhibitor of the mTOR kinase,"
  sent14 should "contain a negative activation" in {
    val mentions = getBioMentions(sent14)
    hasNegativeActivation("Rapamycin", "mTOR", mentions) should be(true)
  }

  val sent15 = "Rapamycin, an activator of the mTOR kinase,"
  sent15 should "contain a positive activation" in {
    val mentions = getBioMentions(sent15)
    hasPositiveActivation("Rapamycin", "mTOR", mentions) should be(true)
  }

  val sent16 = "Inhibition of mTOR by rapamycin has been standard treatment"
  sent16 should "contain a negative activation (MARCO)" in {
    val mentions = getBioMentions(sent16)
    hasNegativeActivation("rapamycin", "mTOR", mentions) should be(true)
  }

  val sent17 = "XRCC1 stimulates DNA-PK enzymatic activity"
  sent17 should "contain 1 activation" in {
    val mentions = getBioMentions(sent17)
    hasPositiveActivation("XRCC1", "DNA-PK", mentions) should be(true)
  }

  val sent18 = "Reciprocally, XRCC1 stimulates the kinase activity of DNA-PK on serine 15 of p53 in vitro"
  sent18 should "contain 1 activation" in {
    val mentions = getBioMentions(sent18)
    hasPositiveActivation("XRCC1", "DNA-PK", mentions) should be(true)
  }

  val sent19 = "XRCC1 stimulates DNA-PK catalytic activity in vitro"
  sent19 should "contain 1 activation" in {
    val mentions = getBioMentions(sent19)
    hasPositiveActivation("XRCC1", "DNA-PK", mentions) should be(true)
  }

  val sent20 = "Taken together, these data indicate that XRCC1 strongly stimulates DNA-PK activity and that this stimulatory effect is weakened in the mutant S371D that mimics a phosphorylated status of the BRCT1 domain."
  sent20 should "contain 1 activation" in {
    val mentions = getBioMentions(sent20)
    hasPositiveActivation("XRCC1", "DNA-PK", mentions) should be(true)
  }

  val sent21 = "The phosphorylation of MEK activates K-Ras."
  sent21 should "contain 1 activation with a phosphorylation event as its controller" in {
    val mentions = getBioMentions(sent21)
    val activations = mentions.filter(_ matches "Positive_activation")
    activations should have size (1)
    activations.head.arguments("controller").head.label should equal ("Phosphorylation")
  }

  val sent22 = "The phosphorylation of MEK deactivates K-Ras."
  sent22 should "contain 1 Negative Activation with a phosphorylation event as its controller" in {
    val mentions = getBioMentions(sent22)
    val negActs = mentions.filter(_ matches "Negative_activation")
    negActs.length should be (1)
    negActs.head.arguments("controller").head.label should equal ("Phosphorylation")
    // We shouldn't pick up any Positive Activations
    mentions.count(_ matches "Positive_activation") should be (0)
  }

  // Relies on COREF
  /*val sent23 = "ASPP1 is common, as is its inhibition by ASPP2."
  sent23 should "contain 1 activation" in {
    val mentions = getBioMentions(sent23)
    // Two entities and one event
    mentions should have size (3)
    mentions.filter(_ matches "ActivationEvent") should have size (1)
    hasNegativeActivation("ASPP2", "ASPP1", mentions) should be(true)
  }*/

  val sent24 = "Ubiquitinated Ras activates Raf and PI3K more than non-ubiquitinated Ras"
  sent24 should "contain 2 activations" in {
    val mentions = getBioMentions(sent24)
    hasPositiveActivation("Ras", "Raf", mentions) should be(true)
    hasPositiveActivation("Ras", "PI3K", mentions) should be(true)
  }

  val sent25 = "Figure 2 shows that only the K650M and K650E ASPP1 mutants activated STAT1 in 293T and RCS cells."
  sent25 should "contain 2 activations (GUS)" in {
    val mentions = getBioMentions(sent25)
    // TODO: this fails because we do not capture the 2 mutations for the controller
    mentions.filter(_ matches "ActivationEvent") should have size (2)
    hasPositiveActivation("ASPP1", "STAT1", mentions) should be(true)
  }

  // test missing controller argument
  val sent26 = "ERK phosphorylation in lysates from A375 expressing indicated ORFs following shRNA mediated C-RAF depletion (shCRAF)."
  sent26 should "not contain a positive activation" in {
    val mentions = getBioMentions(sent26)
    mentions.filter(_.label.contains("Positive_activation")) should have size (0)
  }

  // test normal controller argument
  val sent27 = "Interacting proteins that facilitate FGFR3 mediated STAT1 activation could exist in cells."
  sent27 should "contain 1 activation with a controller" in {
    val mentions = getBioMentions(sent27)
    val activations = mentions.filter(_ matches "Positive_activation")
    activations should have size (1)
    activations.head.arguments("controller") should have size (1)
  }

  // tests that controller/controlled don't have overlapping syntactic paths
  val sent28 = "The basal levels of EGFR downstream signaling, shown by the levels of activation specific phosphorylation of Akt, ERK, and STAT3, were not consistently associated with the HER family expression levels or EGFR sequence coding status in a positive or negative manner among the cell lines."
  sent28 should "not contain regulations or activations" in {
    val mentions = getBioMentions(sent28)
    val regulations = mentions.filter(_ matches "Regulation")
    val activations = mentions.filter(_ matches "ActivationEvent")
    regulations should be ('empty)
    activations should be ('empty)
  }

//  val sent29 = "HDAC inhibitors including trichostatin A completely restored RECK"
//  sent29 should "contain 1 negative activations" in {
//    val mentions = getBioMentions(sent29)
//    mentions.filter(_.label.contains("Negative_activation")) should have size (1)
//    hasNegativeActivation("HDAC", "RECK", mentions) should be(true)
//  }

  // Test Activations where the controlled is a BioProcess
  val sent30 = "In some cases, the presence of Ras inhibits autophagy."
  sent30 should "contain 1 negative activation over a BioProcess" in {
    val mentions = getBioMentions(sent30)
    mentions.filter(_.label.contains("Negative_activation")) should have size (1)
    hasNegativeActivation("Ras", "autophagy", mentions) should be (true)
  }

  // From MITRE's feedback2, 2016 summer eval
  val sent33 = "We found that prolonged expression of active Ras resulted in up-regulation of the MKP3 gene."
  sent33 should "contain 1 transcription and 1 positive activation" in {
    val mentions = getBioMentions(sent33)
    mentions.filter(_.label.contains("Transcription")) should have size (1)
    mentions.filter(_.label.contains("Positive_activation")) should have size (1)
    mentions.filter(_.label.contains("Positive_regulation")) should have size (0)
  }

  val sent34 = "We found that prolonged expression of active Ras resulted in up-regulation of the MKP3 gene via the PI3K/Akt pathway."
  sent34 should "contain 1 transcription and 1 positive activation" in {
    val mentions = getBioMentions(sent34)
    mentions.filter(_.label.contains("Transcription")) should have size (1)
    mentions.filter(_.label.contains("Positive_activation")) should have size (1)
    mentions.filter(_.label.contains("Positive_regulation")) should have size (0)
  }
  val sent36 = "Apoptosis activated p53."
  sent36 should "contain no activations" in {
    val mentions = getBioMentions(sent36)
    hasPositiveActivation("Apoptosis", "p53", mentions) should be (false)
  }
  val sent37 = "Cell aging increases apoptosis"
  sent37 should "contain 1 activation" in {
    val mentions = getBioMentions(sent37)
    hasPositiveActivation("Cell aging", "apoptosis", mentions) should be (true)
  }

  val sent38 = "EGFR silencing deactivates MAPK1"
  sent38 should "contain 1 positive activation" in {
    val mentions = getBioMentions(sent38)
    hasPositiveActivation("EGFR", "MAPK1", mentions) should be (true)
  }
  val sent39 = "EGFR deletion deactivates MAPK1"
  sent39 should "contain 1 positive activation" in {
    val mentions = getBioMentions(sent39)
    hasPositiveActivation("EGFR", "MAPK1", mentions) should be (true)
  }

  val sent40 = "We now show that mTOR inhibition induces insulin receptor substrate-1 expression and abrogates feedback inhibition of the pathway , resulting in Akt activation both in cancer cell lines and in patient tumors treated with the rapamycin derivative , RAD001 ."
  sent40 should "contain 1 positive activation" ignore {
    val cms = getCorefmentionsFromText(sent40)
    cms.count(_ matches "Positive_activation") should be (1)
    hasPositiveActivation(controllerEntity = "rapamycin", controlledEntity = "Akt", cms)
  }

  val sent41 = "AKT1 expression results in subsequent activation of MEK"
  sent41 should "contain 1 Transcription event and 1 activation event" in {
    val mentions = getBioMentions(sent41)
    mentions.filter(_ matches "Transcription") should have size (1)
    mentions.filter(_ matches "Positive_activation") should have size (1)
  }

  val sent42 = "AKT1 expression results in subsequent MEK activation"
  sent42 should "contain 1 activation event" in {
    val mentions = getBioMentions(sent42)
    mentions.filter(_ matches "Transcription") should have size (1)
    mentions.filter(_ matches "Positive_activation") should have size (1)
  }

  val sent43 = "GAPDH catalyzes the conversion of glyceraldehyde-3-phosphate to 1,3-bisphosphoglycerate."
  sent43 should "contain no activations" in {
    val mentions = getBioMentions(sent43)
    mentions.filter(_ matches "Positive_activation") should have size (0)
  }

  val sent44 = "AR function increases docetaxel sensitivity."
  sent44 should "contain no activations" in {
    val mentions = getBioMentions(sent44)
    mentions.filter(_ matches "Positive_activation") should have size (0)
  }

  val sent45 = "The consequences of increased AR function might then increase docetaxel resistance via increasing p21 expression."
  sent45 should "contain no activations" in {
    val mentions = getBioMentions(sent45)
    mentions.filter(_ matches "Positive_activation") should have size (0)
  }

  val sent46 = "BRAF inhibition in NF1 deficient cells"
  sent46 should "contain no activations" in {
    val mentions = getBioMentions(sent46)
    mentions.filter(_ matches "Negative_activation") should have size (0)
  }

  val sent47 = "Here we present a case of a patient with metastatic CMM positive for the BRAF-V600E mutation who was treated with vemurafenib."
  sent47 should "not pick up treated as trigger" in {
    val mentions = getBioMentions(sent47)
    hasPositiveActivation("vemurafenib", " BRAF", mentions) should be (false)
  }

  val sent48 = "Our group previously reported the case of a patient with a recurrent BRAF V600E mutant brainstem ganglioglioma successfully treated with vemurafenib and vinblastine."
  sent48 should "not pick up treated as trigger" in {
    val mentions = getBioMentions(sent47)
    hasPositiveActivation("vemurafenib", " BRAF", mentions) should be (false)
  }

  val sent49 = "This drug, vemurafenib, selectively targets V600E mutant BRAF and it is unknown how this drug may affect autophagic flux."
  sent49 should "not pick up targets as trigger" in {
    val mentions = getBioMentions(sent47)
    hasPositiveActivation("vemurafenib", " BRAF", mentions) should be (false)
  }

  val sent50 = "the Rac1 activator TIAM1"
  sent50 should "contain 1 positive activation event" in {
    val mentions = getBioMentions(sent50)
    hasPositiveActivation("Rac1", "TIAM1", mentions) should be(false)
    hasPositiveActivation("TIAM1", "Rac1", mentions) should be(true)
  }
}
