package org.clulab.reach

import org.scalatest.{FlatSpec, Matchers}
import TestUtils._
import org.clulab.reach.mentions._


/**
  * Tests coreference-based events
  * Date: 5/22/15
  * Last Modified: issue #347: remove bad commented-out test24.
  */
class TestCoreference extends FlatSpec with Matchers {
  val sent1 = "ASPP2 is even more common than BEF, and it is often ubiquitinated."
  sent1 should "not produce a ubiquitination of ASPP2" in {
    val mentions = getBioMentions(sent1)
    TestUtils.hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
  }
  it should "produce a ubiquitination of BEF" in {
    val mentions = getBioMentions(sent1)
    TestUtils.hasEventWithArguments("Ubiquitination", List("BEF"), mentions) should be (false)
  }

  val sent2 = "Even more than BEF, ASPP2 is common, as is their phosphorylation."
  sent2 should "produce two phosphorylations, one of ASPP2 and one of BEF" in {
    val mentions = getBioMentions(sent2)
    TestUtils.hasEventWithArguments("Phosphorylation", List("BEF"), mentions) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    mentions.filter(_.label == "Phosphorylation") should have size 2
  }

  val sent3 = "Even more than BEF, ASPP2 is common, as is their binding."
  sent3 should "produce one binding of BEF and ASPP2" in {
    val mentions = getBioMentions(sent3)
    TestUtils.hasEventWithArguments("Binding", List("BEF", "ASPP2"), mentions) should be (true)
    mentions.filter(_.label == "Binding") should have size 1
  }

  val sent4 = "ASPP2 is common, even more than BEF and Mek, and so is its binding to them."
  sent4 should "produce two bindings: (BEF, ASPP2), (Mek, ASPP2)" in {
    val mentions = getBioMentions(sent4)
    TestUtils.hasEventWithArguments("Binding", List("BEF", "ASPP2"), mentions) should be (true)
    TestUtils.hasEventWithArguments("Binding", List("Mek", "ASPP2"), mentions) should be (true)
    TestUtils.hasEventWithArguments("Binding", List("Mek", "BEF"), mentions) should be (false)
    mentions.filter(_.label == "Binding") should have size 2
  }

  val sent5 = "To address the effect of BEF ubiquitination on its binding to PI3K and Raf family members, either " +
    "total G12V-K-BEF or the ubiquitinated subfraction of G12V-K-BEF was immunoprecipitated and the immunoprecipitates " +
    "were probed with antibodies to detect associated BEF effector molecules."
  sent5 should "contain 2 binding events" in {
    val mentions = getBioMentions(sent5)
    hasEventWithArguments("Ubiquitination", List("BEF"), mentions) should be (true)
    hasEventWithArguments("Binding", List("BEF", "Raf"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "BEF"), mentions) should be (true)
  }

  // Ensure that regulation is removed if no resolved controller is found.
  val sent6 = "It phosphorylates BEF."
  sent6 should "contain no positive regulation" in {
    val mentions = getBioMentions(sent6)
    mentions.filter(_ matches "Positive_regulation") should have size (0)
    hasEventWithArguments("Phosphorylation", List("BEF"), mentions) should be (true)
  }

  // Ensure that controller cannot be antecedent to controlled's arguments
  val sent7 = "BEF phosphorylates it."
  sent7 should "produce no events" in {
    val mentions = getBioMentions(sent7)
    mentions.filter(_.isInstanceOf[BioEventMention]) should have size (0)
    mentions should have size (1)
  }

  val sent8 = "ASPP2 is common, it is well known, and BEF sumoylates it."
  sent8 should "contain one sumoylation and one regulation" in {
    val mentions = getBioMentions(sent8)
    TestUtils.hasEventWithArguments("Sumoylation", List("ASPP2"), mentions) should be (true)
    val reg = mentions.find(_ matches "Positive_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    controller.text should be ("BEF")
  }

  // Works across sentences; ignores irrelevant pronouns.
  val sent9 = "Much work has been done on ASPP2. It is known that BEF binds it."
  sent9 should "contain one binding and no other events" in {
    val mentions = getBioMentions(sent9)
    mentions.find(_ matches "ComplexEvent") should not be ('defined)
    hasEventWithArguments("Binding", List("BEF", "ASPP2"), mentions) should be (true)
    mentions.filter(_.isInstanceOf[BioEventMention]) should have size (1)
  }

  // Number-sensitive search works with cause controllers but not triggered regulation plurals
  val sent10 = "BEF and Mek are in proximity, and they phosphorylate ASPP2."
  val sent10a = "BEF and Mek are in proximity, and they upregulate the phosphorylation of ASPP2."
  sent10 should "contain one phosphorylation and two regulations" in {
    val mentions = getBioMentions(sent10)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions)
    mentions.filter(_ matches "Positive_regulation") should have size (2)
    hasPositiveRegulationByEntity("BEF","BioChemicalEntity",Seq("ASPP2"),mentions)
    hasPositiveRegulationByEntity("Mek","BioChemicalEntity",Seq("ASPP2"),mentions)
  }
  sent10a should "contain one phosphorylation and two regulations" in {
    val mentions = getBioMentions(sent10a)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions)
    mentions.filter(_ matches "Positive_regulation") should have size (2)
    hasPositiveRegulationByEntity("BEF","BioChemicalEntity",Seq("ASPP2"),mentions)
    hasPositiveRegulationByEntity("Mek","BioChemicalEntity",Seq("ASPP2"),mentions)
  }

  // Number-sensitive search works with cause controlleds but not triggered regulation plurals
  val sent11 = "BEF and Mek are in proximity, and ASPP2 phosphorylates them."
  sent11 should "contain two phosphorylation and two regulations" in {
    val mentions = getBioMentions(sent11)
    mentions.filter(_ matches "Phosphorylation") should have size (2)
    hasEventWithArguments("Phosphorylation", List("BEF"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("Mek"), mentions) should be (true)
    mentions.filter(_ matches "Positive_regulation") should have size (2)
    hasPositiveRegulationByEntity("ASPP2","Phosphorylation",Seq("BEF"),mentions)
    hasPositiveRegulationByEntity("ASPP2","Phosphorylation",Seq("Mek"),mentions)
  }

  // Number-sensitive search works with activation controllers, but plurals are forbidden.
  val sent12 = "BEF is in proximity, and it activates ASPP2."
  sent12 should "contain a Positive_activation" in {
    val mentions = getBioMentions(sent12)
    mentions.filter(_ matches "ActivationEvent") should have size (1)
    hasEventWithArguments("Positive_activation", List("BEF", "ASPP2"), mentions) should be (true)
  }

  // Number-sensitive search works with activation controlleds, but plurals are forbidden.
  val sent13 = "Mek is in proximity, and ASPP2 activates it."
  sent13 should "contain one activation and one regulation" in {
    val mentions = getBioMentions(sent13)
    mentions.filter(_ matches "ActivationEvent") should have size (1)
    hasEventWithArguments("Positive_activation", List("ASPP2", "Mek"), mentions) should be (true)
  }

  // Sane noun phBEFes should be matched
  val sent14 = "ASPP1 is common, and this protein binds GTP."
  sent14 should "contain one binding event only" in {
    val mentions = getBioMentions(sent14)
    hasEventWithArguments("Binding", List("ASPP1", "GTP"), mentions) should be (true)
    mentions should have size (4)
  }

  // Filter out bindings with one complete theme and one unresolved theme.
  val sent14b = "This protein binds GTP."
  sent14b should "contain no binding events" in {
    val mentions = getBioMentions(sent14b)
    mentions.filter(_ matches "Binding") should have size (0)
    mentions should have size (1)
  }

  // Ignore noun phBEFes that can't have BioChemicalEntity antecedents
  val sent15 = "BEF is common, and a mouse binds GTP."
  sent15 should "not contain any events" in {
    val mentions = getBioMentions(sent15)
    mentions filter (_ matches "Event") should have size (0)
    mentions should have size (3)
  }

  // Ignore anything two sentences prior when searching for antecedents.
  val sent16 = "BEF is common. This is an intervening sentence. It binds Mek."
  sent16 should "not contain any events" in {
    val mentions = getBioMentions(sent16)
    mentions filter (_ matches "Event") should have size (0)
  }

  // Can find an antecedent mention between start of event mention and start of text bound mention
  val sent17 = "ASPP2 is common, and BEF binds the Mek protein."
  sent17 should "contain a single binding between Mek and BEF" in {
    val mentions = getBioMentions(sent17)
    hasEventWithArguments("Binding", List("BEF", "Mek"), mentions) should be (true)
    hasEventWithArguments("Binding", List("BEF", "ASPP2"), mentions) should be (false)
    hasEventWithArguments("Binding", List("Mek", "ASPP2"), mentions) should be (false)
  }

  // Events with invalid numbers of antecedents are ignored
  val sent18 = "ASPP2 and BEF are common, as is its binding."
  val sent18a = "ASPP2 and BEF are common, as is their activation."
  val sent18b = "The phosphorylation of ASPP2 and BEF is common, as is their upregulation."
  sent18 should "not contain any events" in {
    val mentions = getBioMentions(sent18)
    mentions filter (_ matches "Event") should have size (0)
  }

  sent18a should "not contain any events" in {
    val mentions = getBioMentions(sent18a)
    mentions filter (_ matches "Event") should have size (0)
  }

  sent18b should "contain no activations or regulations" in {
    val mentions = getBioMentions(sent18b)
    mentions filter (_ matches "ComplexEvent") should have size (0)
    mentions filter (_ matches "ActivationEvent") should have size (0)
    mentions filter (_ matches "Phosphorylation") should have size (2)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("BEF"), mentions) should be (true)
  }

  val sent19 = "ASPP1 is common, and it binds Mek and BEF"
  sent19 should "contain two bindings, (ASPP1,Mek) and (ASPP1,BEF)" in {
    val mentions = getBioMentions(sent19)
    mentions filter (_ matches "Binding") should have size (2)
    mentions filter (_ matches "Event") should have size (2)
    hasEventWithArguments("Binding", List("ASPP1", "Mek"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP1", "BEF"), mentions) should be (true)
  }

  val sent20 = "We also monitored how siRNA-induced loss of LMTK2 influenced phosphorylation of PP1Cthr320. Four " +
    "different LMTK2 siRNAs all markedly reduced LMTK2 levels and this led to a corresponding decrease in PP1Cthr320 " +
    "phosphorylation."
  sent20 should "not contain an activation of and by the same entity" in {
    val mentions = getBioMentions(sent20)
    hasPositiveActivation("LMTK2","LMTK2",mentions) should be (false)
  }

  val sent21 = "Inhibition of mTOR kinase is feasible with the macrolide natural product rapamycin (aka: sirolimus, " +
    "RAPA, Rapamune, AY-22989, and NSC-226080). Rapamycin is an FDA-approved agent used as immunosuppressive therapy " +
    "post organ transplant ."
  sent21 should "not produce a requirement error from Anaphoric.antecedent" in {
    val mentions = getBioMentions(sent21)
    mentions.forall { mention =>
      !mention.toCorefMention.antecedentOrElse(mention.toCorefMention).isGeneric
    } should be (true)
  }

  val sent22 = "Second, STAT1 accumulates and shows nuclear localization in the cartilage of TD-affected human fetuses " +
    "as well as in mice carrying the K644E-FGFR3 mutation (homologous to human K650E)     ,     . Finally, two " +
    "experimental studies show that the loss of STAT1 partially rescues the growth-inhibitory action of FGF signaling " +
    "in chondrocytes     ,     , both suggesting the role of STAT1 in the growth-inhibitory FGFR3 action in cartilage."
  sent22 should "not produce an activation with an activation controlled" in {
    val mentions = getBioMentions(sent22)
    mentions filter (_ matches "ActivationEvent") should have size (3)
    mentions.forall { mention =>
      !(mention matches "ActivationEvent") ||
        mention.arguments("controlled").forall(controlled => !(controlled.antecedentOrElse(controlled) matches "Event"))
    } should be (true)
    hasEventWithArguments("Positive_Activation", List("STAT1 partially rescues the growth-inhibitory action of FGF"), mentions) should be (false)
  }

  val sent23 = "Most efforts at understanding Ras mediated transformation have centered on identifying those targets " +
    "that bind RasGTP . However , our data raise the possibility that there is a class of proteins , such as " +
    "PI3KC2beta , that bind nucleotide-free Ras and are negatively regulated by this interaction ."
  sent23 should "not produce any Regulations" in {
    val mentions = getBioMentions(sent23)
    mentions filter (_ matches "Regulation") should have size (0)
  }

  val sent25 = "Another example can be given with mutated p53. The pivotal role of p53 as a tumor suppressor is " +
    "illustrated by the fact that this protein is found mutated in ∼50% of human cancers. In most cases, mutations " +
    "in p53 greatly increase the otherwise short half life of this protein and cause it to accumulate in tumor cells."
  sent25 should "not produce an error due to multiple antecedents" in {
    val mentions = getBioMentions(sent25)
    mentions.exists(_.text == "p53") should be (true)
  }

  val sent26 = "Many RTKs interact directly with Grb2, some rely on Shc family adaptors to recruit Grb2, and others " +
    "do both    . While direct Grb2/RTK interactions involve binding of the Grb2 SH2 domain to pYXNX motifs, Shc " +
    "proteins interact with RTKs primarily through the binding of their N-terminal PTB domain to NPXpY motifs."
  sent26 should "not produce an error due to multiple antecedents" in {
    val mentions = getBioMentions(sent26)
    mentions.exists(_.text == "Grb2") should be (true)
  }

  val simpleEventTypes = Seq(
    "Phosphorylation",
    "Ubiquitination",
    "Hydroxylation",
    "Sumoylation",
    "Acetylation",
    "Farnesylation",
    "Ribosylation",
    "Methylation"
  )
  val simpleEventVbs = Seq(
    "phosphorylates",
    "ubiquitinates",
    "hydroxylates",
    "sumoylates",
    "acetylates",
    "farnesylates",
    "ribosylates",
    "methylates"
  )
  val simpleEventNs = Seq(
    "phosphorylation",
    "ubiquitination",
    "hydroxylation",
    "sumoylation",
    "acetylation",
    "farnesylation",
    "ribosylation",
    "methylation"
  )

  for (i <- simpleEventTypes.indices) {
    // Event coreference only with definite determiners or demonstratives
    val sent27a = s"We found that ASPP1 ${simpleEventVbs(i)} ASPP2, and this ${simpleEventNs(i)} upregulates STAT1."
    val sent27b = s"We found that ASPP1 ${simpleEventVbs(i)} ASPP2, and ${simpleEventNs(i)} upregulates STAT1."
    sent27a should "contain an ActivationEvent" in {
      val mentions = getBioMentions(sent27a)
      mentions filter (_ matches "ActivationEvent") should have size (1)
      hasEventWithArguments(simpleEventTypes(i), List("ASPP2"), mentions) should be(true)
      hasEventWithArguments("Positive_activation", List(s"ASPP2", "STAT1"), mentions) should be(true)
    }
    sent27b should "not contain an ActivationEvent" in {
      val mentions = getBioMentions(sent27b)
      mentions filter (_ matches "ActivationEvent") should have size (0)
      hasEventWithArguments(simpleEventTypes(i), List("ASPP2"), mentions) should be(true)
    }
  }

  // Filter out open-class generic mentions ("protein") that have no definite determiner.
  val sent28a = "ASPP1 is common, and a protein is phosphorylated."
  val sent28b = "ASPP1 is common, and a cistron phosphorylates ASPP2."
  sent28a should "not contain any events" in {
    val mentions = getBioMentions(sent28a)
    mentions filter (_ matches "Event") should have size (0)
  }
  sent28b should "not contain any complex events" in {
    val mentions = getBioMentions(sent28b)
    mentions filter (_ matches "ComplexEvent") should have size (0)
  }

  // Organs and other context entities can't be antecedents
  val sent29 = "Liver is delicious, and it phosphorylates Raf."
  val sent30 = "Humans are numerous, and they are sometimes activated by Raf."
  sent29 should "not contain any complex events" in {
    val mentions = getBioMentions(sent29)
    mentions filter (_ matches "ComplexEvent") should have size (0)
  }
  sent30 should "not contain any Events" in {
    val mentions = getBioMentions(sent30)
    mentions filter (_ matches "Event") should have size (0)
  }

  // Link a known mutant of a known protein with an unknown mutant of a known protein
  val sent31a = "ASPP1 K341L is common, and this mutant ASPP1 binds GTP."
  val sent31b = "ASPP2 K341L is common, and this mutant ASPP1 binds GTP."
  sent31a should "contain a binding to a non-generic mutant" in {
    val mentions = getBioMentions(sent31a)
    val relevantMutant = mentions.find(_ matches "Binding").get.arguments.values.flatten.find(_.text == "ASPP1").get
    relevantMutant.toCorefMention.antecedent.get.asInstanceOf[CorefMention].mutants.exists(mut => mut.text == "K341L") should be (true)
  }
  sent31b should "contain a binding to ASPP1" in {
    val mentions = getBioMentions(sent31b)
    val relevantMutant = mentions.find(_ matches "Binding").get.arguments.values.flatten.find(m => m.text == "ASPP1").get.toCorefMention
    relevantMutant.antecedent.isEmpty should be (true)
    relevantMutant.hasGenericMutation should be (true)
  }
  it should "not contain a binding to ASPP2" in {
    val mentions = getBioMentions(sent31b)
    mentions.find(_ matches "Binding").get.arguments.values.flatten.exists(m => m.text == "ASPP2") should be (false)
  }

  // Link a known mutant of a known protein with an unknown mutant of a known protein
  val sent32a = "ASPP1 K341L is common, and the K341L mutant binds GTP."
  val sent32b = "ASPP1 K341M is common, and the K341L mutant binds GTP."
  sent32a should "contain a binding to a non-generic mutant" in {
    val mentions = getBioMentions(sent32a)
    val relevantMutant = mentions.find(_ matches "Binding").get.arguments.values.flatten.find(_.text == "mutant").get
    relevantMutant.toCorefMention.antecedent.get.asInstanceOf[CorefMention].mutants.exists(mut => mut.text == "K341L") should be (true)
  }
  sent32b should "not contain a binding" in {
    val mentions = getBioMentions(sent32b)
    mentions.exists(_ matches "Binding") should be (false)
  }

  // Link an unknown mutant to a fully known one. Don't link when the anaphor is anything other than 'mutant(s)'
  val sent33a = "ASPP1 K341L is common, and the mutant binds GTP."
  val sent33b = "ASPP1 is common, and the mutant binds GTP."
  val sent33c = "ASPP1 K341L is common, and the protein binds GTP."
  sent33a should "contain a binding to a non-generic mutant" in {
    val mentions = getBioMentions(sent33a)
    val relevantMutant = mentions.find(_ matches "Binding").get.arguments.values.flatten.find(_.text == "mutant").get
    relevantMutant.toCorefMention.antecedent.get.asInstanceOf[CorefMention].mutants.exists(mut => mut.text == "K341L") should be (true)
  }
  sent33b should "not contain a binding" in {
    val mentions = getBioMentions(sent33b)
    mentions.exists(_ matches "Binding") should be (false)
  }
  sent33c should "not contain a binding" in {
    val mentions = getBioMentions(sent33c)
    mentions.exists(_ matches "Binding") should be (false)
  }

//  val sent34 = "Cells were transfected with N540K, G380R, R248C, Y373C, K650M and K650E-FGFR3 mutants and analyzed " +
//    "for activatory STAT1(Y701) phosphorylation 48 hours later. In 293T and RCS cells, all six FGFR3 mutants induced " +
//    "activatory ERK(T202/Y204) phosphorylation"
//  sent34 should "contain 12 regulations of 2 ERK phosphorylations" in {
//    val mentions = getBioMentions(sent34)
//    val phos = mentions.filter(m => m.matches("Phosphorylation") &&
//      m.arguments.getOrElse("theme", Nil).map(_.text).contains("ERK"))
//    phos should have size (2)
//    val regs = mentions.filter(m => m.matches("Positive_regulation"))
//    regs should have size (12)
//  }

//  val sent35 = "Vectors carrying the wild-type FGFR3 as well as the N540K (HCH), G380R (ACH), R248C, Y373C, K650E " +
//  "(TD) and K650M (SADDAN and TD) mutants were expressed in CHO cells. It is possible that N540K, G380R, R248C and " +
//  "Y373C mutants still activate STAT1 in cells, despite the lack of this capacity in a kinase assay"
//
//  val sent36 = "GST-N343 was phosphorylated. In contBEFt, its Ala mutant at Ser34 (S34A) was not phosphorylated. The " +
//    "Ala mutant at Thr149 was phosphorylated by Cdk5/p35, similarly to the unmutated fragment."

  // Spread grounding from BEF to ungrounded alias BEF4H.
  val sent37a = "BEF4H protein (hereafter referred to as BEF) is phosphorylated."
  sent37a should "apply BEF grounding to BEF4H" in {
    val mentions = getBioMentions(sent37a)
    val entities = mentions filter (_ matches "Entity")
    entities should have size (2)
    entities.head.grounding.get.equals(entities.last.grounding.get) should be (true)
  }
  // Order shouldn't matter
  val sent37b = "BEF (hereafter referred to as BEF4H) is phosphorylated."
  sent37b should "apply BEF grounding to BEF4H" in {
    val mentions = getBioMentions(sent37b)
    val entities = mentions filter (_ matches "Entity")
    entities should have size (2)
    entities.head.grounding.get.equals(entities.last.grounding.get) should be (true)
  }
  // Aliases must be of same type
  val sent38 = "Ras (hereafter referred to as S135) is phosphorylated."
  sent38 should "not apply Ras grounding to S135 or vice versa" in {
    val mentions = getBioMentions(sent38)
    val entities = mentions filter (m => (m matches "Entity") || (m matches "Site"))
    entities should have size (2)
    entities.head.grounding.get.equals(entities.last.grounding.get) should be (false)
  }
  val sent39 = "BEF (hereafter referred to as S135) is phosphorylated."
  sent39 should "not apply S135 grounding to H-BEF or vice versa" in {
    val mentions = getBioMentions(sent39)
    val entities = mentions filter (m => (m matches "Entity") || (m matches "Site"))
    entities should have size (2)
    entities.head.grounding.get.equals(entities.last.grounding.get) should be (false)
  }
  val sent40 = "BEF4H, sometimes called BEF, phosphorylates Akt."
  sent40 should "apply BEF grounding to BEF4H" in {
    val mentions = getBioMentions(sent40)
    val ungrounded = mentions.find(_.text == "BEF4H")
    val grounded = mentions.find(_.text == "BEF")
    ungrounded.isDefined should be (true)
    grounded.isDefined should be (true)
    ungrounded.get.grounding.get.equals(grounded.get.grounding.get) should be (true)
  }
  val sent41 = "BEF4H (alias BEF) phosphorylates Akt."
  sent41 should "apply BEF grounding to BEF4H" in {
    val mentions = getBioMentions(sent41)
    val ungrounded = mentions.find(_.text == "BEF4H")
    val grounded = mentions.find(_.text == "BEF")
    ungrounded.isDefined should be (true)
    grounded.isDefined should be (true)
    ungrounded.get.grounding.get.equals(grounded.get.grounding.get) should be (true)
  }
  // Series should work with 'or'
  val sent42 = "Akt1 (a.k.a. Akt334, AktTR, or Akt4H) is phosphorylated."
  sent42 should "apply Akt1 grounding to 3 made-up proteins" in {
    val mentions = getBioMentions(sent42)
    val entities = mentions filter (m => m matches "Entity")
    entities should have size (4)
    entities.combinations(2).forall(pair => pair.head.grounding.get.equals(pair.last.grounding.get)) should be (true)
  }
  // Series should not work with 'and' (because we could have "BEF and Akt (a.k.a. BEF334 and Akt4H)"),
  // which isn't handled yet.
  val sent43 = "Akt1 (a.k.a. Akt334 and Akt4H) is phosphorylated."
  sent43 should "not apply Akt1 grounding to other proteins" in {
    val mentions = getBioMentions(sent43)
    val entities = mentions filter (m => m matches "Entity")
    entities should have size (3)
    entities.combinations(2).forall(pair => pair.head.grounding.get.equals(pair.last.grounding.get)) should be (false)
  }

  // Alias assignment works in any order, and across sentences!
  val sent44a = "Akt (also called Akt334, AktTR, or Akt4H) is phosphorylated. AktTR is also ubiquitinated."
  sent44a should "apply Akt grounding to 3 proteins" in {
    val mentions = getBioMentions(sent44a)
    val akt = mentions filter (m => m.text == "Akt")
    akt should have size (1)
    val s2akt = mentions filter (m => m.sentence == 1 && m.matches("Entity"))
    s2akt should have size (1)
    akt.head.grounding.get.equals(s2akt.head.grounding.get)
  }
  val sent44b = "AktTR is ubiquitinated. Akt, previously known as Akt334, AktTR, or Akt4H, is also phosphorylated."
  sent44b should "apply Akt grounding to 3 proteins" in {
    val mentions = getBioMentions(sent44b)
    val akt = mentions filter (m => m.text == "Akt")
    akt should have size (1)
    val s2akt = mentions filter (m => m.sentence == 0 && m.matches("Entity"))
    s2akt should have size (1)
    akt.head.grounding.get.equals(s2akt.head.grounding.get)
  }
  // Alias assignment works across sections of a document
  val sent45a = "Akt1, previously known as Akt334, AktTR, or Akt4H, is also phosphorylated."
  val sent45b = "AktTR is ubiquitinated."
  "Intra-document alias" should "share Akt1 grounding across sections" in {
    val fe1 = FriesEntry("test", "aliasDoc", "01", "start", isTitle = false, sent45a, None)
    val fe2 = FriesEntry("test", "aliasDoc", "01", "start", isTitle = false, sent45b, None)
    val mentions = testReach.extractFrom(Seq(fe1, fe2))
    val akt = mentions.find(_.text == "Akt1").get
    mentions.filter(_.text == "AktTR").forall(m => m.grounding.get == akt.grounding.get) should be (true)
  }
  // No problem if no mentions in a document.
  val sent46 = "This sentence has no mentions."
  "Empty document: coref" should "share Akt grounding across sections" in {
    val fe = FriesEntry("anotherTest", "noMentions", "02", "end", isTitle = false, sent46, None)
    val mentions = testReach.extractFrom(Seq(fe))
    mentions.isEmpty should be (true)
  }
  // No error when some mentions have generic and some non-generic mutations
  val sent47 = "We analyzed sporadic CRCs in Omani (of African origin, N = 61), Iranian (of Caucasian origin, " +
    "N = 53) and African American (N = 95) patients for microsatellite instability, expression status of mismatched " +
    "repair genes (hMLH1, hMSH2) and presence of the BRAF (V600E) mutation. In the Omani group, all tumors with " +
    "BRAF mutations were located in the left side of the colon, and for African Americans, 88% of tumors with BRAF " +
    "mutations were found in the right side of the colon."
  sent47 should "not produce an error" in {
    val mentions = getBioMentions(sent47)
  }
  // No error indicating CorefMentions that should have been split but weren't
  val sent48 = "Since EGFR mutation is known to be associated with sensitivity to erlotinib, and KRas mutations are " +
    "associated with resistance, we focused on the group of wild-type EGFR/KRas cell lines. We found that the half " +
    "maximal inhibitory concentration (IC50) for erlotinib was significantly higher in cell lines that segregated to " +
    "clusters with methylated SRAMs compared to those that segregated to clusters with unmethylated SRAMs"
  sent48 should "not produce an error" in {
    val mentions = getBioMentions(sent48)
  }

  // Aliases must be of same type
  val sent50 = "Akt (hereafter referred to as diacylglycerol) is phosphorylated."
  sent50 should "not apply Akt grounding to diacylglycerol or vice versa" in {
    val mentions = getBioMentions(sent50)
    val entities = mentions filter (_ matches "Entity")
    entities should have size 2
    entities.head.grounding.get.equals(entities.last.grounding.get) should be (false)
  }
  val sent51 = "Diacylglycerol (hereafter referred to as S135) functions as a second messenger signaling lipid."
  sent51 should "not apply S135 grounding to diacylglycerol or vice versa" in {
    val mentions = getBioMentions(sent51)
    val entities = mentions filter (m => (m matches "Entity") || (m matches "Site"))
    entities should have size 2
    entities.head.grounding.get.equals(entities.last.grounding.get) should be (false)
  }

  val sent55 = "Gab1 mutant protein enhances EGF induced activation of the PI-3"
  sent55 should "contain a two-level complex event" in {
    val mentions = getBioMentions(sent55)
    val posreg = mentions.filter(_ matches "Positive_regulation")
    val posact = mentions.filter(_ matches "Positive_activation")
    posreg should have size 1
    posact should have size 1
    posreg.head.arguments("controlled").head == posact.head should be (true)
  }

  val sent56a = "Akta and HSP20 are common. It phosphorylates Akta."
  sent56a should "match 'It' to 'HSP20' (not 'Akta')" in {
    val mentions = getBioMentions(sent56a)
    val it = mentions.find(_.text == "It")
    //it should not be empty
    it.get.antecedentOrElse(it.get).text should be ("HSP20")
  }

  val sent56b = "ASPP1 binds Mek. It then binds KIAA0771."
  sent56b should "match 'It' to 'Mek' (not 'ASPP1')" in {
    val mentions = getBioMentions(sent56b)
    val it = mentions.find(_.text == "It")
    //it should not be empty
    it.get.antecedentOrElse(it.get).text should be ("Mek")
  }

  val sent57 = "It is possible that the effects of HSP20 on AKT might differ between normal cardiomyocytes or " +
    "mesenchymal stem cells and HCC cells. The binding partner(s) of HSP20 and their interaction(s) might be " +
    "dependent on the cell types."
  sent57 should "match 'their' to 'AKT' (not 'HSP20')" in {
    val mentions = getBioMentions(sent57)
    val their = mentions.filter(_.text == "their")
    their.nonEmpty should be (true)
    val ants = their.map(m => m.antecedentOrElse(m).text)
    ants should contain ("AKT")
    ants should contain ("HSP20")
  }

  val sent58 = "ASPP1 (better known as ASPP2) is a common protein."
  sent58 should "share grounding between ASPP1 and ASPP2" in {
    val mentions = getBioMentions(sent58)
    mentions should have size 2
    mentions.head.candidates should not be empty
    mentions.last.candidates should not be empty
    val aspp1Cands = mentions.head.candidates.get.toSet
    val aspp2Cands = mentions.last.candidates.get.toSet
    aspp1Cands should equal(aspp2Cands)
  }

  val sent59 = "ASPP1 (better known as 23peM) is a common protein."
  sent59 should "make a mention of 23peM and ground it to ASPP1" in {
    val mentions = getBioMentions(sent59)
    mentions should have size 2
    mentions.head.candidates should not be empty
    mentions.last.candidates should not be empty
    mentions.head.labels should equal (mentions.last.labels)
    val aspp1Cands = mentions.head.candidates.get.toSet
    val nonceCands = mentions.last.candidates.get.toSet
    aspp1Cands should equal (nonceCands)
  }

  val sent60 = "23peM (ASPP1) is a common protein."
  sent60 should "make a mention of 23peM and ground it to ASPP1" in {
    val mentions = getBioMentions(sent59)
    mentions should have size 2
    mentions.head.candidates should not be empty
    mentions.last.candidates should not be empty
    mentions.head.labels should equal (mentions.last.labels)
    val nonceCands = mentions.head.candidates.get.toSet
    val aspp1Cands = mentions.last.candidates.get.toSet
    aspp1Cands should equal (nonceCands)
  }

  // When documents are processed together, aliases found in one should be sought in the others
  val sent61 = "We examine the role of 23peM."
  val doc1 = testReach.mkDoc(sent61, "testDoc1")
  val doc2 = testReach.mkDoc(sent60, "testDoc2")
  sent61 should "contain a mention of 23peM" in {
    val mentions = testReach.extractFrom(Nil, Seq(doc1, doc2))
    val nonces = mentions.filter(_.text == "23peM")
    nonces should have size 2
    val asp = mentions.find(_.text == "ASPP1")
    asp should not be empty
    val aspCands = asp.get.candidates.get.toSet
    nonces.foreach(nonce => nonce.candidates.get.toSet should equal (aspCands))
  }

  val sent62a = "We studied the effects of the Pax6 homologs eyeless and eyegone."
  sent62a should "contain grounded mentions for eyeless and eyegone" in {
    val mentions = getBioMentions(sent62a)
    mentions should have size 3
    val pax = mentions.find(_.text == "Pax6")
    pax should not be empty
    val targetGrounding = pax.get.candidates.get.toSet
    mentions.filter(_.text == "eyeless").foreach(_.candidates.get.toSet should equal(targetGrounding))
    mentions.filter(_.text == "eyegone").foreach(_.candidates.get.toSet should equal(targetGrounding))
  }

  val sent62b = "The Pax6 homologs eyeless, eyefull, and eyegone were found in established lines."
  sent62b should "contain grounded mentions for eyeless, eyefull, and eyegone" in {
    val mentions = getBioMentions(sent62b)
    mentions should have size 4
    val pax = mentions.find(_.text == "Pax6")
    pax should not be empty
    val targetGrounding = pax.get.candidates.get.toSet
    mentions.filter(_.text == "eyeless").foreach(_.candidates.get.toSet should equal(targetGrounding))
    mentions.filter(_.text == "eyefull").foreach(_.candidates.get.toSet should equal(targetGrounding))
    mentions.filter(_.text == "eyegone").foreach(_.candidates.get.toSet should equal(targetGrounding))
  }

  val sent63 = "Eyeless and eyegone, homologs of Pax6, are the subject of this work."
  sent63 should "contain grounded mentions for eyeless and eyegone" in {
    val mentions = getBioMentions(sent63)
    mentions should have size 3
    val pax = mentions.find(_.text == "Pax6")
    pax should not be empty
    val targetGrounding = pax.get.candidates.get.toSet
    mentions.filter(_.text.toLowerCase == "eyeless").foreach(_.candidates.get.toSet should equal(targetGrounding))
    mentions.filter(_.text == "eyegone").foreach(_.candidates.get.toSet should equal(targetGrounding))
  }

  //
  // Several alias unit tests that no longer work with processors 8.0.0
  // These tests no longer work because "DAG" is now tagged as a GGP, which correctly blocks the aliasing
  // HOWEVER, I believe these tests should pass when "DAG" is replaced with, say, "DDD", which is NOT a known entity
  //   See the bCapture used for the alias, which should match here: https://github.com/clulab/reach/blob/master/main/src/main/resources/org/clulab/reach/biogrammar/entities_master.yml#L16
  //   TODO: debug again after processors 8.0.0 is released
  //
  /*
  // Alias assignment works for Simple_chemicals
  val sent49a = "Diacylglycerol (hereafter referred to as DAG) functions as a second messenger signaling lipid."
  sent49a should "apply diacylglycerol grounding to DAG" in {
    val mentions = getBioMentions(sent49a)
    val entities = mentions filter (_ matches "Entity")
    entities should have size 2
    entities.head.grounding.get.equals(entities.last.grounding.get) should be (true)
  }
  // Order shouldn't matter
  val sent49b = "DAG (hereafter referred to as diacylglycerol) functions as a second messenger signaling lipid."
  sent49b should "apply diacylglycerol grounding to DAG" in {
    val mentions = getBioMentions(sent49b)
    val entities = mentions filter (_ matches "Entity")
    entities should have size 2
    entities.head.grounding.get.equals(entities.last.grounding.get) should be (true)
  }
  val sent52 = "Diacylglycerol, sometimes called DAG, functions as a second messenger signaling lipid."
  sent52 should "apply diacylglycerol grounding to DAG" in {
    val mentions = getBioMentions(sent52)
    val entities = mentions filter (_ matches "Entity")
    entities should have size 2
    entities.head.grounding.get.equals(entities.last.grounding.get) should be (true)
  }
  val sent53 = "Diacylglycerol (alias DAG) functions as a second messenger signaling lipid."
  sent53 should "apply diacylglycerol grounding to DAG" in {
    val mentions = getBioMentions(sent53)
    val entities = mentions filter (_ matches "Entity")
    entities should have size 2
    entities.head.grounding.get.equals(entities.last.grounding.get) should be (true)
  }
  // Series should work with 'or'
  val sent54 = "Diacylglycerol (a.k.a. DAG, DAG, or DAG) functions as a second messenger signaling lipid."
  sent54 should "apply diacylglycerol grounding to 3 chemicals" in {
    val mentions = getBioMentions(sent54)
    val entities = mentions filter (_ matches "Entity")
    entities should have size 4
    entities.combinations(2).forall(pair => pair.head.grounding.get.equals(pair.last.grounding.get)) should be (true)
  }
  */
}
