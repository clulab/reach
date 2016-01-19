package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.mentions._

/**
 * Tests coreference-based events
 * Date: 5/22/15
 */
class TestCoreference extends FlatSpec with Matchers {
  val sent1 = "ASPP2 is even more common than Ras, and it is often ubiquitinated."
  sent1 should "not produce a ubiquitination of ASPP2" in {
    val mentions = getBioMentions(sent1)
    TestUtils.hasEventWithArguments("Ubiquitination", List("ASPP2"), mentions) should be (true)
  }
  it should "produce a ubiquitination of Ras" in {
    val mentions = getBioMentions(sent1)
    TestUtils.hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (false)
  }

  val sent2 = "Even more than Ras, ASPP2 is common, as is their phosphorylation."
  sent2 should "produce two phosphorylations, one of ASPP2 and one of Ras" in {
    val mentions = getBioMentions(sent2)
    TestUtils.hasEventWithArguments("Phosphorylation", List("Ras"), mentions) should be (true)
    TestUtils.hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions) should be (true)
    mentions.filter(_.label == "Phosphorylation") should have size 2
  }

  val sent3 = "Even more than Ras, ASPP2 is common, as is their binding."
  sent3 should "produce one binding of Ras and ASPP2" in {
    val mentions = getBioMentions(sent3)
    TestUtils.hasEventWithArguments("Binding", List("Ras", "ASPP2"), mentions) should be (true)
    mentions.filter(_.label == "Binding") should have size 1
  }

  val sent4 = "ASPP2 is common, even more than Ras and Mek, and so is its binding to them."
  sent4 should "produce two bindings: (Ras, ASPP2), (Mek, ASPP2)" in {
    val mentions = getBioMentions(sent4)
    TestUtils.hasEventWithArguments("Binding", List("Ras", "ASPP2"), mentions) should be (true)
    TestUtils.hasEventWithArguments("Binding", List("Mek", "ASPP2"), mentions) should be (true)
    TestUtils.hasEventWithArguments("Binding", List("Mek", "Ras"), mentions) should be (false)
    mentions.filter(_.label == "Binding") should have size 2
  }

  val sent5 = "To address the effect of Ras ubiquitination on its binding to PI3K and Raf family members, either " +
    "total G12V-K-Ras or the ubiquitinated subfraction of G12V-K-Ras was immunoprecipitated and the immunoprecipitates " +
    "were probed with antibodies to detect associated Ras effector molecules."
  sent5 should "contain 2 binding events" in {
    val mentions = getBioMentions(sent5)
    hasEventWithArguments("Ubiquitination", List("Ras"), mentions) should be (true)
    hasEventWithArguments("Binding", List("Ras", "Raf"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "Ras"), mentions) should be (true)
  }

  // Ensure that regulation is removed if no resolved controller is found.
  val sent6 = "It phosphorylates Ras."
  sent6 should "contain no positive regulation" in {
    val mentions = getBioMentions(sent6)
    mentions.filter(_ matches "Positive_regulation") should have size (0)
    hasEventWithArguments("Phosphorylation", List("Ras"), mentions) should be (true)
  }

  // Ensure that controller cannot be antecedent to controlled's arguments
  val sent7 = "Ras phosphorylates it."
  sent7 should "produce no events" in {
    val mentions = getBioMentions(sent7)
    mentions.filter(_.isInstanceOf[BioEventMention]) should have size (0)
    mentions should have size (1)
  }

  val sent8 = "ASPP2 is common, it is well known, and Ras sumoylates it."
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
    controller.text should be ("Ras")
  }

  // Works across sentences; ignores irrelevant pronouns.
  val sent9 = "Much work has been done on ASPP2. It is known that Ras binds it."
  sent9 should "contain one binding and no other events" in {
    val mentions = getBioMentions(sent9)
    mentions.find(_ matches "ComplexEvent") should not be ('defined)
    hasEventWithArguments("Binding", List("Ras", "ASPP2"), mentions) should be (true)
    mentions.filter(_.isInstanceOf[BioEventMention]) should have size (1)
  }

  // Number-sensitive search works with cause controllers but not triggered regulation plurals
  val sent10 = "Ras and Mek are in proximity, and they phosphorylate ASPP2."
  val sent10a = "Ras and Mek are in proximity, and they upregulate the phosphorylation of ASPP2."
  sent10 should "contain one phosphorylation and two regulations" in {
    val mentions = getBioMentions(sent10)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions)
    mentions.filter(_ matches "Positive_regulation") should have size (2)
    hasPositiveRegulationByEntity("Ras","BioChemicalEntity",Seq("ASPP2"),mentions)
    hasPositiveRegulationByEntity("Mek","BioChemicalEntity",Seq("ASPP2"),mentions)
  }
  sent10a should "contain one phosphorylation and two regulations" in {
    val mentions = getBioMentions(sent10a)
    hasEventWithArguments("Phosphorylation", List("ASPP2"), mentions)
    mentions.filter(_ matches "Positive_regulation") should have size (2)
    hasPositiveRegulationByEntity("Ras","BioChemicalEntity",Seq("ASPP2"),mentions)
    hasPositiveRegulationByEntity("Mek","BioChemicalEntity",Seq("ASPP2"),mentions)
  }

  // Number-sensitive search works with cause controlleds but not triggered regulation plurals
  val sent11 = "Ras and Mek are in proximity, and ASPP2 phosphorylates them."
  sent11 should "contain two phosphorylation and two regulations" in {
    val mentions = getBioMentions(sent11)
    mentions.filter(_ matches "Phosphorylation") should have size (2)
    hasEventWithArguments("Phosphorylation", List("Ras"), mentions) should be (true)
    hasEventWithArguments("Phosphorylation", List("Mek"), mentions) should be (true)
    mentions.filter(_ matches "Positive_regulation") should have size (2)
    hasPositiveRegulationByEntity("ASPP2","Phosphorylation",Seq("Ras"),mentions)
    hasPositiveRegulationByEntity("ASPP2","Phosphorylation",Seq("Mek"),mentions)
  }

  // Number-sensitive search works with activation controllers, but plurals are forbidden.
  val sent12 = "Ras is in proximity, and it activates ASPP2."
  sent12 should "contain a Positive_activation" in {
    val mentions = getBioMentions(sent12)
    mentions.filter(_ matches "ActivationEvent") should have size (1)
    hasEventWithArguments("Positive_activation", List("Ras", "ASPP2"), mentions) should be (true)
  }

  // Number-sensitive search works with activation controlleds, but plurals are forbidden.
  val sent13 = "Mek is in proximity, and ASPP2 activates it."
  sent13 should "contain one activation and one regulation" in {
    val mentions = getBioMentions(sent13)
    mentions.filter(_ matches "ActivationEvent") should have size (1)
    hasEventWithArguments("Positive_activation", List("ASPP2", "Mek"), mentions) should be (true)
  }

  // Sane noun phrases should be matched
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

  // Ignore noun phrases that can't have BioChemicalEntity antecedents
  val sent15 = "Ras is common, and a mouse binds GTP."
  sent15 should "not contain any events" in {
    val mentions = getBioMentions(sent15)
    mentions filter (_ matches "Event") should have size (0)
    mentions should have size (3)
  }

   // Ignore anything two sentences prior when searching for antecedents.
   val sent16 = "Ras is common. This is an intervening sentence. It binds Mek."
   sent16 should "not contain any events" in {
     val mentions = getBioMentions(sent16)
     mentions filter (_ matches "Event") should have size (0)
   }

  // Can find an antecedent mention between start of event mention and start of text bound mention
  val sent17 = "ASPP2 is common, and Ras binds the Mek protein."
  sent17 should "contain a single binding between Mek and Ras" in {
    val mentions = getBioMentions(sent17)
    hasEventWithArguments("Binding", List("Ras", "Mek"), mentions) should be (true)
    hasEventWithArguments("Binding", List("Ras", "ASPP2"), mentions) should be (false)
    hasEventWithArguments("Binding", List("Mek", "ASPP2"), mentions) should be (false)
  }

  // Events with invalid numbers of antecedents are ignored
  val sent18 = "ASPP2 and Ras are common, as is its binding."
  val sent18a = "ASPP2 and Ras are common, as is their activation."
  val sent18b = "The phosphorylation of ASPP2 and Ras is common, as is their upregulation."
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
    hasEventWithArguments("Phosphorylation", List("Ras"), mentions) should be (true)
  }

  val sent19 = "ASPP1 is common, and it binds Mek and Ras"
  sent19 should "contain two bindings, (ASPP1,Mek) and (ASPP1,Ras)" in {
    val mentions = getBioMentions(sent19)
    mentions filter (_ matches "Binding") should have size (2)
    mentions filter (_ matches "Event") should have size (2)
    hasEventWithArguments("Binding", List("ASPP1", "Mek"), mentions) should be (true)
    hasEventWithArguments("Binding", List("ASPP1", "Ras"), mentions) should be (true)
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

  val sent24 = "Previous work has shown that Gab1 is not a global substrate of Shp2, as complex formation between " +
    "Gab1 and Shp2 does not reduce the total EGF-induced tyrosine phosphorylation levels of Gab1 [15]. However there " +
    "have been several reports suggesting that Shp2 may specifically de-phosphorylate the tyrosine phosphorylation " +
    "sites on Gab1 that bind to p85, thus terminating recruitment of PI-3 kinase and EGF-induced activation of the " +
    "PI-3 kinase pathway"
  sent24 should "have a complex controller if it produces an ActivationEvent" in {
    val mentions = getBioMentions(sent24)
    val act = mentions.find(_ matches "ActivationEvent")
    if (act.nonEmpty) {
      val controller = act.get.arguments("controller").head
      (controller.antecedentOrElse(controller) matches "Complex") should be (true)
    }
  }

  val sent25 = "Another example can be given with mutated p53. The pivotal role of p53 as a tumor suppressor is " +
    "illustrated by the fact that this protein is found mutated in ∼50% of human cancers. In most cases, mutations " +
    "in p53 greatly increase the otherwise short half life of this protein and cause it to accumulate in tumor cells."
  sent25 should "not produce an error due to multiple antecedents" in {
    val mentions = getBioMentions(sent25)
    mentions.find(_.text == "p53").nonEmpty should be (true)
  }

  val sent26 = "Many RTKs interact directly with Grb2, some rely on Shc family adaptors to recruit Grb2, and others " +
    "do both    . While direct Grb2/RTK interactions involve binding of the Grb2 SH2 domain to pYXNX motifs, Shc " +
    "proteins interact with RTKs primarily through the binding of their N-terminal PTB domain to NPXpY motifs."
  sent26 should "not produce an error due to multiple antecedents" in {
    val mentions = getBioMentions(sent26)
    mentions.find(_.text == "Grb2").nonEmpty should be (true)
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

}
