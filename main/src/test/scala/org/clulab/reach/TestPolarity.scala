package org.clulab.reach

import org.scalatest.{FlatSpec, Matchers}
import org.clulab.reach.mentions._
import TestUtils._
import com.typesafe.scalalogging.LazyLogging

class TestPolarity extends FlatSpec with Matchers{

//  val sen1 = "S-phase kinase associated protein 2 (Skp2) is a member of mammalian F-box proteins, which displays S-phase-promoting function through ubiquitin mediated proteolysis of the CDK inhibitor p27."
//  sen1 should "contain a negative activation" in {
//    val mentions = getBioMentions(sen1)
//    info(s"Num mentions: ${mentions.size}")
//
//    hasNegativeActivation("Skp2", "p27", mentions)
//  }

//  val sen2 = """ABT-737, a small molecule BH3-only mimetic developed by Abbott Laboratories, specifically targets the antiapoptotic bcl-2 members Bcl-2, Bcl-xL and Bcl-w."""
//  sen2 should "have an inhihbition" in {
//    val mentions = getBioMentions(sen2).filter(_ matches "ComplexEvent")
//
//    // TODO: Write a function to identify the specific event
//    info(s"Num mentions: ${mentions.size}")
//    // TODO make sure this labels is the correct for "inhibition"
//    mentions map (_.displayLabel) should contain ("Negative_regulation")
//  }
//
//  val sen3 = """We suggest that this difference is because of reduced autocrine and paracrine CD40 activation by the lower level of CD40L expressed on plasmid transduced CLL cells and the shorter time - course of vaccine manufacture (5 h versus 3 days)."""
//  sen3 should "have double negations, hence positive polarity" in {
//
//    val mentions = getBioMentions(sen3).filter(_ matches "ComplexEvent")
//    // TODO: Write a function to identify the specific event
//    info(s"Num mentions: ${mentions.size}")
//
//
//    mentions map (_.displayLabel) should contain ("Positive_regulation")
//  }
//
//  val sen4 = """These observations support the model that Sufu is the major negative regulator of FP induction and that activated Smo restricts the inhibitory function of Sufu through Kif7."""
//  sen4 should "have a negative regulation due to \"restricts the ... function of\" AND positive polarity" in {
//    val mentions = getBioMentions(sen4).filter(_ matches "ComplexEvent")
//    // TODO: Write a function to identify the specific event
//    info(s"Num mentions: ${mentions.size}")
//
//    // TODO: Is this the correct label?
//    mentions map (_.displayLabel) should contain ("Positive_regulation")
//  }
//
//
//  val sen5 = """Previous studies have shown that EGFR signaling mediated by the MEK and ERK and PI3K and AKT pathways is essential for RPE cell proliferation and survival [XREF_BIBR]."""
//  sen5 should "ERK should be downstream of EGFR" in {
//    val mentions = getBioMentions(sen5).filter(_ matches "ComplexEvent")
//
//    val mention = mentions filter {
//      m =>
//        val controller = m.arguments("controller").head
//        val controlled = m.arguments("controlled").head
//
//        if(controller.text == "EGFR" && controlled.text == "ERK")
//          true
//        else
//          false
//    }
//
//    mention should have size 1
//  }
//
//  val sen6 = """At the whole muscle level, the same two week HFD protocol did not result in significantly diminished insulin stimulated Akt activation 9."""
//  sen6 should "Have positive polarity" in {
//    val mentions = getBioMentions(sen5).filter(_ matches "ComplexEvent")
//
//    // TODO: Write a function to identify the specific event
//    mentions.head.displayLabel should startWith ("Positive")
//  }

  val sen7 = """The effect of this modification on FoxO1 localization and activity is controversial with CDK1 reported to cause nuclear localization and transcriptional activation, and CDK2 reported to cause cytoplasmic localization and inhibition of FoxO1."""

  // Broken syntax
  ignore should "have a negative activation of FoxO1 by CDK2" in {
    val mentions = getBioMentions(sen7)
    info(s"Num mentions: ${mentions.size}")

    hasNegativeActivation("CDK2", "FoxO1", mentions) should be (true)
  }

  // TODO: Not sure this test is correct because I don't know about the polarity of "modulating"
  val sen8 = """Taken together, these results suggest that the benefits of RET are associated with increased autophagy activity and reduced apoptosis of muscle cells by modulating IGF-1 and its receptors, the Akt and mTOR and Akt and FOXO3a signaling pathways in aged skeletal muscles."""

  sen8 should "have a positive activation of AKT by RET" in {
    val mentions = getBioMentions(sen8)
    hasPositiveActivation("RET", "AKT", mentions) should be (true)
  }

  val sen9 = """Biliverdin reductase-A functions as a scaffold protein for the activation of ERK by MEK1/2 and of Elk1 by ERK."""

  sen9 should "have a positive activation of ERK by MEK1/2" in {
    val mentions = getBioMentions(sen9)
    hasPositiveActivation("ERK", "MEK1/2", mentions) should be (true)
  }

  val sen10 = """Potential downstream targets of activated TAK1 include MKK4 and JNKK and MKK3 and MAPKK6, which directly activate c-Jun N-terminal kinase (JNK) and p38 MAP kinase, respectively [XREF_BIBR, XREF_BIBR]"""

  sen10 should "have a positive activation of c-Jun N-terminal kinase by MKK4" in {
    val mentions = getBioMentions(sen10)
    hasPositiveActivation("MKK4", "c-Jun N-terminal kinase", mentions) should be (true)
  }

  val sen11 = """It has been recently shown that MAPK activation slows down mitochondrial oxidative metabolism by repressing the MITF and PGC1alpha pathway [XREF_BIBR].'"""

  sen11 should "have a negative activation of Mitf by MAPK (ERK?)" in {
    val mentions = getBioMentions(sen11)
    hasNegativeActivation("MAPK", "Mitf", mentions) should be (true)
  }

  val sen12 = """The protein kinase mammalian target of rapamycin (mTOR) regulates mRNA translation and is inhibited by rapamycin."""

  sen12 should "have a negative activation of rapamycin by mTOR" in {
    val mentions = getBioMentions(sen12)
    hasNegativeActivation("rapamycin", "mammalian target of rapamycin", mentions) should be (true)
  }

  val sen13 = """Of these, 6 involved wortmannin or LY-294002 (inhibitors of phosphoinositide 3-kinase (PI3K)) or rapamycin (an inhibitor of the mammalian target of rapamycin complex 1 (mTORC1))."""

  sen13 should "have a negative activation of rapamycin by mammalian target of rapamycin" in {
    val mentions = getBioMentions(sen13)
    hasNegativeActivation("rapamycin", "mammalian target of rapamycin", mentions) should be (true)
  }

  val sen14 = """We previously showed that IGF1R knockdown blocked survival of prostate cancer cells in which Akt activation was deregulated by PTEN loss."""

  ignore should "have a negative activation of AKT by PTEN" in {
    val mentions = getBioMentions(sen14)
    hasNegativeActivation("PTEN", "AKT", mentions) should be (true)
  }

  val sen16 = """XREF_BIBR, XREF_BIBR By using a high content phenotypic screen (HCS) to identify selective inhibitors of IL-6 induced activation of the STAT3 pathway, 11 we identified the quinazoline 11a (XREF_FIG)."""

  sen16 should "have a positive activation of IL-6 by STAT3" in {
    val mentions = getBioMentions(sen16)
    hasPositiveActivation("IL-6", "STAT3", mentions) should be (true)
  }

  val sen15 = """In agreement with the ABCA1 expression level, the ApoAI mediated cholesterol efflux was significantly lower in macrophages treated with ApoE-free lipoproteins than in those treated with ApoE containing lipoproteins."""

  // Broken syntax
  ignore should "have a positive activation of cholesterol by ApoAI" in {
    val mentions = getBioMentions(sen15)
    hasPositiveActivation("ApoAI", "cholesterol", mentions) should be (true)
  }

  val sen17 = """Finally, we compared wild-type IRF3 and IRF3 5SD for their effects on TGF-beta target genes, and measured basal, i.e. autocrine TGF-beta-dependent, and TGF-beta-induced mRNA expression of Smad7, p15 Ink4B and p21 Cip1, three direct Smad3 targets that are induced by TGF-beta, and c-Myc, which is directly repressed by Smad3 in response to autocrine or added TGF-beta."""

  sen17 should "have a positive activation of SMAD3 by TGFbeta" in {
    val mentions = getBioMentions(sen17)
    hasPositiveActivation("TGFbeta", "SMAD3", mentions) should be (true)
  }

  val sen18 = """The Wip1 gene is frequently amplified or overexpressed in human cancers, promoting tumor growth by switching off major checkpoint kinases and p53."""

  sen18 should "have a negative activation of p53 by Wip1" in {
    val mentions = getBioMentions(sen18)
    hasNegativeActivation("Wip1", "p53", mentions) should be (true)
  }

  val sen19 = """The TSC2 and TSC1 tuberous sclerosis complex, acting downstream of AKT, negatively regulates mTORC1 by inhibiting the GTPase activity of Rheb (Ras Homolog Enriched in Brain), which is a positive regulator of mTORC1."""

  sen19 should "have a negative activation of Rheb by TSC2" in {
    val mentions = getBioMentions(sen19)
    hasNegativeActivation("TSC2", "Rheb", mentions) should be (true)
  }

  val sen20 = """These data indicate that niacin accelerates hepatic intracellular post-translational degradation of apoB by selectively reducing triglyceride synthesis (through inhibiting both fatty acid synthesis and fatty acid esterification to produce TG) without affecting ALLN-inhibitable protease- or MTP mediated intracellular apoB processing, resulting in decreased apoB secretion and hence lower circulating levels of the atherogenic lipoproteins."""

  sen20 should "have a positive activation of ApoB by MTP" in {
    val mentions = getBioMentions(sen20)
    hasPositiveActivation("MTP", "apoB", mentions) should be (true)
  }

  val sen21 = """In addition, Knockdown of CDK5 induced growth inhibition and knockdown of TP53 reduced silencing CDK5 mediated growth inhibition in the presence or absence of paclitaxel (XREF_FIG)."""

  sen21 should "have a positive activation of TP53 by CDK5" in {
    val mentions = getBioMentions(sen21)
    hasPositiveActivation("CDK5", "TP53", mentions) should be (true)
  }

  val sen22 = """Insulin inhibits adipocyte hormone sensitive lipase and activates lipoprotein lipase [XREF_BIBR, XREF_BIBR]."""

  sen22 should "have a positive activation of lipoprotein lipase by insulin" in {
    val mentions = getBioMentions(sen22)
    hasPositiveActivation("Insulin", "lipoprotein lipase", mentions) should be (true)
  }
}
