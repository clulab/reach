package org.clulab.reach

import org.clulab.reach.TestUtils._
import org.scalatest.{FlatSpec, Matchers}


class TestPolarity extends FlatSpec with Matchers{

  def activationBehaivor(sentence:String, controller:String, controlled:String, positive:Boolean, ignored:Boolean = false): Unit ={

    val description = s"contain a ${if(positive) "positive" else "negative"} activation of $controlled by $controller"

    if(!ignored) {
      it should description in {
        val mentions = getBioMentions(sentence)
        info(s"Num mentions: ${mentions.size}")

        val result =
          if (positive)
            hasPositiveActivation(controller, controlled, mentions)
          else
            hasNegativeActivation(controller, controlled, mentions)

        result should be(true)
      }

    }
    else{
      it should description ignore {}
    }




  }
  def positiveActivationBehavior(sentence:String, controller:String, controlled:String, ignored:Boolean = false): Unit = activationBehaivor(sentence, controller, controlled, positive = true, ignored)
  def negativeActivationBehavior(sentence:String, controller:String, controlled:String, ignored:Boolean = false): Unit = activationBehaivor(sentence, controller, controlled, positive = false, ignored)

  def regulationBehavior(sentence:String, controllerEntity:String, controlledLabel:String, controlledArgs:Seq[String], positive:Boolean, ignored:Boolean = false): Unit ={

    val description = s"contain a ${if(positive) "positive" else "negative"} regulation of ${controlledArgs.mkString(",")} by $controllerEntity"

    if(!ignored) {
      it should description in {
        val mentions = getBioMentions(sentence)
        info(s"Num mentions: ${mentions.size}")

        val result =
          if (positive)
            hasPositiveRegulationByEntity(controllerEntity, controlledLabel, controlledArgs, mentions)
          else
            hasNegativeRegulationByEntity(controllerEntity, controlledLabel, controlledArgs, mentions)

        result should be(true)
      }
    }
    else{
      it should description ignore {}
    }


  }
  def positiveRegulationBehavior(sentence:String, controllerEntity:String, controlledLabel:String, controlledArgs:Seq[String], ignored:Boolean = false) = regulationBehavior(sentence, controllerEntity, controlledLabel, controlledArgs, positive = true, ignored)
  def negativeRegulationBehavior(sentence:String, controllerEntity:String, controlledLabel:String, controlledArgs:Seq[String], ignored:Boolean = false) = regulationBehavior(sentence, controllerEntity, controlledLabel, controlledArgs, positive = false, ignored)

  // Broken syntax
  val sen7 = """The effect of this modification on FoxO1 localization and activity is controversial with CDK1 reported to cause nuclear localization and transcriptional activation, and CDK2 reported to cause cytoplasmic localization and inhibition of FoxO1."""
  sen7 should behave like negativeActivationBehavior(sen7, "CDK2", "FoxO1", ignored = true)

  // TODO: Not sure this test is correct because I don't know about the polarity of "modulating"
  val sen8 = """Taken together, these results suggest that the benefits of RET are associated with increased autophagy activity and reduced apoptosis of muscle cells by modulating IGF-1 and its receptors, the Akt and mTOR and Akt and FOXO3a signaling pathways in aged skeletal muscles."""
  sen8 should behave like positiveActivationBehavior(sen8, "RET", "AKT", ignored = true)

  val sen9 = """Biliverdin reductase-A functions as a scaffold protein for the activation of ERK by MEK1/2 and of Elk1 by ERK."""
  sen9 should behave like positiveActivationBehavior(sen9, "MEK1/2", "ERK")

  val sen10 = """Potential downstream targets of activated TAK1 include MKK4 and JNKK and MKK3 and MAPKK6, which directly activate c-Jun N-terminal kinase (JNK) and p38 MAP kinase, respectively [XREF_BIBR, XREF_BIBR]"""
  sen10 should behave like positiveActivationBehavior(sen10, "MKK4", "c-Jun N-terminal kinase")

  val sen11 = """It has been recently shown that MAPK activation slows down mitochondrial oxidative metabolism by repressing the MITF and PGC1alpha pathway [XREF_BIBR].'"""
  sen11 should behave like negativeActivationBehavior(sen11, "MAPK", "Mitf", ignored = true)

  val sen12 = """The protein kinase mammalian target of rapamycin (mTOR) regulates mRNA translation and is inhibited by rapamycin."""
  sen12 should behave like negativeActivationBehavior(sen12, "rapamycin", "mammalian target of rapamycin")

  val sen13 = """Of these, 6 involved wortmannin or LY-294002 (inhibitors of phosphoinositide 3-kinase (PI3K)) or rapamycin (an inhibitor of the mammalian target of rapamycin complex 1 (mTORC1))."""
  sen13 should behave like negativeActivationBehavior(sen13, "rapamycin", "mammalian target of rapamycin")

  val sen14 = """We previously showed that IGF1R knockdown blocked survival of prostate cancer cells in which Akt activation was deregulated by PTEN loss."""
  sen14 should behave like negativeActivationBehavior(sen14, "PTEN", "AKT", ignored = true)

  val sen16 = """XREF_BIBR, XREF_BIBR By using a high content phenotypic screen (HCS) to identify selective inhibitors of IL-6 induced activation of the STAT3 pathway, 11 we identified the quinazoline 11a (XREF_FIG)."""
  sen16 should behave like positiveActivationBehavior(sen16, "STAT3", "IL-6", ignored = true)

  // Broken syntax
  val sen15 = """In agreement with the ABCA1 expression level, the ApoAI mediated cholesterol efflux was significantly lower in macrophages treated with ApoE-free lipoproteins than in those treated with ApoE containing lipoproteins."""
  sen15 should behave like positiveActivationBehavior(sen15, "ApoAI", "cholesterol", ignored = true)

  val sen17 = """Finally, we compared wild-type IRF3 and IRF3 5SD for their effects on TGF-beta target genes, and measured basal, i.e. autocrine TGF-beta-dependent, and TGF-beta-induced mRNA expression of Smad7, p15 Ink4B and p21 Cip1, three direct Smad3 targets that are induced by TGF-beta, and c-Myc, which is directly repressed by Smad3 in response to autocrine or added TGF-beta."""
  sen17 should behave like positiveActivationBehavior(sen17, "TGF-beta", "Smad3")

  val sen18 = """The Wip1 gene is frequently amplified or overexpressed in human cancers, promoting tumor growth by switching off major checkpoint kinases and p53."""
  sen18 should behave like negativeActivationBehavior(sen18, "Wip1", "p53")

  val sen19 = """The TSC2 and TSC1 tuberous sclerosis complex, acting downstream of AKT, negatively regulates mTORC1 by inhibiting the GTPase activity of Rheb (Ras Homolog Enriched in Brain), which is a positive regulator of mTORC1."""
  sen19 should behave like negativeActivationBehavior(sen19, "TSC2", "Rheb")

  val sen20 = """These data indicate that niacin accelerates hepatic intracellular post-translational degradation of apoB by selectively reducing triglyceride synthesis (through inhibiting both fatty acid synthesis and fatty acid esterification to produce TG) without affecting ALLN-inhibitable protease- or MTP mediated intracellular apoB processing, resulting in decreased apoB secretion and hence lower circulating levels of the atherogenic lipoproteins."""
  sen20 should behave like positiveActivationBehavior(sen20, "MTP", "apoB")

  val sen21 = """In addition, Knockdown of CDK5 induced growth inhibition and knockdown of TP53 reduced silencing CDK5 mediated growth inhibition in the presence or absence of paclitaxel (XREF_FIG)."""
  sen21 should behave like positiveActivationBehavior(sen21, "CDK5", "TP53")


  // Tests based on Ben's observations from https://docs.google.com/spreadsheets/d/1pqhs8ZX6uByLTSrCMYHOOyEwe5ZCEWjFGR9T4uCExTk/edit#gid=151195583

  val sen1 = "S-phase kinase associated protein 2 (Skp2) is a member of mammalian F-box proteins, which displays S-phase-promoting function through ubiquitin mediated proteolysis of the CDK inhibitor p27."
  sen1 should behave like positiveActivationBehavior(sen1, "Skp2", "p27", ignored = true)

  val sen22 = """Insulin inhibits adipocyte hormone sensitive lipase and activates lipoprotein lipase [XREF_BIBR, XREF_BIBR]."""
  sen22 should behave like positiveActivationBehavior(sen22, "Insulin", "lipoprotein lipase")

  val sen23 = """SIRT1 activation appears to induce post-translational modifications of the p53 protein, affecting the ability of this protein to regulate the expression of downstream gene targets important for regulating DNA damage repair and apoptosis."""
  sen23 should behave like negativeActivationBehavior(sen23, "SIRT1", "p53", ignored = true)

  val sen24 = """This corresponded with a decreased number of cells in G1-phase and increased number of cells in S-phase as revealed by FACS analysis, consistent with the hypothesis that the decline of Skp2 promotes the accumulation of p21 and p27 in lovastatin arrested cells."""
  sen24 should behave like negativeActivationBehavior(sen24, "Skp2", "p21", ignored = true)

  val sen25 = """ABT-737, a small molecule BH3-only mimetic developed by Abbott Laboratories, specifically targets the antiapoptotic bcl-2 members Bcl-2, Bcl-xL and Bcl-w."""
  sen25 should behave like negativeActivationBehavior(sen25, "ABT-737", "Bcl-2", ignored = true)

  val sen26 = """Previous studies have shown that EGFR signaling mediated by the MEK and ERK and PI3K and AKT pathways is essential for RPE cell proliferation and survival [XREF_BIBR]."""
  sen26 should behave like negativeActivationBehavior(sen26, "ERK", "EGFR", ignored = true)

  // Doesn't seem to contain the event from row 9 on Ben's observations
  val sen27 = """We suggest that this difference is because of reduced autocrine and paracrine CD40 activation by the lower level of CD40L expressed on plasmid transduced CLL cells and the shorter time - course of vaccine manufacture (5 h versus 3 days)."""
  sen27 should behave like positiveActivationBehavior(sen27, "CD40L", "CD40-Ig", ignored = true)

  val sen28 = """These observations support the model that Sufu is the major negative regulator of FP induction and that activated Smo restricts the inhibitory function of Sufu through Kif7."""
  sen28 should behave like negativeActivationBehavior(sen28, "Smo", "Sufu", ignored = true)

  val sen29 = """At the whole muscle level, the same two week HFD protocol did not result in significantly diminished insulin stimulated Akt activation 9."""
  sen29 should behave like positiveActivationBehavior(sen29, "insulin", "Akt", ignored = true)

  val sen30 = """EGF, but not TGF alpha, efficiently induces degradation of the EGF receptor (EGFR)."""
  sen30 should behave like positiveActivationBehavior(sen30, "EGF", "EGF receptor", ignored = true)

  val sen31 = """Thus, inactivation of p53 signalling, observed after multiple exposures to UVB and heat, may be attributed to the SIRT1 mediated post-translational modification of the p53 protein."""
  sen31 should behave like negativeActivationBehavior(sen31, "SIRT1", "p53", ignored = true)

  val sen32 = """Mcl1 destruction relieves its inhibition of Bax and Bak (pro apoptotic factors), allowing them to bind the mitochondrial outer membrane to induce an apoptotic cell death [XREF_BIBR, XREF_BIBR, XREF_BIBR]."""
  sen32 should behave like negativeActivationBehavior(sen32, "Mcl1", "Bax", ignored = true)

  val sen33 = """In particular, the BCL-2 selective inhibitor, ABT-199 (Venetoclax) and ABT-263 (Navitoclax), which also targets BCL-2, BCL-X L and BCL-w, have been employed successfully for treating haematological malignancies."""
  sen33 should behave like negativeActivationBehavior(sen33, "Navitoclax", "BCL-w", ignored = true)

  val sen34 = """Unexpectedly co-misexpression of Trbl and Akt in the fat body led to a significant reduction in total FoxO levels (XREF_FIG), suggesting that Trbl and Akt might act combinatorially to direct FoxO turnover."""
  sen34 should behave like negativeActivationBehavior(sen34, "Akt", "FoxO")

  val sen35 = """The mechanisms by which androgen receptor (AR) antagonists inhibit AR activity, and how their antagonist activity may be abrogated in prostate cancer that progresses after androgen deprivation therapy, are not clear."""
  sen35 should behave like positiveActivationBehavior(sen35, "androgen receptor", "AR", ignored = true)

  val sen36 = """6,7 Bax shuttling is also mediated by Bcl-2 and Mcl-1, 6 but could additionally involve Bcl-2 protein independent mechanisms."""
  sen36 should behave like negativeActivationBehavior(sen36, "Mcl-1", "Bax", ignored = true)

  val sen37 = """Inhibition of neutral endopeptidase protects endogenous ANP, and inhibition of angiotensin converting enzyme blocks angiotensin II production, whereas inhibition of both peptidases is required to protect endogenous bradykinin (BK)."""
  sen37 should behave like positiveActivationBehavior(sen37, "angiotensin converting enzyme", "angiotensin II")

  val sen38 = """Thus, mutations in PALB2 disrupt BRCA2 tumor suppression function and predispose carriers to similar cancers."""
  sen38 should behave like positiveActivationBehavior(sen38, "PALB2", "BRCA2", ignored = true)

  // Bad extraction of events
  val sen39 = """Previously, it was reported that Akt phosphorylated Hand2 in vitro and phosphorylation negatively regulated Hand2 DNA binding and activation of luciferase reporter gene expression XREF_BIBR."""
  sen39 should behave like positiveActivationBehavior(sen39, "Akt phosphorylated Hand2", "Hand2", ignored = true)

  val sen40 = """Negatively regulating IRFs include IRF4 that competitively inhibits IRF5 from binding to TLR, thereby inhibiting inflammatory responses."""
  sen40 should behave like negativeActivationBehavior(sen40, "IRF4", "IRF5")

  val sen41 = """MIF binds to the extracellular domain of EGFR and inhibits EGF induced EGFR activation."""
  sen41 should behave like positiveActivationBehavior(sen41, "EGF", "EGFR", ignored = true)

  val sen42 = """Moreover, the demonstration that silencing TAK1 in TNFalpha stimulated non malignant epithelial cells suppresses MUC1-C-mediated activation of IKKbeta and NF-kappaB, indicated that MUC1-C and TAK1 are functionally linked in the inflammatory response."""
  sen42 should behave like positiveActivationBehavior(sen42, "TAK1", "IKKbeta")

  val sen43 = """A prime example are the bcl-2 family proteins such as Bad and Bax whose activation and mitochondrial translocation are shown to induce cytochrome c release following apoptotic stimuli after cerebral ischemia XREF_BIBR, XREF_BIBR, XREF_BIBR, XREF_BIBR, XREF_BIBR, XREF_BIBR."""
  sen43 should behave like negativeActivationBehavior(sen43, "bcl-2", "cytochrome c", ignored = true)

  val sen44 = """Keap1 still modulates the redox sensitivity of Nrf2 by controlling the availability of free Nrf2 proteins."""
  sen44 should behave like negativeActivationBehavior(sen44,"Keap1", "Nrf2", ignored = true)

  val sen45 = """Nrf2 is mainly regulated at the level of its protein stability by the cytosolic protein Keap1, which functions as a substrate recruiting subunit of a Cullin3 E3 ubiquitin ligase to target Nrf2 for ubiquitination and subsequent degradation."""
  sen45 should behave like negativeActivationBehavior(sen45,"Keap1", "Nrf2" , ignored = true)

  val sen46 = """The inhibition of STAT3 activation by SOCS3 was presented in other human and animal studies in different inflammatory settings."""
  sen46 should behave like negativeActivationBehavior(sen46, "SOCS3", "STAT3", ignored = true)

  val sen47 = """In in vitro studies, estradiol significantly increased cell proliferation of A549 + ERalpha or A549 + ERbeta, which was significantly suppressed by selective ER modulators, tamoxifen or raloxifene."""
  sen47 should behave like positiveActivationBehavior(sen47, "ER", "ERbeta", ignored = true)

  val sen48 = """Here we demonstrate that the Pten heterozygosity in mouse embryonic fibroblasts enhances cell adhesion dependent PI3K and Akt signaling, cell spreading, and proliferation, while Pten and Mgat5 double mutant cells are normalized."""
  sen48 should behave like negativeActivationBehavior(sen48, "Pten", "Akt", ignored = true)

  val sen49 = """Using insulin resistant PDK1 (K465E and K465E) PH domain knock-in mice, we found that introducing the PTEN (+/-) mutation to slightly stimulate Akt restored normal insulin sensitivity."""
  sen49 should behave like negativeActivationBehavior(sen49, "PTEN", "Akt", ignored = true)

  val sen50 = """Noggin increased BMP-4 transcripts, suggesting autocrine control of BMP-4 expression."""
  sen50 should behave like negativeActivationBehavior(sen50, "Noggin", "BMP-4", ignored = true)

  val sen51 = """For example, the unique binding pocket on EloC where the N terminus of Cul2 interacts is an attractive target for a small molecule that would inhibit the VHL mediated HIF-1alpha downregulation pathway."""
  sen51 should behave like negativeActivationBehavior(sen51, "VHL", "HIF-1alpha", ignored = true)

  val sen52 = """Interestingly, we also found that Bcl-xL and Bcl-2 are not targeted by ABT-737 in either human oral cancer cell line (XREF_SUPPLEMENTARY)."""
  sen52 should behave like negativeActivationBehavior(sen52,"ABT-737", "Bcl-xL", ignored = true)

  val sen53 = """Taken together, we conclude that SRT1720, SRT2183, SRT1460, and resveratrol are not direct activators of SIRT1."""
  sen53 should behave like negativeActivationBehavior(sen53, "SRT1720", "SIRT1", ignored = true)

  val sen54 = """To assess how flavaglines compare to other agents, we examined Temsirolimus, a rapamycin derivative that targets the mTOR pathway, and has been investigated in numerous treatment regimens for AML and other cancers."""
  sen54 should behave like negativeActivationBehavior(sen54, "Temsirolimus", "mTOR", ignored = true)

  val sen55 = """After retinoic acid (RA) differentiation, TSC1 +/-cells maintained lower levels of TSC1 and hamartin and increased mTOR activity."""
  sen55 should behave like negativeActivationBehavior(sen55, "TSC1", "mTOR", ignored = true)

  val sen56 = """Alternatively, Hhip might modulate Shh signaling by sequestering it away from other receptors [XREF_BIBR]."""
  sen56 should behave like negativeActivationBehavior(sen56, "Hhip", "Shh", ignored = true)

  val sen57 = """The binding of IL-7 has also been shown to down-regulate IL-7Ralpha by modulating its gene expression."""
  sen57 should behave like positiveActivationBehavior(sen57, "IL-7", "IL-7Ralpha", ignored = true)

  val sen58 = """Caspase-3 primarily activates PARP; however, studies have additionally indicated that caspase-6 and caspase-7 are also able to cause PARP cleavage."""
  sen58 should behave like negativeActivationBehavior(sen58, "caspase-7", "PARP", ignored = true)

  val sen59 = """Phosphorylation of AKT at S473 was also increased by BRCA1-KD."""
  sen59 should behave like negativeRegulationBehavior(sen59, "BRCA1-KD", "Phosphorylation", Seq("AKT"), ignored = false)
}
