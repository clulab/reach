package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import org.clulab.reach.mentions._
import TestUtils._

class TestAssociationEvents extends FlatSpec with Matchers {
  val sent1 = "We found that frailty and pre-frailty were associated with significantly elevated CRP and IL-6 levels across all geographical settings and among community and institutionalized participants."

  val sent2 = "Frailty (SMD = 1.12, 95%CI: 0.27–2.13, p = 0.01; I2 = 99%) and pre-frailty (SMD = 0.56, 95%CI: 0.00–1.11, p = 0.05; I2 = 99%) were associated with higher serum levels of IL-6 versus robust participants."
  val sent3 = "A direct association between frailty and elevated levels of inflammation, as marked by elevated interleukin-6 (IL-6)"
  val sent4 = "frailty and pre-frailty are associated with higher inflammatory parameters and in particular CRP and IL-6"
  val sent5 = "Frailty and pre-frailty are associated with higher CRP and IL 6."
  val sent6 = "There is evidence that anti-inflammatory interleukins (ILs) such as IL-10 and IL-4, although implicated to a lesser extent, also associate with the frailty phenotype"
  val sent7 = "frailty is associated with alterations in the concentration of pro-inflammatory molecules."
  val sent8 = "Frailty-associated physiological dysregulation"
  val sent9 = "But when inflammation becomes chronic, often associated with aging or diseases"
  val sent10 = "frailty and pre-frailty are associated with higher inflammatory parameter levels, in particular, CRP and IL6"
  val sent11 = "Upregulation of pro-inflammatory cytokines has not only been associated with increased morbidity and mortality in older adults but also has been linked to frailty"
  val sent12 = "Aging was significantly associated with higher fibrinogen (p=0.04) and D-dimer levels (p=0.01) but only among NF subjects."
  val sent13 = "Upregulation of cytokines such as interleukin-1 (IL-1), interleukin-6 (IL-6) and tumor necrosis factor-α (TNF-α) that contribute to systemic inflammation have been independently associated with increased morbidity and mortality in older adults"
  val sent14 = "inflammatory index, an additive index of serum IL-6 and soluble TNF-α receptor −1 (TNFR1) has been shown not only to best capture age-associated chronic inflammation but also predict mortality in older adults"
  val sent15 = "age was positively associated with TNFR1 (r=0.22; p=0.02), TNFR2 (r=0.25; p=0.02) and the inflammatory index (r=0.28, p=0.008) but not IL-6 (r=0.05; p=0.5)"
  val sent16 = "Age was positively associated with both fibrinogen (r=0.21; p=0.04) and D-dimer (r=0.27; p=0.01) (data not shown)"
  val sent17 = "Age was associated with increased TNFR1 and TNFR2"
  val sent18 = "we did not find IL-6 or CRP to be significantly associated with age"
  val sent19 = "Aging was associated with increasing inflammatory index score in our study"
  val sent20 = "Elevated levels of IL-6 have been linked to multiple age-associated conditions, such as atherosclerosis, dementia and frailty"
  val sent21 = "Frailty in older adults has been associated with superoxide anion overproduction by nicotinamide adenine dinucleotide phosphate-oxidase (NADPH) oxidase and low-grade chronic inflammation"
  val sent22 = "We did find aging to be not only associated with both these coagulation markers but also more strongly associated with a pro-thrombotic state than frailty status"
  val sent23 = "subclinically higher levels of serum immune mediators in older adults relative to youngsters, both groups devoid of overt infectious disease, is associated with the pathophysiology of chronic conditions such as frailty"
  val sent24 = "Enhanced levels of circulating immune mediators such as IL-6 are often taken as a surrogate for “inflammageing” and are consistently associated with the frail phenotype and mortality"
  val sent25 = "There is evidence that anti-inflammatory interleukins (ILs) such as IL-10 and IL-4, although implicated to a lesser extent, also associate with the frailty phenotype [19–21]"
  val sent26 = "other reports have associated augmented serum concentrations of IL-6 with lower gait speed and reduced muscle strength in a context of frailty"


  sent1 should "contain four association events" in {
    val mentions = getBioMentions(sent1)
    mentions.filter(_.label == "Association") should have size 6
  }

  sent2 should "contain four association events" in {
    val mentions = getBioMentions(sent2)
    mentions.filter(_.label == "Association") should have size 4
  }

  sent3 should "contain one association event" in {
    val mentions = getBioMentions(sent3)
    mentions.filter(_.label == "Association") should have size 1
  }

  sent4 should "contain four association events" in {
    val mentions = getBioMentions(sent4)
    mentions.filter(_.label == "Association") should have size 4
  }

  sent5 should "contain four association events" in {
    val mentions = getBioMentions(sent5)
    mentions.filter(_.label == "Association") should have size 4
  }

  sent6 should "contain two association events" in {
    val mentions = getBioMentions(sent6)
    mentions.filter(_.label == "Association") should have size 2
  }

  sent7 should "contain one association event" in {
    val mentions = getBioMentions(sent7)
    mentions.filter(_.label == "Association") should have size 1
  }

  sent8 should "contain one association event" in {
    val mentions = getBioMentions(sent8)
    mentions.filter(_.label == "Association") should have size 1
  }

  sent9 should "contain two association events" in {
    val mentions = getBioMentions(sent9)
    mentions.filter(_.label == "Association") should have size 2
  }

  sent10 should "contain four association events" in {
    val mentions = getBioMentions(sent10)
    mentions.filter(_.label == "Association") should have size 4
  }

  sent11 should "contain two association events" in {
    val mentions = getBioMentions(sent11)
    mentions.filter(_.label == "Association") should have size 2
  }

  sent12 should "contain two association events" in {
    val mentions = getBioMentions(sent12)
    mentions.filter(_.label == "Association") should have size 2
  }

  sent13 should "contain six association events" in {
    val mentions = getBioMentions(sent13)
    mentions.filter(_.label == "Association") should have size 6
    mentions.filter(_.label == "Association") should have size 6
  }

  sent14 should "contain one association event" in {
    val mentions = getBioMentions(sent14)
    mentions.filter(_.label == "Association") should have size 1
  }

  sent15 should "contain four association events" in {
    val mentions = getBioMentions(sent15)
    mentions.filter(_.label == "Association") should have size 4
  }

  sent16 should "contain two association events" in {
    val mentions = getBioMentions(sent16)
    mentions.filter(_.label == "Association") should have size 2
  }

  sent17 should "contain two association events" in {
    val mentions = getBioMentions(sent17)
    mentions.filter(_.label == "Association") should have size 2
  }

  sent18 should "contain two association event" in {
    val mentions = getBioMentions(sent18)
    mentions.filter(_.label == "Association") should have size 2
  }

  sent19 should "contain one association event" in {
    val mentions = getBioMentions(sent19)
    mentions.filter(_.label == "Association") should have size 1
  }

  sent20 should "contain three association events" in { // TODO consider "link" events in this example
    val mentions = getBioMentions(sent20)
    mentions.filter(_.label == "Association") should have size 3
  }

  sent21 should "contain one association event" in {
    val mentions = getBioMentions(sent21)
    mentions.filter(_.label == "Association") should have size 1
  }

  sent23 should "contain one association event" in { // TODO very complex sentence
    val mentions = getBioMentions(sent23)
    mentions.filter(_.label == "Association") should have size 1
  }

  sent24 should "contain two association events" in {
    val mentions = getBioMentions(sent24)
    mentions.filter(_.label == "Association") should have size 2
  }

  sent25 should "contain two association events" in {
    val mentions = getBioMentions(sent25)
    mentions.filter(_.label == "Association") should have size 2
  }

  sent26 should "contain two association events" in {
    val mentions = getBioMentions(sent26)
    mentions.filter(_.label == "Association") should have size 2
  }
}
