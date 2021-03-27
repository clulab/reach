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

  sent1 should "contain four association events" in {
    val mentions = getBioMentions(sent1)
    mentions.filter(_.label == "Association") should have size (6)
  }

  sent2 should "contain two association events" in {
    val mentions = getBioMentions(sent2)
    mentions.filter(_.label == "Association") should have size (2)
  }

  sent3 should "contain one association event" in {
    val mentions = getBioMentions(sent3)
    mentions.filter(_.label == "Association") should have size (1)
  }

  sent4 should "contain four association events" in {
    val mentions = getBioMentions(sent4)
    mentions.filter(_.label == "Association") should have size (4)
  }

  sent5 should "contain four association events" in {
    val mentions = getBioMentions(sent5)
    mentions.filter(_.label == "Association") should have size(4)
  }

  sent6 should "contain two association events" in {
    val mentions = getBioMentions(sent6)
    mentions.filter(_.label == "Association") should have size(2)
  }
}
