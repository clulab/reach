package org.clulab.reach

import org.clulab.reach.TestUtils._
import org.scalatest.{FlatSpec, Matchers}

class TestAmountEvents extends FlatSpec with Matchers {

  val sent1 = "We tested the level of neurofibromin present in the sample."
  sent1 should "have an AmountEvent of neurofibromin" in {
    val mentions = getBioMentions(sent1)
    hasEventWithArguments("Amount", Seq("neurofibromin"), mentions) should be (true)
  }

  val sent2 = "Ets-1 upregulates the loss of MMP-9."
  sent2 should "have a Decrease_amount event of MMP-9" in {
    val mentions = getBioMentions(sent2)
    hasEventWithArguments("DecreaseAmount", Seq("MMP-9"), mentions) should be (true)
  }

  val sent3 = "The knockdown of p65 impaired the induction of CXCL10"
  sent2 should "have a Increase_amount event of CXCL10" in {
    val mentions = getBioMentions(sent2)
    hasEventWithArguments("IncreaseAmount", Seq("CXCL10"), mentions) should be (true)
  }
}
