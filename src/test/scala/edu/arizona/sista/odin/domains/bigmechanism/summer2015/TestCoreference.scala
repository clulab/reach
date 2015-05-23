package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin.domains.bigmechanism.summer2015.TestUtils._
import org.scalatest.{Matchers, FlatSpec}

/**
 * Tests coreference-based events
 * Date: 5/22/15
 */
class TestCoreference extends FlatSpec with Matchers {
  val sent15 = "To address the effect of K-Ras ubiquitination on its binding to PI3K and Raf family members, either total G12V-K-Ras or the ubiquitinated subfraction of G12V-K-Ras was immunoprecipitated and the immunoprecipitates were probed with antibodies to detect associated Ras effector molecules."
  sent15 should "contain 2 binding events" in {
    val mentions = parseSentence(sent15)
    hasEventWithArguments("Ubiquitination", List("K-Ras"), mentions) should be (true)
    // TODO: this requires coref!
    hasEventWithArguments("Binding", List("K-Ras", "Raf"), mentions) should be (true)
    hasEventWithArguments("Binding", List("PI3K", "K-Ras"), mentions) should be (true)
  }
}
