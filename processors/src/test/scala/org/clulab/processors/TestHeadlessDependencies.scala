package org.clulab.processors

import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.struct.Interval
import org.clulab.utils.DependencyUtils
import org.scalatest.{FlatSpec, Matchers}

class TestHeadlessDependencies extends FlatSpec with Matchers {
  var proc:Processor = new BioNLPProcessor()

  "BioNLPProcessor" should "handle cycles in dependency tree robustly" in {
    val doc = proc.annotate("Other mechanisms involved in asthma physiopathology are the inhalation of drugs , as well as respiratory viruses [8] , which promote an immune response mediated by IgG antibodies .")

    val sent = doc.sentences.head
    val deps = sent.dependencies.get
    deps.roots.nonEmpty should be (true)

    val span = Interval(25, 26)
    noException shouldBe thrownBy (DependencyUtils.findHeadStrict(span, sent))
    // Keep an eye out for changes.  Some slightly subjective decisions are involved.
    val head = DependencyUtils.findHeadStrict(span, sent)
    head.get should be(25)
  }
}
