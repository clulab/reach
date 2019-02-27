package org.clulab.reach.context
import org.clulab.reach.mentions.BioMention
import org.ml4ai.data.classifiers.LinearSVMWrapper

class SVMContextEngine extends ContextEngine {
  val svmWrapper = new LinearSVMWrapper(null)
  val trainedSVMInstance = svmWrapper.loadFrom("/Users/shraddha/datascience/ScalaContext/src/main/resources/svmTrainedModel.dat")
  override def assign(mentions: Seq[BioMention]): Seq[BioMention] = {
    for (m <- mentions) {

    }
    null
  }

  override def infer(mentions: Seq[BioMention]): Unit = ()

  override def update(mentions: Seq[BioMention]): Unit = ()

}
