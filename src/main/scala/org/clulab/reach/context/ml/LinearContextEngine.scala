package org.clulab.reach.conetxt.ml

import java.io._
import org.clulab.reach.context.ContextEngine
import org.clulab.reach.mentions._
import org.clulab.learning._

class LinearContextEngine(val parametersFile:File, val normalizersFile:File) extends ContextEngine {

  // Load the trained data
  val classifier = LiblinearClassifier.loadFrom[Boolean, String](parametersFile.getAbsolutePath)
  val normalizers:ScaleRange[String] = ScaleRange.loadFrom(new FileReader(normalizersFile))

  var paperMentions:Option[Seq[BioTextBoundMention]] = None

  def classify(mention:BioMention):BioMention = {
    // TODO: Classify this event with all the context types in the paper
    mention
  }

  // Implementation of the ContextEngine trait
  def assign(mentions: Seq[BioMention]): Seq[BioMention] = paperMentions match {
    case Some(contextMentions) => mentions map classify
    case None => throw new RuntimeException("LinearContextEngine hasn't been called to infer")
  }

  def infer(mentions: Seq[BioMention]) {
    // We store the paper's mentions here to do classification later
    paperMentions = Some(mentions.filter{ case tb:BioTextBoundMention => true; case _ => false}.map(_.asInstanceOf[BioTextBoundMention]))
  }
  def update(mentions: Seq[BioMention]) {
    // Not doing anything here yet
  }
  ////////////////////////////////////////////
}
