package org.clulab.reach.conetxt.ml

import java.io._
import org.clulab.reach.context.ContextEngine
import org.clulab.reach.mentions._
import org.clulab.learning._
import org.clulab.reach.context.dataset.ContextType

class LinearContextEngine(val parametersFile:File, val normalizersFile:File) extends ContextEngine {

  // Load the trained data
  val classifier = LiblinearClassifier.loadFrom[Boolean, String](parametersFile.getAbsolutePath)
  val normalizers:ScaleRange[String] = ScaleRange.loadFrom(new FileReader(normalizersFile))

  var paperMentions:Option[Seq[BioTextBoundMention]] = None

  def classify(mention:BioMention):BioMention = paperMentions match {
    // Classify this event with all the context types in the paper
    case Some(contextMentions) =>
        // extract features and classify a pair
        val actualContextMentions:Seq[BioTextBoundMention] = contextMentions map {
            identity
        }

        // Convert the context mentions to a seq of ContextTypes
        val contextTypes:Seq[ContextType] = actualContextMentions.map(m => ContextType.parse(m.nsId))

        // Create the context map
        val contextMap:Map[String, Seq[String]] = contextTypes.map(t => (t.contextType.toString, t.id)).groupBy(t => t._1).mapValues(v => v.map(_._2)).mapValues(_.toSet.toSeq)

        // Assign context
        mention.context = Some(contextMap)

        // Return the mention with context
        mention

    case None => throw new RuntimeException("LinearContextEngine hasn't been called to infer")
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
