package org.clulab.reach.conetxt.ml

import java.io._
import org.clulab.reach.context.ContextEngine
import org.clulab.reach.mentions._
import org.clulab.learning._
import org.clulab.reach.context.dataset.ContextType
import org.clulab.reach.context.dataset._
import org.clulab.learning._

class LinearContextEngine(val parametersFile:File, val normalizersFile:File) extends ContextEngine {

  // Load the trained data
  val classifier = LiblinearClassifier.loadFrom[String, String](parametersFile.getAbsolutePath)
  val normalizers:ScaleRange[String] = ScaleRange.loadFrom(new FileReader(normalizersFile))

  var paperMentions:Option[Seq[BioTextBoundMention]] = None
  var paperContextTypes:Option[Seq[ContextType]] = None
  var paperContextTypeCounts:Option[Map[String, Int]] = None


  def classify(eventMention:BioMention):BioMention = paperMentions match {
    // Classify this event with all the context types in the paper
    case Some(contextMentions) =>
        val contextTypes:Seq[ContextType] = paperContextTypes.get.filter{
            t =>
                val mentions = contextMentions.filter(m => ContextType.parse(m.nsId) == t)

                // Create feature pairs
                val instances = FeatureExtractor.extractFeatures(eventMention.document, Seq(eventMention.asInstanceOf[BioEventMention]), mentions)
                // Get the type frequency
                val contextTypeCount:Int = paperContextTypeCounts.get.apply(t.id)
                // Make the datum instance for classification
                val datum = FeatureExtractor.mkRVFDatum(instances, contextTypeCount, "true") // Label doesn´t matter here
                // Normalize the datum
                val scaledFeats =  Datasets.svmScaleDatum(datum.featuresCounter, normalizers)
                val scaledDatum = new RVFDatum(datum.label, scaledFeats)
                // Classify it
                val isContext:Boolean = classifier.classOf(scaledDatum) == "true"

                // If it's context, we keep it :)
                isContext
        }

        // Create the context map
        val contextMap:Map[String, Seq[String]] = contextTypes.map(t => (t.contextType.toString, t.id)).groupBy(t => t._1).mapValues(v => v.map(_._2)).mapValues(_.toSet.toSeq)

        // Assign context
        eventMention.context = Some(contextMap)

        // Return the mention with context
        eventMention

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

    // Get the contexttypes in the document
    paperContextTypes = Some(paperMentions.get.map(m => ContextType.parse(m.nsId)))

    // Compute the context type counts
    paperContextTypeCounts = Some(paperMentions.get.map(_.nsId).groupBy(identity).mapValues(_.size))
  }
  def update(mentions: Seq[BioMention]) {
    // Not doing anything here yet
  }
  ////////////////////////////////////////////
}
