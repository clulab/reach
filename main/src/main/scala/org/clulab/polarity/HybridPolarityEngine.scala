package org.clulab.polarity
import org.clulab.polarity.ml.MLPolarityEngine
import org.clulab.reach.mentions.BioEventMention

/**
  * Base class to any hybrid polarity classification approach
  * @param mlEngine Instance with the ML learning based polarity engine to be used
  */
abstract class HybridPolarityEngine(mlEngine:MLPolarityEngine) extends PolarityEngine {

  /**
    * Predicate that determines which approach to be used for polarity classification based on <param>evt<param>'s properties
    * @param evt Event to be classified
    * @return True if the ML-based classifier is chosen, False otherwise
    */
  def useMLPolarityEngine(evt:BioEventMention):Boolean

  /**
    * This implementation steers classification to the appropriate polarity engine given the logical predicate.
    *
    * @param evt BioEventMention to be operated on
    * @return Outcome of polarity classification
    */
  override def computePolarity(evt: BioEventMention): Polarity =
    if(useMLPolarityEngine(evt))
      mlEngine.computePolarity(evt)
    else
      LinguisticPolarityEngine.computePolarity(evt)
}

class HybridLinguisticDeepLearingPolarityEngine(mlEngine:MLPolarityEngine) extends HybridPolarityEngine(mlEngine:MLPolarityEngine) {
  override def useMLPolarityEngine(evt:BioEventMention):Boolean={
    val SEMANTIC_NEGATIVE_PATTERN = LinguisticPolarityEngine.SEMANTIC_NEGATIVE_PATTERN
    val lemmas = evt.lemmas.get
    var neg_count = 0
    for (lemma <- lemmas){
      if (SEMANTIC_NEGATIVE_PATTERN.findFirstIn(lemma).isDefined)
        neg_count+=1
    }
    println(lemmas)
    println(neg_count)
    if (neg_count<=1){
      false
    }
    else{
      true
    }
  }
}