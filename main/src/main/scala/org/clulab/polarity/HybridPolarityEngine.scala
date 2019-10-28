package org.clulab.polarity
import com.typesafe.config.ConfigFactory
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

class HybridLinguisticDeepLearningPolarityEngine(mlEngine:MLPolarityEngine) extends HybridPolarityEngine(mlEngine:MLPolarityEngine) {
  override def useMLPolarityEngine(evt:BioEventMention):Boolean={
    val SEMANTIC_NEGATIVE_PATTERN = LinguisticPolarityEngine.SEMANTIC_NEGATIVE_PATTERN
    val lemmas = evt.lemmas.get

    val config = ConfigFactory.load()
    val configPath = "polarity"
    var negCountThreshold = 1
    if(config.hasPath(configPath)) {
      negCountThreshold = config.getInt(configPath + ".negCountThreshold")
      //logger.info(s"Negation count threshold is set to ${negCountThreshold}")
    }


    var negCount = 0
    for (lemma <- lemmas){
      if (SEMANTIC_NEGATIVE_PATTERN.findFirstIn(lemma).isDefined)
        negCount+=1
    }
    if (negCount<=negCountThreshold){
      false
    }
    else{
      //println(lemmas)
      //println(neg_count)
      true
    }
  }
}