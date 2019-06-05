package org.clulab.reach.context

import org.clulab.context.utils.ContextPairInstance
import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}

object ContextFeatValUtils {
  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)

  def getFeatValMapPerInput(filteredPairs: Seq[Pair], ctxMentions: Seq[BioTextBoundMention]):Map[ContextPairInstance, (Map[String,Double],Map[String,Double],Map[String,Double])] = {
    val tempo = filteredPairs.map{p =>
      val featureExtractor = new ContextFeatureExtractor(p, ctxMentions)
      featureExtractor.extractFeaturesToCalcByBestFeatSet()
    }
    val flattenedMap = tempo.flatMap(t=>t).toMap
    flattenedMap
  }

  def getCtxPairInstances(filteredPairs: Seq[Pair], ctxMentions: Seq[BioTextBoundMention]): Seq[ContextPairInstance] = {
    val tempo = filteredPairs.map{p =>
      val featureExtractor = new ContextFeatureExtractor(p, ctxMentions)
      featureExtractor.extractFeaturesToCalcByBestFeatSet()
    }
    tempo.flatMap(t => t.keySet)
  }

}
