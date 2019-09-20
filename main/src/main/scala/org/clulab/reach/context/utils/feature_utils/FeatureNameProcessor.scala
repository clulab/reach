package org.clulab.reach.context.utils.feature_utils



object FeatureNameProcessor {
  def fixFeatureNameInInputStream(headers:Seq[String]): Seq[String] = {
     val result = collection.mutable.ListBuffer[String]()
     headers.map(h => if(headers.indexOf(h) == 1) result += "PMCID" else result += h)
     result
   }

  def createBestFeatureSetForTraining(allFeatures:Seq[String]):Map[String, Seq[String]] = {
      val nonNumericFeatures = Seq("PMCID", "label", "EvtID", "CtxID", "")
      val numericFeatures = allFeatures.toSet -- nonNumericFeatures.toSet
      val featureDict = createFeatureTypeDictionary(numericFeatures.toSeq)
      featureDict
    }

  def createFeatureTypeDictionary(numericFeatures: Seq[String]):Map[String, Seq[String]] = {
      val contextDepFeatures = numericFeatures.filter(_.startsWith("ctxDepTail"))
      val eventDepFeatures = numericFeatures.filter(_.startsWith("evtDepTail"))
      val nonDepFeatures = numericFeatures.toSet -- (contextDepFeatures.toSet ++ eventDepFeatures.toSet)
      val map = collection.mutable.Map[String, Seq[String]]()
      map += ("All_features" -> numericFeatures)
      map += ("Non_Dependency_Features" -> nonDepFeatures.toSeq)
      map += ("NonDep_Context" -> (nonDepFeatures ++ contextDepFeatures.toSet).toSeq)
      map += ("NonDep_Event" -> (nonDepFeatures ++ eventDepFeatures.toSet).toSeq)
      map += ("Context_Event" -> (contextDepFeatures.toSet ++ eventDepFeatures.toSet).toSeq)
      map.toMap
    }


  // we want to "unaggregate" the name of the feature
  // for example, if my feature name is sentenceDistance_max, we want to take only sentenceDistance,
  // because if we miss that, we will get feature names like sentenceDistance_max_min, sentencedistance_max_max,
  // which is not meaningful to our SVM.
  def resolveToUnaggregatedFeatureName(seq: Seq[String], take: Int):Seq[String] = {
    val result = collection.mutable.ListBuffer[String]()
    val ids = seq.take(take)
    val numericalFeatureNames = seq.drop(take)
    result ++= ids
    val miniList = collection.mutable.ListBuffer[String]()
    numericalFeatureNames.map(m => {
      val lim = m.length-4

      var slice = ""
      if(m.contains("_max"))
        m.replace("_max","")
      else if(m.contains("_min"))
        m.replace("_min","")
      else m.replace("_avg","")

      miniList += slice
    })
    result ++=miniList.toSet.toSeq
    result
  }

  def extendFeatureName(f:String):(String, String, String) = {

    val feat_min = s"${f}_min"
    val feat_max = s"${f}_max"
    val feat_avg = s"${f}_avg"
    (feat_min, feat_max, feat_avg)

  }
}
