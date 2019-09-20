package org.clulab.reach.context.svm_scripts


import org.clulab.learning.LinearSVMClassifier

class TrainSVMInstance extends App {
//  val resourcesPath = "/org/clulab/context/svmFeatures"
//  val SVMClassifier = new LinearSVMClassifier[Int, String](C = 0.001, eps = 0.001, bias = false)
//  val svmInstance = new LinearSVMContextClassifier(Some(SVMClassifier))
//  val groupedFeatures = config.getString("svmContext.groupedFeatures")
//  val hardCodedFeaturePath = config.getString("contextEngine.params.hardCodedFeatures")
//  val (allFeatures,rows) = IOUtilsForFeatureName.loadAggregatedRowsFromFile(groupedFeatures, hardCodedFeaturePath)
//  val nonNumericFeatures = Seq("PMCID", "label", "EvtID", "CtxID", "")
//  val numericFeatures = allFeatures.toSet -- nonNumericFeatures.toSet
//  val featureDict = createFeaturesLists(numericFeatures.toSeq)
//  val bestFeatureSet = featureDict("NonDep_Context")
//  val trainingDataPrior = rows.filter(_.PMCID != "b'PMC4204162'")
//  val trainingData = extractDataByRelevantFeatures(bestFeatureSet, trainingDataPrior)
//
//  // training the machine learning model and writing it to file
//  val trainingLabels = DummyClassifier.convertOptionalToBool(trainingData)
//  val labelsToInt = DummyClassifier.convertBooleansToInt(trainingLabels)
//  val tups = svmInstance.constructTupsForRVF(trainingData)
//  val (trainDataSet, _) = svmInstance.mkRVFDataSet(labelsToInt,tups)
//  svmInstance.fit(trainDataSet)
//  svmInstance.saveModel(fileName)
//
//
//  def createFeaturesLists(numericFeatures: Seq[String]):Map[String, Seq[String]] = {
//    val contextDepFeatures = numericFeatures.filter(_.startsWith("ctxDepTail"))
//    val eventDepFeatures = numericFeatures.filter(_.startsWith("evtDepTail"))
//    val nonDepFeatures = numericFeatures.toSet -- (contextDepFeatures.toSet ++ eventDepFeatures.toSet)
//    val map = collection.mutable.Map[String, Seq[String]]()
//    map += ("All_features" -> numericFeatures)
//    map += ("Non_Dependency_Features" -> nonDepFeatures.toSeq)
//    map += ("NonDep_Context" -> (nonDepFeatures ++ contextDepFeatures.toSet).toSeq)
//    map += ("NonDep_Event" -> (nonDepFeatures ++ eventDepFeatures.toSet).toSeq)
//    map += ("Context_Event" -> (contextDepFeatures.toSet ++ eventDepFeatures.toSet).toSeq)
//    map.toMap
//  }
//
//  def extractDataByRelevantFeatures(featureSet:Seq[String], data:Seq[AggregatedContextInstance]):Seq[AggregatedContextInstance] = {
//    val result = data.map(d => {
//      val currentSent = d.sentenceIndex
//      val currentPMCID = d.PMCID
//      val currentEvtId = d.EvtID
//      val currentContextID = d.CtxID
//      val currentLabel = d.label
//      val currentFeatureName = d.featureGroupNames
//      val currentFeatureValues = d.featureGroups
//      val indexList = collection.mutable.ListBuffer[Int]()
//      // we need to check if the feature is present in the current row. Only if it is present should we try to access its' value.
//      // if not, i.e. if the feature is not present and we try to access it, then we get an ArrayIndexOutOfBound -1 error/
//      featureSet.map(f => {
//        if(currentFeatureName.contains(f)) {
//          val tempIndex = currentFeatureName.indexOf(f)
//          indexList += tempIndex
//        }
//      })
//      val valueList = indexList.map(i => currentFeatureValues(i))
//      AggregatedContextInstance(currentSent, currentPMCID, currentEvtId, currentContextID, currentLabel, valueList.toArray, featureSet.toArray)
//    })
//    result
//  }
}
