package org.clulab.context.utils

import java.io._

import scala.collection.mutable
import scala.io.Source

object CodeUtils {
  def argMax(values:Map[Int, Double]):Int = {
    var bestK = Integer.MIN_VALUE
    var bestF1 = Double.MinValue
    values.map(x => {if (x._2 > bestF1) {bestK = x._1; bestF1 = x._2}})
    bestK
  }

  def f1(preds: Map[String, Int]): Double = {
    val p = precision(preds)
    val r = recall(preds)
    if (p + r == 0) 0.0
    else ((2 * (p * r))/(p + r))
  }

  def precision(preds: Map[String, Int]): Double = {
    if(!(preds("TP") + preds("FP") == 0)) preds("TP").toDouble / (preds("TP") + preds("FP")).toDouble
    else 0.0
  }

  def recall(preds: Map[String, Int]): Double = {
    if (!(preds("TP") + preds("FN") == 0)) preds("TP").toDouble/(preds("TP") + preds("FN")).toDouble
    else 0.0
  }


  def accuracy(preds:Map[String, Int]): Double = {
    if (!((preds("TP") + preds("FP") + preds("FN") + preds("TN")) == 0)) (preds("TP") + preds("TN")).toDouble/(preds("TP") + preds("TN") + preds("FP") + preds("FN")).toDouble
    else 0.0
  }

  def predictCounts(yTrue: Array[Int], yPred: Array[Int]): Map[String, Int] = {
    val indexValuePair = yTrue zip yPred
    var TP = 0; var FP = 0; var TN = 0; var FN = 0
    for((gt,pr) <- indexValuePair) {
      if (gt == 1 && pr == 1) TP+=1
      if (gt == 1 && pr == 0) FN +=1
      if (gt == 0 && pr == 0) TN +=1
      if (gt == 0 && pr == 1) FP +=1
    }
    Map(("TP" -> TP), ("FP" -> FP), ("TN" -> TN), ("FN" -> FN))
  }

  def scoreMaker(name: String, truthTest:Array[Int], predTest:Array[Int]):Map[String, (String, Double, Double, Double)] = {
    val countsTest = CodeUtils.predictCounts(truthTest, predTest)
    val precTest = CodeUtils.precision(countsTest)
    val recallTest = CodeUtils.recall(countsTest)
    val f1Test = CodeUtils.f1(countsTest)
    val testTup = ("test", precTest, recallTest, f1Test)
    val mapToReturn = Map(name -> testTup)
    mapToReturn
  }

  def combineTrainVal(folds: Array[(Array[Int], Array[Int], Array[Int])]):Array[(Array[Int], Array[Int])] = {
    val trainValCombined = collection.mutable.ListBuffer[(Array[Int], Array[Int])]()
    for((train,validate,test) <- folds) {
      val trainVal = train ++ validate
      val toAdd = (trainVal, test)
      trainValCombined += toAdd
    }
    trainValCombined.toArray
  }

  def createStats(nums: Iterable[Double]): (Double, Double, Double) = {
    val min = nums.min
    val max = nums.max
    val avg = nums.sum / nums.size
    (min, max, avg)
  }

  def extendFeatureName(f:String):(String, String, String) = {

    val feat_min = s"${f}_min"
    val feat_max = s"${f}_max"
    val feat_avg = s"${f}_avg"
    (feat_min, feat_max, feat_avg)

  }

  def resolveUnaggregatedFeatureName(seq: Seq[String], take: Int):Seq[String] = {
    val result = collection.mutable.ListBuffer[String]()
    val ids = seq.take(take)
    val numericalFeatureNames = seq.drop(take)
    result ++= ids
    val miniList = collection.mutable.ListBuffer[String]()
    numericalFeatureNames.map(m => {
      val lim = m.length-4
      val slice = m.slice(0,lim)
      miniList += slice
    })
    result ++=miniList.toSet.toSeq
    result
  }

  def writeHardcodedFeaturesToFile(fileName: String):Unit = {
    val listOfSpecificForAggregated = Seq("PMCID", "label", "EvtID", "CtxID", "closesCtxOfClass_min", "closesCtxOfClass_max", "closesCtxOfClass_avg", "context_frequency_min","context_frequency_max", "context_frequency_avg",
      "evtNegationInTail_min","evtNegationInTail_max","evtNegationInTail_avg", "ctxSentenceFirstPerson_min","ctxSentenceFirstPerson_max","ctxSentenceFirstPerson_avg","ctxNegationIntTail_min","ctxNegationIntTail_max","ctxNegationIntTail_avg","evtSentenceFirstPerson_min","evtSentenceFirstPerson_max","evtSentenceFirstPerson_avg", "evtSentencePastTense_min","evtSentencePastTense_max","evtSentencePastTense_avg", "evtSentencePresentTense_min","evtSentencePresentTense_max","evtSentencePresentTense_avg", "ctxSentencePresentTense_min","ctxSentencePresentTense_max","ctxSentencePresentTense_avg", "ctxSentencePastTense_max","ctxSentencePastTense_min", "ctxSentencePastTense_avg","ctxSentenceFirstPerson_min","ctxSentenceFirstPerson_min","ctxSentenceFirstPerson_min","sentenceDistance_min","sentenceDistance_max","sentenceDistance_avg", "dependencyDistance_min", "dependencyDistance_max", "dependencyDistance_avg")
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    os.writeObject(listOfSpecificForAggregated.toArray)
    os.close
  }

  def readHardcodedFeaturesFromFile(fileName: String):Array[String] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val headers = is.readObject().asInstanceOf[Array[String]]
    headers
  }

  def writeAllFeaturesToFile(allFeatures:Seq[String], fileName:String):Seq[String] = {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    val str = new mutable.StringBuilder()
    for(i<- 0 until allFeatures.size - 1) {
      val current = allFeatures(i)
      str.append(current+",")
    }
    str.append(allFeatures.last)
    val stringEquiv = str.toString()
    val arr = stringEquiv.split(",")
    os.writeObject(arr.asInstanceOf[Array[String]])
    os.close()
    allFeatures
  }

  def loadAggregatedRowsFromFile(groupedFeaturesFileName: String, hardCodedFilePath: String):(Seq[String], Seq[AggregatedContextInstance]) = {
    val listOfSpecificFeatures = readHardcodedFeaturesFromFile(hardCodedFilePath)
    def allOtherFeatures(headers:Seq[String]): Set[String] = headers.toSet -- (listOfSpecificFeatures ++ Seq(""))
    def indices(headers:Seq[String]): Map[String, Int] = headers.zipWithIndex.toMap
    val source = Source.fromFile(groupedFeaturesFileName)
    val lines = source.getLines()
    val headers = lines.next() split ","
    val rectifiedHeaders = rectifyWrongFeatures(headers)
    val features = allOtherFeatures(rectifiedHeaders)
    val ixs = indices(rectifiedHeaders)
    val ret = lines.map(l => AggregatedContextInstance(l, rectifiedHeaders, features, ixs, listOfSpecificFeatures)).toList
    source.close()
    (rectifiedHeaders, ret)
  }

  def featureConstructor(file:String):(Seq[String], Map[String, Seq[String]]) = {
    val is = new ObjectInputStream(new FileInputStream(file))
    val headers = is.readObject().asInstanceOf[Array[String]]
    val rectifiedHeaders = rectifyWrongFeatures(headers)
    is.close()
    (rectifiedHeaders, createBestFeatureSet(rectifiedHeaders))
  }

 private def rectifyWrongFeatures(headers:Seq[String]): Seq[String] = {
    val result = collection.mutable.ListBuffer[String]()
    headers.map(h => if(headers.indexOf(h) == 1) result += "PMCID" else result += h)
    result
  }

  def createBestFeatureSet(allFeatures:Seq[String]):Map[String, Seq[String]] = {
    val nonNumericFeatures = Seq("PMCID", "label", "EvtID", "CtxID", "")
    val numericFeatures = allFeatures.toSet -- nonNumericFeatures.toSet
    val featureDict = CodeUtils.createFeatureDictionary(numericFeatures.toSeq)
    featureDict
  }

  def createFeatureDictionary(numericFeatures: Seq[String]):Map[String, Seq[String]] = {
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
  // for every new feature, we add to a map of (featureName, (_min, _max, _sum, size))
  def aggregateInputRowFeats(features:Seq[String]):Map[String,(Double,Double, Double, Int)] = {
    val resultingMap = collection.mutable.Map[String,(Double,Double, Double, Int)]()
    for(r <- features) {
      if(resultingMap.contains(r)) {
        val valueToBeAdded = 1.0
        val currentFeatDetails = resultingMap(r)
        val tupReplace = (Math.min(currentFeatDetails._1, valueToBeAdded),
                        Math.max(currentFeatDetails._2, valueToBeAdded),
                        currentFeatDetails._3 + valueToBeAdded,
                        currentFeatDetails._4+1)
        resultingMap(r) = tupReplace

      }
      else {
        val entry = (r -> (1.0,1.0,1.0,1))
        resultingMap += entry
      }
    }
    resultingMap.toMap
  }

  // in the given map, key is the feature name that we will extend to name_min, name_max, name_avg
  // value is (_min, _max, total, size) wherein we extract the following tup: (_min, _max, total/size)
  // important part is to store them in the same order, in harmony with AggregatedRowNew
  def finalFeatValuePairing(aggr: Map[String,(Double,Double, Double, Int)]): Seq[((String,String,String), (Double,Double,Double))] = {
    val finalPairings = collection.mutable.ListBuffer[((String,String,String), (Double,Double,Double))]()
    for((key,value)<- aggr){
      val extendedKey = extendFeatureName(key)
      val nameTup = (extendedKey._1, extendedKey._2, extendedKey._3)
      val valueTup = (value._1, value._2, (value._3/value._4))
      val currentTup = (nameTup, valueTup)
      finalPairings+= currentTup
    }

    finalPairings
  }

  def featFreqMap(input: Seq[AggregatedContextInstance], bestFeatureSet:Seq[String]):Map[String,Int]= {
    val mut = collection.mutable.HashMap[String,Int]()
    for(i <- input){
      val currentFeatureSet = i.featureGroupNames
      for(c<-currentFeatureSet) {
        if(bestFeatureSet.contains(c)){
          if(mut.contains(c)){
            val freq = mut(c)+1
            mut += (c -> freq)
          }
          else mut+=(c->1)
        }
      }
    }
    mut.toMap
  }

  def writeFeatFreqToFile(map: Map[String,Int], fileName:String): Unit = {
    val pw = new PrintWriter(new File(fileName))
    for((k,v) <- map.toSeq) {
      val str = s"The Frequency of ${k} is ${v} \n"
      pw.write(str)
    }
  }

  def writeFeatValsToFile(input:Seq[AggregatedContextInstance], fileName:String):Unit = {
    val pw = new PrintWriter(new File(fileName))
    for(i <- input){
      val currentFeatureSet = i.featureGroupNames
      val currentFeaturesValue = i.featureGroups
      val pairs = currentFeatureSet zip currentFeaturesValue
      for((name,value) <- pairs) {
        val str = s"${name} : ${value} \n"
        pw.write(str)
      }
    }

  }



}
