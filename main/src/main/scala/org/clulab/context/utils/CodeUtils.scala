package org.clulab.context.utils

import java.io._
import java.util.zip._

import org.clulab.learning.{RVFDataset, RVFDatum}

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
    if(!(preds("TP").toDouble + preds("FP").toDouble == 0.toDouble)) preds("TP").toDouble / (preds("TP") + preds("FP")).toDouble
    else 0.0
  }

  def recall(preds: Map[String, Int]): Double = {
    if (!(preds("TP").toDouble + preds("FN").toDouble == 0)) preds("TP").toDouble/(preds("TP") + preds("FN")).toDouble
    else 0.0
  }


  def accuracy(preds:Map[String, Int]): Double = {
    if (!((preds("TP") + preds("FP") + preds("FN") + preds("TN").toDouble) == 0)) (preds("TP") + preds("TN")).toDouble/(preds("TP") + preds("TN") + preds("FP") + preds("FN")).toDouble
    else 0.0
  }

  def arithmeticMeanScore(scores:Seq[Double]):Double = {
    val sum = scores.foldLeft(0)(_+_)
    sum/scores.size
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

  def readRVFDatasetFromFile(fileName: String): RVFDataset[Int, String] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val dataset = is.readObject.asInstanceOf[RVFDataset[Int, String]]
    dataset
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
    val fileInputStream = new FileInputStream(groupedFeaturesFileName)
    val bufferedStream = new BufferedInputStream(new GZIPInputStream(fileInputStream))
    val source = Source.fromInputStream(bufferedStream)
    val lines = source.getLines()
    val headers = lines.next() split ","
    val rectifiedHeaders = rectifyWrongFeatures(headers)
    val features = allOtherFeatures(rectifiedHeaders)
    val ixs = indices(rectifiedHeaders)
    val ret = lines.map(l => AggregatedContextInstance(l, rectifiedHeaders, features, ixs, listOfSpecificFeatures)).toList
    source.close()
    (rectifiedHeaders, ret)
  }

  def featureConstructor(file:String):Map[String, Seq[String]] = {
    val is = new ObjectInputStream(new FileInputStream(file))
    val headers = is.readObject().asInstanceOf[Array[String]]
    val rectifiedHeaders = rectifyWrongFeatures(headers)
    is.close()
    createBestFeatureSet(rectifiedHeaders)
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


  def generateLabelMap(fileName: String): Map[(String,String,String), Int] = {
    val map = collection.mutable.HashMap[(String,String,String), Int]()
    val source = Source.fromFile(fileName)
    val lines = source.getLines()
    val content = lines.drop(1)
    for(c <- content) {
      val array = c.split(",")
      val pmcid = array(0)
      val evtID = array(1)
      val ctxID = array(2)
      val label = Integer.parseInt(array(3))
      val tup = (pmcid,evtID,ctxID)
      map ++= Map(tup -> label)
    }

    map.toMap
  }


  def findAggrMetrics(seq:Seq[Double]): (Double,Double,Double) = {
    val min = seq.foldLeft(Double.MaxValue)(Math.min(_,_))
    val max = seq.foldLeft(Double.MinValue)(Math.max(_,_))
    val sum = seq.foldLeft(0.0)(_+_)
    val avg = sum.toDouble/seq.size.toDouble
    (min,max,avg)
  }


  def writeMetricsToCSV(metricMap:Map[String,(Double,Double,Double)], filePath:String): Unit = {
    val pw = new PrintWriter(filePath)
    for((key,valueTup) <- metricMap) {
      val string = s"${key},${valueTup._1.toString.take(5)},${valueTup._2.toString.take(5)},${valueTup._3.toString.take(5)}"
      pw.write(string)
      pw.write("\n")
    }
    pw.close()
  }


}
