package org.clulab.context.classifiers

import java.io._

import org.clulab.context.utils.AggregatedContextInstance
import org.clulab.struct.Counter
import org.clulab.learning._
case class LinearSVMContextClassifier(classifier: Option[LinearSVMClassifier[Int,String]] = None, pathToClassifier:Option[String] = None) extends ContextClassifier {
  override def fit(xTrain: Seq[AggregatedContextInstance]): Unit = ()

  private def checkForNullException(classForFunct: Option[LinearSVMClassifier[Int,String]], pathForFunct:Option[String]): Option[LinearSVMClassifier[Int,String]] = {
   classForFunct match {
     case Some(c) => Some(c)
     case None => {
       pathForFunct match {
         case Some(s) => {
           val loadedWrapper = loadFrom(s)
           loadedWrapper.classifier}
         case None => None
         }
       }
     }
   }

    /*if(classForFunct == null) {
      if(pathForFunct == null)
        None
      else {
        val loadedWrapper = loadFrom(pathForFunct)
        Some(loadedWrapper.classifier)
      }
    }
    else Some(classForFunct)*/


  def fit(xTrain: RVFDataset[Int, String]):Unit = {
    val classifierToTrain = checkForNullException(classifier, pathToClassifier)
    classifierToTrain match {
      case Some(c) => c.train(xTrain)
      case None => println("ERROR: The Linear SVM model has not been trained yet, since default null parameters were detectected in the custructor. However, you can fit the model by loading it from file, using the loadFrom function.")
    }
    //val modelWrap = loadFrom()
  }

  override def predict(data: Seq[AggregatedContextInstance]): Array[Int] = {
    val (_, individualRows) = dataConverter(data)
    val classifierToPredict = checkForNullException(classifier, pathToClassifier)
    classifierToPredict match {
      case Some(c) => individualRows.map(c.classOf(_))
      case None => {
        println("ERROR: No valid classifier was found on which I could predict. Please ensure you are passing a valid LinearSVM classifier, or a path to a classifier. I am now returning a default array of 0s")
        Array.fill(individualRows.size)(0)
      }
    }
    }

  def predict(testDatum:RVFDatum[Int, String]):Int = {
    val classifierToPredict = checkForNullException(classifier, pathToClassifier)
    classifierToPredict match {
      case Some(c) => c.classOf(testDatum)
      case None => {
        println("I cannot predict the current datapoint on an empty classifier. Returning a default value of 0")
      0
      }
    }
    //classifier.classOf(testDatum)
  }

  override def saveModel(fileName: String): Unit = {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    os.writeObject(this)
    os.close()
  }

  override def loadFrom(fileName: String): LinearSVMContextClassifier = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[LinearSVMContextClassifier]
    is.close()
    c
  }


  // Consider features as pairs of (feature name, feature value)
  private def mkRVFDatum[L](label:L, features:Array[(String, Double)]):RVFDatum[L, String] = {
    // In here, Counter[T] basically works as a dictionary, and String should be the simplest way to implement it
    // when you call c.incrementCount, you basically assign the feature called "featureName", the value in the second parameter ("inc")
    val c = new Counter[String]
    // In this loop we go through all the elements in features and initialize the counter with the values. It's weird but that's the way it was written
    for((featureName, featureValue) <- features) c.incrementCount(featureName, inc = featureValue)
    // Just changed the second type argument to string here. Label is the class, so, L can be Int to reflext 1 or 0
    new RVFDatum[L, String](label, c)
  }

  // Here I made the changes to reflect my comments above.
  def mkRVFDataSet(labels: Array[Int], dataSet:Array[Array[(String, Double)]]):(RVFDataset[Int, String], Array[RVFDatum[Int, String]]) = {
    val dataSetToReturn = new RVFDataset[Int, String]()
    val datumCollect = collection.mutable.ListBuffer[RVFDatum[Int, String]]()
    val tupIter = dataSet zip labels
    for((d,l) <- tupIter) {
      val currentDatum = mkRVFDatum(l,d)
      dataSetToReturn += currentDatum
      datumCollect += currentDatum
    }
    (dataSetToReturn, datumCollect.toArray)
  }

  def constructTupsForRVF(rows: Seq[AggregatedContextInstance]):Array[Array[(String, Double)]] = {
    val toReturn = collection.mutable.ListBuffer[Array[(String,Double)]]()
    rows.map(r => {
      val featureVals = r.featureGroups
      val featureName = r.featureGroupNames
      val zipped = featureName zip featureVals
      toReturn += zipped
    })
    toReturn.toArray
  }

  def dataConverter(data:Seq[AggregatedContextInstance], existingLabels: Option[Array[Int]] = None):(RVFDataset[Int, String], Array[RVFDatum[Int, String]]) = {
    val tups = constructTupsForRVF(data)
    val labels = existingLabels match {
      case None => createLabels(data)
      case Some(x) => x }
    val result = mkRVFDataSet(labels, tups)
    result
  }

  def createLabels(data:Seq[AggregatedContextInstance]):Array[Int] = {
    val currentTruthTest = DummyClassifier.convertOptionalToBool(data)
    val currentTruthTestInt = DummyClassifier.convertBooleansToInt(currentTruthTest)
    currentTruthTestInt
  }


}
