/***
* Does cross validation on the ML context model to verify the performance of the implementaiton
*/

package org.clulab.reach.context.ml

import java.io.File
import collection.mutable
import org.clulab.learning._
import Trainer._

object CrossValidation extends App {

      // First parameter: Corpus directory

      println("== Context ML model cross validation ==")
      println

      val corpusDir = new File(args(0))

      // Create a map with the article annotations
      val annotations = loadAnnotations(corpusDir).map(a => (a.name -> a)).toMap

      // Extract all the feartues ahead of time
      println(s"Extracting all featrues of ${annotations.size} papers...")
      val data:Map[String, Iterable[RVFDatum[String, String]]] =
          annotations.map{
              case(name, ann) =>
                // Extract features
                val features = extractFeatures(ann).values
                (name -> features)
          }.toMap

      val cvResults = new mutable.HashMap[String, BinaryClassificationResults]()
      val allResults = new mutable.ArrayBuffer[(Boolean, Boolean)]

      // CV Loop
      val keySet = annotations.keySet

      for(evalFold <- keySet){

          println(s"Testing fold: ${annotations(evalFold).name} ...")
          val trainingFolds = keySet - evalFold

          // Training fold's dataset
          println("Training")
          val trainingDataset = new RVFDataset[String, String]()

          // Training loop
          for(trainingFold <- trainingFolds){

            val trainingData = data(trainingFold)

            // Add the data of this paper to the training dataset
            for(datum <- trainingData){
              trainingDataset += datum
            }
          }

          // Balance dataset
          val balancedDataset = balanceDataset(trainingDataset)

          println(s"Sizes: ${trainingDataset.size} - ${balancedDataset.size}")

          println(s"Training fold size:${balancedDataset.size}\tPositives: ${balancedDataset.labels.filter(balancedDataset.labelLexicon.get(_) == "true").size}\tNegatives: ${balancedDataset.labels.filter(balancedDataset.labelLexicon.get(_) == "false").size}")

          // Normalize dataset
          val scalers = normalize(balancedDataset)

          // Train the classifier
          val classifier = train(balancedDataset)

          println("Evaluation ...")
          // Extract the evaluation fold features
          val testingData = data(evalFold)

          println(s"Testing fold size: ${testingData.size}\tPositives: ${testingData.filter(_.label == "true").size}\tNegatives: ${testingData.filter(_.label == "false").size}")

          // Evaluate the testing data using the trained classifier

          // Results is an array with the tuples (truth, prediction) boolean values
          val results = new mutable.ArrayBuffer[(Boolean, Boolean)]
          for(datum <- testingData){
              val scaledFeats =  Datasets.svmScaleDatum(datum.featuresCounter, scalers)
              val scaledDatum = new RVFDatum(datum.label, scaledFeats)

              val predictedLabel = classifier.classOf(scaledDatum) == "true"

              val truth = datum.label == "true"

              results += Tuple2(truth, predictedLabel)
          }

          val bcr = new BinaryClassificationResults(results.toSeq)
          println(bcr)
          println
          cvResults += (evalFold -> bcr)
          allResults ++= results
      }

      val microAverage = new BinaryClassificationResults(allResults)

      println
      println(s"Microaveraged results of ${keySet.size} folds:")
      println(microAverage)
}
