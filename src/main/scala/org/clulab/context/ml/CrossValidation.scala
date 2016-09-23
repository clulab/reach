/***
* Does cross validation on the ML context model to verify the performance of the implementaiton
*/

package org.clulab.context.ml

import java.io.File
import collection.mutable
import org.clulab.learning._
import Trainer._
import org.clulab.reach._
import org.clulab.context.ml.dataset._
import org.clulab.processors._
import org.clulab.odin._
import org.clulab.context.ContextEngine
import org.clulab.reach.mentions._
import java.io._
import collection.mutable.{ListBuffer, ArrayBuffer}
import util.Random
import ai.lum.common.Interval
import org.clulab.processors._
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.struct.Counter
import org.clulab.serialization.DocumentSerializer
import org.clulab.reach.darpa.{DarpaActions, MentionFilter, NegationHandler}
import org.clulab.reach.mentions._
import org.clulab.context._

object CrossValidation extends App {

      def classifyWithPolicy(anns:ArticleAnnotations):Seq[(Boolean, Boolean)] = {
        // Simulates Policy 4

        val entities:Seq[BioMention] = anns.preprocessed.get.mentions

        val manualContextAnnotations = anns.contextAnnotations

        val contextMentions = entities.filter(e => ContextClass.isContextMention(e) && e.label == "Cellular_component")
          .map(_.asInstanceOf[BioTextBoundMention])

        val filteredMentions = contextMentions.filter{
          m =>
            !manualContextAnnotations.exists{
              mc =>
                val i = Interval.closed(m.tokenInterval.start, m.tokenInterval.end)
                mc.sentenceId == m.sentence && mc.interval.intersects(i)
            }
        }.map(FeatureExtractor.contextMention2Annotation)

        // All the context types in the paper
        val augmentedCtxAnnotations = manualContextAnnotations ++ filteredMentions
        val ctxTypes:Set[ContextType] = augmentedCtxAnnotations.map(_.contextType).toSet

        // Group the event with the context annotations
        val hits:Seq[Seq[(Boolean, Boolean)]] = for(event <- anns.eventAnnotations) yield {
            val evtSentence = event.sentenceId
            val candidateContexts:Set[ContextType] = augmentedCtxAnnotations
              .filter{
                ca =>
                  Math.abs(ca.sentenceId - evtSentence) <= 3
              }.map(_.contextType).toSet

            val realContexts = event.annotatedContexts.get

            (for(ctx <- ctxTypes.toSeq) yield {

              if(candidateContexts.contains(ctx)){
                if(realContexts.contains(ctx)){
                  (true, true)
                }
                else{
                  (false, true)
                }
              }
              else{
                if(realContexts.contains(ctx)){
                  (true, false)
                }
                else{
                  (false, false)
                }
              }
            }).toSeq
        }

        // val hits:Seq[(Boolean, Boolean)] = anns.eventAnnotations.toSeq.flatMap{
        //   e =>
        //   ctxTypes.map{
        //     x => (true, true)
        //   }.toSeq
        // }


        hits.flatten
      }

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
      val deterministicCVResults = new mutable.HashMap[String, BinaryClassificationResults]()
      val allResults = new mutable.ArrayBuffer[(Boolean, Boolean)]
      val allDeterministicResults = new mutable.ArrayBuffer[(Boolean, Boolean)]

      // CV Loop
      val keySet = annotations.keySet

      for(evalFold <- keySet){

          // Do the policy classification for comparison purposes
          val evalAnnotations = annotations(evalFold)
          val policyHits = classifyWithPolicy(evalAnnotations)
          val policyResults = new BinaryClassificationResults(policyHits)
          deterministicCVResults += Tuple2(evalFold, policyResults)
          allDeterministicResults ++= policyHits

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
          println(s"DEBUG: Testing data size: ${testingData.size}")
          for(datum <- testingData){
              val scaledFeats =  Datasets.svmScaleDatum(datum.featuresCounter, scalers)
              val scaledDatum = new RVFDatum(datum.label, scaledFeats)



              val predictedLabel = if(datum.getFeatureCount("sentenceDistance_SAME") >= 1) true; else classifier.classOf(scaledDatum) == "true"

              val truth = datum.label == "true"

              results += Tuple2(truth, predictedLabel)
          }

          val bcr = new BinaryClassificationResults(results.toSeq)
          println(bcr)
          println("Policy 4")
          println(policyResults)
          println
          cvResults += (evalFold -> bcr)
          allResults ++= results
      }

      val microAverage = new BinaryClassificationResults(allResults)
      val policyMicroAverage = new BinaryClassificationResults(allDeterministicResults)

      println
      println(s"Microaveraged ML results of ${keySet.size} folds:")
      println(microAverage)
      println(s"Microaveraged deterministic results of ${keySet.size} folds:")
      println(policyMicroAverage)

}
