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

import collection.mutable.{ArrayBuffer, ListBuffer}
import util.Random
import ai.lum.common.Interval
import org.clulab.processors._
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.struct.Counter
import org.clulab.serialization.DocumentSerializer
import org.clulab.reach.darpa.{DarpaActions, MentionFilter, NegationHandler}
import org.clulab.reach.mentions._
import org.clulab.context._
import org.clulab.reach.mentions.serialization.json.JSONSerializer
import org.json4s.native.JsonMethods._

object CrossValidation extends App {

  /***
    * Base line for the evaluation
    * @param anns
    * @return
    */
  def classifyWithPolicy(anns:ArticleAnnotations):Seq[(Boolean, Boolean)] = {
        // Simulates Policy 4

        // Restore the mentions from JSON
        val jsonAST = parse(anns.preprocessed.get.mentions)

        val entities:Seq[BioMention] = JSONSerializer.toCorefMentions(jsonAST).map(_.asInstanceOf[BioMention])

        val manualContextAnnotations = anns.contextAnnotations

        // Get all the context mentions in the document besides those manually annotated by a biologist
        //val contextMentions = entities.filter(e => ContextClass.isContextMention(e) && e.label == "Cellular_component")
        val contextMentions = entities.filter(e => ContextClass.isContextMention(e))
          .map(_.asInstanceOf[BioTextBoundMention])

        // Keep only the extracted context mentions that don't overlap with a manually annotated context mention
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

        // Get all the different context types in the paper
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


        hits.flatten
      }

  // First parameter: Corpus directory

  println("== Context ML model cross validation ==")
  println

  val corpusDir = new File(args(0))

  // Create a map with the article annotations
  val annotations = loadAnnotations(corpusDir).map(a => (a.name -> a)).toMap

  // Extract all the feartues ahead of time
  // Key: Paper ID
  // Value: Iterable of annotations ??
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

  // Iterate through each paper, where each is the "evaluation fold"
  for(evalFold <- keySet){

      // Do the policy classification for comparison purposes
      val evalAnnotations = annotations(evalFold) // Fetch the paper annotations for the current paper(fold)
      val policyHits = classifyWithPolicy(evalAnnotations) // Run the baseline
      val policyResults = new BinaryClassificationResults(policyHits) // Compute the performance of the baseline
      // Store the baseline results
      deterministicCVResults += Tuple2(evalFold, policyResults)
      allDeterministicResults ++= policyHits
      /////////////////////////////

      // Fetch the training papers (training fold
      println(s"Testing fold: ${annotations(evalFold).name} ...")
      val trainingFolds = keySet - evalFold

      // Training fold's dataset
      println("Training")
      var trainingDataset = new RVFDataset[String, String]()

      // Training loop
      for(trainingFold <- trainingFolds){

        // Fetch the precomputed features of this paper
        val trainingData = data(trainingFold)

        // Balance dataset
        // val balancedSlice = balanceDataset(trainingData, negativesPerPositive = 3)

        // Add the data of this paper to the training dataset
        //for(datum <- balancedSlice) {
        for(datum <- trainingData){

          trainingDataset += datum
        }

        // Do feature selection
        trainingDataset = FeatureUtils.featureSelection(trainingDataset)
      }

      // Balance dataset
      val balancedDataset = balanceDataset(trainingDataset, negativesPerPositive = 1)
      //val balancedDataset = trainingDataset

      println(s"Original size:${trainingDataset.size} - After class-balancing size:${balancedDataset.size}")

      println(s"Positives: ${balancedDataset.labels.filter(balancedDataset.labelLexicon.get(_) == "true").size}\tNegatives: ${balancedDataset.labels.filter(balancedDataset.labelLexicon.get(_) == "false").size}")

      // Normalize dataset
      // Store the scalers to normalize the testing fold
      val scalers = normalize(balancedDataset)

      // Train the classifier
      val classifier = train(balancedDataset)

      println("Evaluation ...")
      // Extract the evaluation fold features
      val testingData = data(evalFold)

      // NO class balancing here, as this is the testing dataset
      println(s"Testing fold size: ${testingData.size}\tPositives: ${testingData.filter(_.label == "true").size}\tNegatives: ${testingData.filter(_.label == "false").size}")

      // Evaluate the testing data using the trained classifier

      // Results is an array with the tuples (truth, prediction) boolean values
      val results = new mutable.ArrayBuffer[(Boolean, Boolean)]
      for(datum <- testingData){

          // Scale the datum with the scalers from training
          val scaledFeats =  Datasets.svmScaleDatum(datum.featuresCounter, scalers)
          val scaledDatum = new RVFDatum(datum.label, scaledFeats)


          // Evaluate this prediction
          //val predictedLabel = if(datum.getFeatureCount("sentenceDistance_SAME") >= 1) true; else classifier.classOf(scaledDatum) == "true"
          val predictedLabel = classifier.classOf(scaledDatum) == "true"

          val truth = datum.label == "true"

           // Store the results of the fold
          results += Tuple2(truth, predictedLabel)
      }

      val bcr = new BinaryClassificationResults(results.toSeq)
      println("ML classifier")
      println(bcr)
      println("Policy 4")
      println(policyResults)
      println
      cvResults += (evalFold -> bcr)
      allResults ++= results
  }

  // Compute microaveraged scores for ML an baseline
  val microAverage = new BinaryClassificationResults(allResults)
  val policyMicroAverage = new BinaryClassificationResults(allDeterministicResults)

  println
  println(s"Microaveraged ML results of ${keySet.size} folds:")
  println(microAverage)
  println(s"Microaveraged deterministic results of ${keySet.size} folds:")
  println(policyMicroAverage)


  // Plots
  val individualResults = cvResults.values

  println("Precision - Size:")
  for(cv <- individualResults){
    println(s"${cv.precision}\t-\t${cv.size}")
  }
}
