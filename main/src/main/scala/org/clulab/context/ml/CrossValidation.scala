/***
* Does cross validation on the ML context model to verify the performance of the implementation
*/

package org.clulab.context.ml

import java.io.File

import collection.mutable
import org.clulab.learning._
import Trainer._
import org.clulab.reach._
import org.clulab.context.ml.dataset.{FeatureFamily, _}
import org.clulab.processors._
import org.clulab.odin._
import org.clulab.context.ContextEngine
import org.clulab.reach.mentions._
import java.io._

import collection.mutable.{ArrayBuffer, ListBuffer}
import util.Random
import ai.lum.common.Interval
import com.typesafe.config.ConfigFactory
import org.clulab.processors._
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.struct.Counter
import org.clulab.serialization.DocumentSerializer
import org.clulab.reach.darpa.{DarpaActions, MentionFilter, NegationHandler}
import org.clulab.utils.Serializer
import org.clulab.reach.mentions._
import org.clulab.context._
import org.clulab.reach.mentions.serialization.json.JSONSerializer
import org.json4s.native.JsonMethods._
//To convert java list to scala.List: (reference: https://stackoverflow.com/questions/17913215/how-to-get-a-list-with-the-typesafe-config-library)
import collection.JavaConversions._

object CrossValidation extends App {

  def sampleStatistics[T<:Double](s:Iterable[T]):(Double, Double) = {
    val avg = s.sum[Double]/s.size
    val stde = Math.sqrt((s.map(_ - avg).map(i => Math.pow(i, 2))).sum[Double] / (s.size - 1.0))

    (avg, stde)
  }

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
    /*
    def testModelOnTrainingData(balancedDataset:RVFDataset[String, String], classifier: Trainer): Unit ={
        println(s"Testing fold size: ${balancedDataset.size}\tPositives: ${balancedDataset.filter(_.label == "true").size}\tNegatives: ${balancedDataset.filter(_.label == "false").size}")
    
        // Evaluate the TRAINING data using the trained classifier
        // Data should already be scaled
    
        // Results is an array with the tuples (truth, prediction) boolean values
        val results = new mutable.ArrayBuffer[(Boolean, Boolean)]
        for(datum <- balancedDataset){
        
            // Scale the datum with the scalers from training.  SHOULD ALREADY BE SCALED
            //val scaledFeats =  Datasets.svmScaleDatum(datum.featuresCounter, scalers)
            //val scaledDatum = new RVFDatum(datum.label, scaledFeats)
        
        
            // Evaluate this prediction
            //val predictedLabel = if(datum.getFeatureCount("sentenceDistance_SAME") >= 1) true; else classifier.classOf(scaledDatum) == "true"
            val predictedLabel = classifier.classOf(scaledDatum) == "true"
        
            val truth = datum.label == "true"
        
            // Store the results of the fold
            results += Tuple2(truth, predictedLabel)
        }
       
    
        val sparseness = sparsenessMeter.sparseness
        val features = sparsenessMeter.totalFeatures
        val bcr = new BinaryClassificationResults(results.toSeq)
        println(s"Sparseness: $sparseness\tTotal features: $features")
        println("ML classifier")
        println(bcr)
    }
    */
  
  def printTrainingResults(trainingSetResults: mutable.ArrayBuffer[(Boolean, Boolean)]){
    val bcr = new BinaryClassificationResults(trainingSetResults.toSeq)
    println("ML Classifier Training Results")
    println(bcr)
  }
    
    
  // First parameter: Corpus directory

  println("== Context ML model cross validation ==")
  println

  val corpusDir = new File(args(0))

  // Create a map with the article annotations
  val annotations = loadAnnotations(corpusDir).map(a => (a.name -> a)).toMap

  // Specify config.  Pasted from ReachCLI.scala:
  // use specified config file or the default one if one is not provided
  val config =
    // Assuming path to papers is the first argument (i.e. not specified in config file):
    // If args only contain path to papers:
    if (args.length == 1) ConfigFactory.load()
    // else:
    //   for now, specifying config file as second argument:
    else ConfigFactory.parseFile(new File(args(1))).resolve()

  val featureFamiliesList = config.getStringList("contextCrossValidation.featureFamilies").toList

  // search for featureFamily string in list of featureFamilies and, if featureFamily is included, append to Set()
  //instantiate empty Set of featureFamily Trait objects:f
  // currently using using var set for appending (could use val mutable.Set, but I'm not sure if that would break something (i.e. if other code is expecting the default immutable Set)
  var featureFamilies = Set.empty[FeatureFamily]
  if (featureFamiliesList.contains("Positional")) featureFamilies += Positional()
  if (featureFamiliesList.contains("Dependency")) featureFamilies += Dependency()
  if (featureFamiliesList.contains("Phi")) featureFamilies += Phi()
  if (featureFamiliesList.contains("NegationProperty")) featureFamilies += NegationProperty()
  if (featureFamiliesList.contains("Tails")) featureFamilies += Tails()
  if (featureFamiliesList.contains("POS")) featureFamilies += POS()

  // Hardcoded feature families:
  /*val featureFamilies = Set[FeatureFamily](Positional(), Dependency(),
    Phi(),
    NegationProperty(),
    Tails(),
    POS())
  */

  // Extract all the features ahead of time
  // Key: Paper ID
  // Value: Iterable of annotations ??
  println(s"Extracting all features of ${annotations.size} papers...")
  val data:Map[String, Iterable[RVFDatum[String, String]]] =
      annotations.map{
          case(name, ann) =>
            // Extract features:
            val features = extractFeatures(ann, featureFamilies).values

            (name -> features)
      }.toMap

  val writeFeaturesToCSV: Boolean = false
  if(writeFeaturesToCSV) {
      // Storage of the csv file's lines
      val csvLines = new mutable.ArrayBuffer[String]()
    
      val featuresNames = data.values.flatten.flatMap(_.features).toSet.toSeq.sorted
      val header = Seq("PMCID", "label") ++ featuresNames
      csvLines += header.mkString(",")
    
      for ((key, values) <- data) {
          val paperID = key
          for (value <- values) {
              val label = value.label
              val numbers = featuresNames map value.getFeatureCount map (_.toString)
              val row = (Seq(key, label) ++ numbers).mkString(",")
              csvLines += row
          }
      }
    
      val ow = new OutputStreamWriter(new FileOutputStream("features.csv"))
      for (line <- csvLines)
          ow.write(s"$line\n")
    
      ow.close()
  }

  val cvResults = new mutable.HashMap[String, BinaryClassificationResults]()
  val deterministicCVResults = new mutable.HashMap[String, BinaryClassificationResults]()
  val allResults = new mutable.ArrayBuffer[(Boolean, Boolean)]
  val allTrainingResults = new mutable.ArrayBuffer[(Boolean, Boolean)]
  val allDeterministicResults = new mutable.ArrayBuffer[(Boolean, Boolean)]
  val sparsenesses = new mutable.ArrayBuffer[Double]()
  val featureCounts = new mutable.ArrayBuffer[Double]()

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

      val sparsenessMeter = new SparsenessMeter
      // Training loop
      for(trainingFold <- trainingFolds){

        // Fetch the precomputed features of this paper
        val trainingData = data(trainingFold)
        
        // Balance dataset
        // val balancedSlice = balanceDataset(trainingData, negativesPerPositive = 3)

        // Add the data of this paper to the training dataset
        //for(datum <- balancedSlice) {
        for(datum <- trainingData){
          sparsenessMeter.accountVector(datum)
          trainingDataset += datum
        }
        
        //Could reduce computation by running featureSelection AFTER balanceDataset
        // Do feature selection (this function doesn't do anything right now but return its input with no side effects):
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
      // IT LOOKS LIKE THE TRAINING SET HAS NOT BEEN NORMALIZED.  balancedDataset is a val so it shouldn't change state, correct?

      // Train the classifier
      val classifier = train(balancedDataset)
      
      //Evaluate classifier performance on the training set
      // Evaluate the TRAINING data using the trained classifier
      val trainingSetResults = new mutable.ArrayBuffer[(Boolean, Boolean)]
      for(ix <- 0 until balancedDataset.size){
          val datum = balancedDataset.mkDatum(ix)
          val predictedLabel = classifier.classOf(datum) == "true"
          val truth = datum.label == "true"
          
          trainingSetResults += Tuple2(truth, predictedLabel)
      }
      
      
      // Now evaluate on test set:
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
     
      printTrainingResults(trainingSetResults)	
      
      val sparseness = sparsenessMeter.sparseness
      val features = sparsenessMeter.totalFeatures
      val bcr = new BinaryClassificationResults(results.toSeq)
      println(s"Sparseness: $sparseness\tTotal features: $features")
      println("ML classifier")
      println(bcr)
      println("Policy 4")
      println(policyResults)
      println
      cvResults += (evalFold -> bcr)
      allResults ++= results
      allTrainingResults ++= trainingSetResults
      sparsenesses += sparseness
      featureCounts += features

  }

  // Compute microaveraged scores for ML training, validation, and deterministic baseline
  val trainingMicroAverage = new BinaryClassificationResults(allTrainingResults)
  val microAverage = new BinaryClassificationResults(allResults)
  val policyMicroAverage = new BinaryClassificationResults(allDeterministicResults)

  println
  println(s"Microaveraged ML TRAINING results of ${keySet.size} folds:")
  println(trainingMicroAverage)
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

  val sparsenessStats = sampleStatistics(sparsenesses)
  val featureCountStats = sampleStatistics(featureCounts)

  val results = CrossValResults(microAverage, cvResults.values.toList,
    sparsenessStats, featureCountStats, featureFamilies.toSeq)

  // add output directory path from args (currently third argument) to filename:
  val outputDir = new File(args(2))
  // Create the directory if it doesn't exist yet
  if(!outputDir.exists())
    outputDir.mkdir()
  
  val outputFile = new File(outputDir, s"cv_${featureFamiliesList.mkString("_")}.ser")
  val filename = outputFile.getAbsolutePath

  // Save serialized output:
  Serializer.save(results, filename)

}



case class CrossValResults(results:BinaryClassificationResults,
                           foldsResults:Seq[BinaryClassificationResults],
                           sparseness:(Double, Double), // Sample mean and standard error of sparseness across folds
                           numFeatures:(Double, Double), // Sample mean and standard error of # present features across folds
                           featureFamilies:Seq[FeatureFamily])
