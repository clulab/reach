package org.clulab.context.ml

import java.io._

import collection.mutable
import util.Random
import ai.lum.common.Interval
import com.typesafe.scalalogging.LazyLogging
import org.clulab.processors._
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.struct.Counter
import org.clulab.learning._
import org.clulab.odin._
import org.clulab.reach._
import org.clulab.serialization.DocumentSerializer
import org.clulab.context.ContextEngine
import org.clulab.context.ml.dataset._
import org.clulab.reach.darpa.{DarpaActions, MentionFilter, NegationHandler}
import org.clulab.reach.mentions._
import org.clulab.context._
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.reach.mentions.serialization.json.JSONSerializer
import org.json4s.native.JsonMethods._
import org.clulab.utils.Serializer
import libsvm._

import scala.collection.immutable.IndexedSeq

class PreAnnotatedDoc(val serializedDoc:String, val mentions:String) extends Serializable

/***
  * Generates a set of Processor's document objects for each file in the directory The first argument is the path to the
  * corpus
  */
object DatasetAnnotator extends App with LazyLogging{
  // Preannotates the dataset and stores the serialized document in the directory

  def annotatePaper(annotations:ArticleAnnotations):PreAnnotatedDoc = {

    val name = annotations.name
    println(s"Started annotating $name...")
    val sentences = annotations.sentences
    // TODO: Change this to use white space tokenizing
    var doc = processor.mkDocumentFromSentences((0 until sentences.size).map(sentences))
    //var doc = processor.mkDocumentFromTokens((0 until sentences.size).map(sentences).map(_.split(" ").toSeq).toSeq)

    // Make sure we have the same number of sentences in the document object as in the annotations
    if (sentences.size != doc.sentences.size) {
      val s = s"Document ${annotations.name} has fewer sentences than it should. Sentences file: ${sentences.size}\tDocument: ${doc.sentences.size}"
      logger.info(s)
    }
    // Make sure the tokens are the same
    for (i <- 0 until sentences.size) {
      val groundTruthTokens = sentences(i).split(" ").filter(_ != "").size
      val annotatedTokens = doc.sentences(i).words.size

      if (groundTruthTokens != annotatedTokens) {
        val s = s"Document ${annotations.name}, line $i has different number of tokens than it should. Ground truth:$groundTruthTokens\tAnnotated:$annotatedTokens"
        logger.info(s)
      }
    }

    doc = processor.annotate(doc)

    println(s"Extracting entities from $name...")
    val entities = reach.extractEntitiesFrom(doc).map(_.asInstanceOf[CorefMention])

    val serializedEntities: String = compact(render(JSONSerializer.jsonAST(entities)))

    val preprocessedAnnotations = new PreAnnotatedDoc(docSerializer.save(doc), serializedEntities)

    preprocessedAnnotations
  }

  val corpusDir = new File(args(0))

  // Loading reach system
  val reach = new ReachSystem
  val processor = reach.processor
  val docSerializer = new DocumentSerializer
  val directories = corpusDir.listFiles.filter(_.isDirectory)
  //
  val allAnnotations = directories.map(d => ArticleAnnotations.readPaperAnnotations(d.getPath))

  for((dir, annotations) <- directories.zip(allAnnotations).par){
    //val dir = new File(annotations.name)


    val preprocessedAnnotations = annotatePaper(annotations)
    Serializer.save[PreAnnotatedDoc](preprocessedAnnotations, new File(dir, "preprocessed.ser").getAbsolutePath)


    println(s"Finished annotating $dir...")
  }
}

/***
  * Trains a context model from the corpus
  */
object Trainer {

  // Loading reach system
  lazy val reach = new ReachSystem
  lazy val processor = reach.processor

  def loadAnnotations(corpusDir:File):Iterable[ArticleAnnotations] = {
    println(s"Loading annotations from ${corpusDir.getPath} ...")
    // Load the annotations
    corpusDir.listFiles.filter(_.isDirectory).map(d => ArticleAnnotations.readPaperAnnotations(d.getPath))
      .filter(_.eventAnnotations.size >= 20)
  }

  def extractFeatures(annotations:ArticleAnnotations, featureFamilies:Set[FeatureFamily]):Map[PairID, RVFDatum[String, String]] = {

    val sentences = annotations.sentences.values

    val preprocessed = annotations.preprocessed match {
      case Some(preprocessed) => preprocessed
      case None =>
        println(s"Annotating ${annotations.name} ...")
        val a = DatasetAnnotator.annotatePaper(annotations)
        a
    }

    val docSer = new DocumentSerializer
    val jsonAST = parse(preprocessed.mentions)
    val e = JSONSerializer.toCorefMentions(jsonAST)
    val (doc:Document, entities:Seq[BioMention]) = (docSer.load(preprocessed.serializedDoc), e.map(_.asInstanceOf[BioMention]))


    // Don't use the system-extracted events for trainig
    // var rawMentions = reach.extractEventsFrom(doc, entities)
    // val events:Seq[BioEventMention] = MentionFilter.keepMostCompleteMentions(
    //   rawMentions, State(rawMentions)).filter{
    //   case ev:BioEventMention => true
    //   case _ => false
    // }.map(_.asInstanceOf[BioEventMention])
    ////////////////////////////////////////////////////

    // Use the manually annotated events for training
    var events = annotations.eventAnnotations

    // Filter out non-context mentions and cellular component mentions, because we don't have training data for this.
    //val contextMentions = entities.filter(e => ContextClass.isContextMention(e) && e.label == "Cellular_component")
    val contextMentions = entities.filter(e => ContextClass.isContextMention(e))
      .map(_.asInstanceOf[BioTextBoundMention])

    // Build the counts of the context annotations
    val contextCounts:mutable.Map[String, Int] = mutable.HashMap() ++ contextMentions.groupBy(_.nsId).mapValues(_.size)

    // Add the counts of the manual annotations
    for(ct <- annotations.contextAnnotations.map(_.contextType)){
      if(contextCounts.contains(ct.id)){
        contextCounts(ct.id) += 1
      }else{
        contextCounts += ct.id -> 1
      }
    }


    // Filter out the reach context mentions that overlap with any of the manual annotations
    val manualContextAnnotations = annotations.contextAnnotations

    val filteredMentions = contextMentions.filter{
      m =>
        !manualContextAnnotations.exists{
          mc =>
            val i = Interval.closed(m.tokenInterval.start, m.tokenInterval.end)
            mc.sentenceId == m.sentence && mc.interval.intersects(i)
        }
    }.map(FeatureExtractor.contextMention2Annotation)

    // Extract features from reach's mentions
    println("Extracting features ...")
    val pairs:Seq[PairFeatures] = FeatureExtractor.extractFeaturesFromCorpus(doc, events, manualContextAnnotations ++ filteredMentions, featureFamilies)

    // Group the data by event location and context id
    val groupedPairs:Map[PairID, Seq[PairFeatures]] = pairs.groupBy(_.id)

    // Generate Datum objects for each group
    val data:Map[PairID, RVFDatum[String, String]] = groupedPairs map {
      case (id:PairID, instances:Iterable[PairFeatures]) =>

        // Figure out the label
        val label = id.textBoundLocation.annotatedContexts match {
        case Some(contexts) =>
          if(contexts.exists(c => c == id.context)) "true" else "false"
        case None =>
          println("DEBUG: Warning, manually annotated event without context annotations!")
          "false" // False
        }

        // Compute the context type frequency
        // TODO: Check for missing in the context counts
        val contextTypeCount = contextCounts.lift(id.context.id).getOrElse(0)

        val datum = FeatureExtractor.mkRVFDatum(instances, contextTypeCount, label)
        (id, datum)
    }

    // Filter out the negatives that don't pass a threshold
    val filteredData = data.filter{
      case (pairId, datum) =>
        if(datum.label == "false"){
          if(datum.getFeatureCount("context_frequency") > 5) // TODO: Parameterize this value
            true
          else
            false
        }
        else{
          true
        }
    }

    data
  }

  def normalize(dataset:RVFDataset[String, String]):ScaleRange[String] = Datasets.svmScaleDataset(dataset)

  def balanceDataset(data:Iterable[RVFDatum[String, String]], negativesPerPositive:Int) = {
    val positiveIndices = data.zipWithIndex.filter{
      case (datum, ix) =>
        if(datum.label == "true") true else false
    }.map(_._2)

    val negativeIndices = data.zipWithIndex.filter{
      case (datum, ix) =>
        if(datum.label == "false") true else false
    }.map(_._2)

    assert(positiveIndices.size <= negativeIndices.size, "There are more positive than negative instances")

    // Do a random sample of the negatives up to the specified ratio
    val suffledNegativeIndices = Random.shuffle(negativeIndices)

    val amount = positiveIndices.size * negativesPerPositive
    val toTake = if(amount <= negativeIndices.size) amount else negativeIndices.size

    val sampledNegativeIndices = negativeIndices.take(toTake)

    val indices2Keep = (positiveIndices ++ sampledNegativeIndices).toSeq.sorted

    indices2Keep.map(data.toSeq)
  }

  def sortByVectorDifference(dataset: RVFDataset[String, String], positiveIndices: IndexedSeq[Int], negativeIndices: IndexedSeq[Int]) = {
    def vectorDifference(keys: Set[String], a: RVFDatum[String, String], b: RVFDatum[String, String]): Double = {
      val diffs = keys.par.map {
        k =>
          a.getFeatureCount(k) - b.getFeatureCount(k)
      }.seq

      val magnitude = Math.sqrt(diffs.map(v => v*v).sum)

      magnitude
    }

    val keys = dataset.featureLexicon.keySet.toSet
    val positives = positiveIndices map dataset.mkDatum map (_.asInstanceOf[RVFDatum[String, String]])

    val negativeRanks = for(i <- negativeIndices) yield {
      val datum = dataset.mkDatum(i).asInstanceOf[RVFDatum[String, String]]
      val distances = positives map {
        p =>
          val distance = vectorDifference(keys, datum, p)

          distance
      }

      distances.min
    }

    val candidates  = negativeIndices zip negativeRanks

    candidates.sortBy{case (ix, d) => d}.map(_._1)
  }

  def balanceDataset(dataset:RVFDataset[String, String],
                     negativesPerPositive:Int=4):RVFDataset[String, String] = {

    // Here we can change the implementation of data set balancing to something else
    //randomlyBalanceDataset(dataset, negativesPerPositive)
    // We could add another implementation here, for example, the euclidian distance similarity:
    euclidianDistanceBalanceDataset(dataset, negativesPerPositive)
  }

  private def euclidianDistanceBalanceDataset(dataset:RVFDataset[String, String],
                                              negativesPerPositive:Int):RVFDataset[String, String] = {
    val (positiveIndices: IndexedSeq[Int], negativeIndices: IndexedSeq[Int]) = classIndices(dataset)
    
    // instantiate an empty set of selectedNegative indices:
    val selectedNegatives = new mutable.HashSet[Int]()
    // make a set of negativeIndices that have not been selected to search through:
    //var unselectedNegatives = negativeIndices
    // or instead just test whether the new negative is already inside of selectedNegatives
    
    //TODO: Implement here the selection loop
    // for number of negatives per positive:
    val nPPRange = 1 to negativesPerPositive
    for(nPP <- nPPRange) {
      for (positiveIndex <- positiveIndices) {
        println(s"finding closest negative training example for $positiveIndex")
        val positiveDatum = dataset.mkDatum(positiveIndex).asInstanceOf[RVFDatum[String, String]]
        // For each negative index, make the negative datum and compute distance_i to positive datum.
        //   If the distance is less than minDist, store the negative index and set minDist to distance_i
        var minDist = Double.MaxValue
        
        // Declare closestNegative which is the index of negativeIndices that indexes a datum which should be closest to the positive datum
        // type Int:
        var closestNegative:Int = 0
        
        // Make subset of negativeIndices containing only those indices which have not been selected:
        val unselectedNegatives = negativeIndices.filterNot(selectedNegatives)
        for (negativeIndex <- unselectedNegatives) {
          val negativeDatum = dataset.mkDatum(negativeIndex).asInstanceOf[RVFDatum[String, String]]
          
          //Compute distance:
          //println(s"minDist = $minDist")
          val distance = euclidianDistance(positiveDatum, negativeDatum)
          //println(s"distance = $distance")
          closestNegative = if (distance < minDist){
            negativeIndex
          } else {closestNegative}
          minDist = if (distance < minDist){
            distance
          } else {minDist}
          //println(s"minDist after comparing with distance = $minDist")
          //println(s"closestNegative: $closestNegative")
          // end for loop
        }
        // Store the negative index in selectedNegatives:
        // changed selectedNegatives ++= closestNegative to the following code:
        println(s"closestNegative = $closestNegative")
        selectedNegatives += closestNegative
        // ++= wants an iterable.  From the docs: Add all the elements provided by an iterator elems to the set.
        // Make sure to not consider this index again if stored. Take it out of the "pool"
      }
    }
    // Create a new dataset object with only the chosen elements
    buildDatasetObject(dataset, positiveIndices ++ selectedNegatives)
  }
  


  private def euclidianDistance(a:RVFDatum[String, String], b:RVFDatum[String, String]):Double = {
    def sqr(v:Double) = v*v
    val (labelsA, labelsB) = (a.features.toSet, b.features.toSet)

    val intersection = (labelsA & labelsB).map(i => a.getFeatureCount(i) - b.getFeatureCount(i)).map(sqr)
    val onlyA = labelsA.diff(labelsB).map(a.getFeatureCount).map(sqr)
    val onlyB = labelsB.diff(labelsA).map(b.getFeatureCount).map(sqr)

    // Compute the sum of squares
    val sum = intersection.sum + onlyA.sum + onlyB.sum

    // Return the square root
    Math.sqrt(sum)
  }



  private def randomlyBalanceDataset(dataset:RVFDataset[String, String],
                     negativesPerPositive:Int):RVFDataset[String, String] = {

    val (positiveIndices: IndexedSeq[Int], negativeIndices: IndexedSeq[Int]) = classIndices(dataset)

    assert(positiveIndices.size <= negativeIndices.size, "There are more positive than negative instances")

    // Sort the negatives by vector difference
    //val sortedNegatives = sortByVectorDifference(dataset, positiveIndices, negativeIndices)

    // Do a random sample of the negatives up to the specified ratio
    val suffledNegativeIndices = Random.shuffle(negativeIndices)

    val amount = positiveIndices.size * negativesPerPositive
    val toTake = if(amount <= negativeIndices.size) amount else negativeIndices.size

    val sampledNegativeIndices = suffledNegativeIndices.take(toTake)//sortedNegatives.take(toTake) //negativeIndices.take(toTake)

    val indices2Keep = (positiveIndices ++ sampledNegativeIndices).sorted

    buildDatasetObject(dataset, indices2Keep)
  }

  private def buildDatasetObject(original: RVFDataset[String, String], indices2Keep: Seq[Int]) = {
    // Build a new dataset
    val ll = original.labelLexicon
    val fl = original.featureLexicon

    val labels = new mutable.ArrayBuffer[Int]
    val features = new mutable.ArrayBuffer[Array[Int]]
    val values = new mutable.ArrayBuffer[Array[Double]]
    for (i <- indices2Keep) {
      labels += original.labels(i)
      features += original.features(i)
      values += original.values(i)
    }

    new RVFDataset(ll, fl, labels, features, values)
  }

  private def classIndices(dataset: RVFDataset[String, String]) = {
    val positiveIndices = 0.until(dataset.size).filter {
      i =>
        val lex = dataset.labelLexicon
        val labels = dataset.labels

        lex.get(labels(i)) == "true"
    }

    val negativeIndices = 0.until(dataset.size).filter {
      i =>
        val lex = dataset.labelLexicon
        val labels = dataset.labels

        lex.get(labels(i)) == "false"
    }
    (positiveIndices, negativeIndices)
  }

  def train(dataset:RVFDataset[String, String]) = {
    // Train the logistic regression

    // Measure sparsity
//    val dimensions = dataset.featureLexicon.size.toDouble
//    val sum = dataset.values.map(_.size/dimensions).sum
//    val denseness = sum / dataset.values.size
//
//    println(s"DENSENESS: $denseness")

    val lrc = new L1LogisticRegressionClassifier[String, String](C=.005, bias=true)
    //val lrc = new LogisticRegressionClassifier[String, String](C=.005, bias=true)

    //val params:svm_parameter = new svm_parameter
    //val lrc = new LibSVMClassifier[String, String](RBFKernel, probability = false)
    lrc.train(dataset)

    // Return the trained logistic regression classifier
    lrc
  }

  def main(args:Array[String]){
    // Trains a LM model out of the annotations and the reach mentions
    // First parameter: Corpus directory
    // Second parameter: output file

    println("== Context ML model training ==")
    println

    val corpusDir = new File(args(0))
    val outputFile = new File(args(1))

    val annotations = loadAnnotations(corpusDir)

    // Training dataset
    val dataset = new RVFDataset[String, String]()

    val featureFamilies = Set[FeatureFamily](Positional(), Dependency(),
      Phi(),
      NegationProperty(),
      Tails(),
      POS())

    // Extract features
    for(ann <- annotations){

      val data = extractFeatures(ann, featureFamilies)
      // Add the data of this paper to the training dataset
      for(datum <- data.values){
        dataset += datum
      }
    }

    // Balance dataset
    val balancedDataset = balanceDataset(dataset)

    // Normalize dataset
    val scalers:ScaleRange[String] = normalize(balancedDataset)

    // Train the classifier
    val classifier = train(balancedDataset)

    // Store the trained model
    classifier.saveTo(outputFile.getAbsolutePath)
    // Store the scalers
    // Old code that uses the built-in text serialization
    // val fw = new FileWriter(outputFile.getAbsolutePath+".scalers")
    // scalers.saveTo(fw)
    // fw.close

    Serializer.save[ScaleRange[String]](scalers, outputFile.getAbsolutePath+".scalers")
  }

}
