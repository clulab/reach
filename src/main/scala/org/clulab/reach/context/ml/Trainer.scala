package org.clulab.reach.context.ml

import java.io._
import collection.mutable.{ListBuffer, ArrayBuffer}
import util.Random
import ai.lum.common.Interval
import org.clulab.struct.Counter
import org.clulab.learning._
import org.clulab.odin._
import org.clulab.reach._
import org.clulab.reach.context.ContextEngine
import org.clulab.reach.context.dataset._
import org.clulab.reach.darpa.{DarpaActions, MentionFilter, NegationHandler}
import org.clulab.reach.context.dataset.ArticleAnnotations
import org.clulab.reach.mentions._

object Trainer {

  // Loading reach system
  lazy val reach = new ReachSystem
  lazy val processor = reach.processor

  def loadAnnotations(corpusDir:File):Iterable[ArticleAnnotations] = {
    println(s"Loading annotations from ${corpusDir.getPath} ...")
    // Load the annotations
    corpusDir.listFiles.filter(_.isDirectory).map(d => ArticleAnnotations.readPaperAnnotations(d.getPath))
  }

  def extractFeatures(annotations:ArticleAnnotations):Map[PairID, RVFDatum[Boolean, String]] = {
    println(s"Annotating ${annotations.name} ...")
    val sentences = annotations.sentences.values
    var doc = processor.mkDocumentFromSentences(sentences)
    doc = processor.annotate(doc)
    println("Extracting entities ...")
    val entities = reach.extractEntitiesFrom(doc)

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

    // Filter out non-context mentions
    val contextMentions = entities.filter(ContextEngine.isContextMention)
      .map(_.asInstanceOf[BioTextBoundMention])

    // Build the counts of the context annotations
    val contextCounts:Map[String, Int] = contextMentions.groupBy(_.nsId).mapValues(_.size)

    // Filter out the reach conetxt mentions that overlap with any of the manual annotations
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
    val pairs:Seq[PairFeatures] = FeatureExtractor.extractFeaturesFromCorpus(doc, events, manualContextAnnotations ++ filteredMentions)

    // Group the data by event location and context id
    val groupedPairs:Map[PairID, Seq[PairFeatures]] = pairs.groupBy(_.id)

    // Generate Datum objects for each group
    val data:Map[PairID, RVFDatum[Boolean, String]] = groupedPairs map {
      case (id:PairID, instances:Iterable[PairFeatures]) =>
        // Iterate over the instances to build a Datum instance
        val c = new Counter[String]

        for(i <- instances; f <- i.toSeq()){
          // Concatenate the feature name and its value
          c.incrementCount(f)
        }

        // Add the context id counts
        val contextTypeFreq = Seq.fill(contextCounts(id.context.id))("context_frequency")
        // Add the same feature multiple times according to the example
        contextTypeFreq foreach (c.incrementCount(_))

        // Figure out the label
        val truthContexts = id.textBoundLocation.annotatedContexts match {
          case Some(contexts) =>
            contexts.exists(c => c == id.context)
          case None =>
            println("DEBUG: Warning, manually annotated event without context annotations!")
            false
        }
        (id, new RVFDatum[Boolean, String](true, c))
    }

    data
  }

  def normalize(dataset:RVFDataset[Boolean, String]):ScaleRange[String] = Datasets.svmScaleDataset(dataset)

  def balanceDataset(dataset:RVFDataset[Boolean, String],
     negativesPerPositive:Int=4):RVFDataset[Boolean, String] = {

      val positiveIndices = 0.until(dataset.size).filter{
        i =>
          val lex = dataset.labelLexicon
          val labels = dataset.labels

          lex.get(labels(i))
      }

      val negativeIndices = 0.until(dataset.size).filter{
        i =>
          val lex = dataset.labelLexicon
          val labels = dataset.labels

          !lex.get(labels(i))
      }

      assert(positiveIndices.size <= negativeIndices.size)

      // Do a random sample of the negatives up to the specified ratio
      val suffledNegativeIndices = Random.shuffle(negativeIndices)

      val amount = positiveIndices.size * negativesPerPositive
      val toTake = if(amount <= negativeIndices.size) negativeIndices.size else amount

      val sampledNegativeIndices = negativeIndices.take(toTake)

      val indices2Keep = (positiveIndices ++ negativeIndices).sorted

      // Build a new dataset
      val ll = dataset.labelLexicon
      val fl = dataset.featureLexicon

      val labels = new ArrayBuffer[Int]
      val features = new ArrayBuffer[Array[Int]]
      val values = new ArrayBuffer[Array[Double]]
      for(i <- indices2Keep){
        labels += dataset.labels(i)
        features += dataset.features(i)
        values += dataset.values(i)
      }

      new RVFDataset(ll, fl, labels, features, values)
  }

  def train(dataset:RVFDataset[Boolean, String]):LogisticRegressionClassifier[Boolean, String] = {
    // Train the logistic regression

    // TODO: adjust the parameters as in the python code
    val lrc = new LogisticRegressionClassifier[Boolean, String](bias=true)
    lrc.train(dataset)

    // Return the trained logistic regression classifier
    lrc
  }

  def main(args:Seq[String]){
    // Trains a LM model out of the annotations and the reach mentions
    // First parameter: Corpus directory
    // Second parameter: output file

    println("== Context ML model training ==")
    println

    val corpusDir = new File(args(0))
    val outputFile = new File(args(1))

    val annotations = loadAnnotations(corpusDir)

    // Training dataset
    val dataset = new RVFDataset[Boolean, String]()

    // Extract features
    for(ann <- annotations){
      val data = extractFeatures(ann)
      // Add the data of this paper to the training dataset
      for(datum <- data.values){
        dataset += datum
      }
    }

    // Balance dataset
    val balancedDataset = balanceDataset(dataset)

    // Normalize dataset
    val scalers = normalize(balancedDataset)

    // Train the classifier
    val classifier = train(balancedDataset)

    // Store the trained model
    classifier.saveTo(outputFile.getAbsolutePath)
    // Store the scalers
    val fw = new FileWriter(outputFile.getAbsolutePath+".scalers")
    scalers.saveTo(fw)
    fw.close
  }

}
