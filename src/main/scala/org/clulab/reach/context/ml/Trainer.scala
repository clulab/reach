package org.clulab.reach.context.ml

import java.io._
import collection.mutable.{ListBuffer, ArrayBuffer}
import util.Random
import ai.lum.common.Interval
import org.clulab.processors._
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.struct.Counter
import org.clulab.learning._
import org.clulab.odin._
import org.clulab.reach._
import org.clulab.reach.context.ContextEngine
import org.clulab.reach.context.dataset._
import org.clulab.reach.darpa.{DarpaActions, MentionFilter, NegationHandler}
import org.clulab.reach.context.dataset.ArticleAnnotations
import org.clulab.reach.mentions._

class PreAnnotatedDoc(val serializedDoc:String, val mentions:Seq[BioMention]) extends Serializable

object DatasetAnnotator extends App{
  // Preannotates the dataset and stores the serialized document in the directory

  val corpusDir = new File(args(0))

  // Loading reach system
  val reach = new ReachSystem
  val processor = reach.processor
  val docSerializer = new DocumentSerializer
  //
  val allAnnotations = corpusDir.listFiles.filter(_.isDirectory).map(d => ArticleAnnotations.readPaperAnnotations(d.getPath))

  for(annotations <- allAnnotations.par){
    val dir = new File(annotations.name)

    println(s"Started annotating $dir...")
    var doc = processor.mkDocumentFromSentences(annotations.sentences.values)
    doc = processor.annotate(doc)

    println(s"Extracting entities from $dir...")
    val entities = reach.extractEntitiesFrom(doc)

    val preprocessedAnnotations = new PreAnnotatedDoc(docSerializer.save(doc), entities)
    val oos = new ObjectOutputStream(new FileOutputStream(new File(dir, "preprocessed.ser")))
    oos.writeObject(preprocessedAnnotations)
    oos.close


    println(s"Finished annotating $dir...")
  }
}

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

    val sentences = annotations.sentences.values

    val (doc:Document, entities:Seq[BioMention]) = annotations.preprocessed match {
      case Some(preprocessed) =>
        val docSer = new DocumentSerializer
        (docSer.load(preprocessed.serializedDoc), preprocessed.mentions)
      case None =>
        println(s"Annotating ${annotations.name} ...")

        var d = processor.mkDocumentFromSentences(sentences)
        d = processor.annotate(d)
        println("Extracting entities ...")
        val e = reach.extractEntitiesFrom(d)
        (d, e)
    }


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

    println(s"DEBUG: Event #: ${events.size}")

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

        // Figure out the label
        val label = id.textBoundLocation.annotatedContexts match {
        case Some(contexts) =>
          contexts.exists(c => c == id.context)
        case None =>
          println("DEBUG: Warning, manually annotated event without context annotations!")
          false
        }

        // Compute the context type frequency
        // TODO: Check for missing in the context counts
        val contextTypeCount = contextCounts.lift(id.context.id).getOrElse(0)

        val datum = FeatureExtractor.mkRVFDatum(instances, contextTypeCount, label)
        (id, datum)
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

    val lrc = new LogisticRegressionClassifier[Boolean, String](C=0.1, bias=true)
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
    val dataset = new RVFDataset[Boolean, String]()

    // Extract features
    for(ann <- annotations){

      val data = extractFeatures(ann)
      // Add the data of this paper to the training dataset
      for(datum <- data.values){
        if(datum.label)
            println("Negative example")
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
    // classifier.saveTo(outputFile.getAbsolutePath)
    // Store the scalers
    // val fw = new FileWriter(outputFile.getAbsolutePath+".scalers")
    // scalers.saveTo(fw)
    // fw.close
  }

}
