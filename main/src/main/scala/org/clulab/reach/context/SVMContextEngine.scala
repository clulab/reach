package org.clulab.reach.context

import java.io._
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.context.classifiers.LinearSVMContextClassifier
import org.clulab.context.utils.{AggregatedContextInstance, ContextPairInstance}
import scala.collection.immutable

class SVMContextEngine(sentenceWindow:Option[Int] = None) extends ContextEngine with LazyLogging {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String
  type ContextID = (String, String)

  var paperMentions:Option[Seq[BioTextBoundMention]] = None
  var orderedContextMentions:Map[Int, Seq[BioTextBoundMention]] = _
  var defaultContexts:Option[Map[String, String]] = None

  val svmWrapper = new LinearSVMContextClassifier(null)

  val config = ConfigFactory.load()
  val configPath = config.getString("contextEngine.params.trainedSvmPath")
  val trainedSVMInstance = svmWrapper.loadFrom(configPath)
  val classifierToUse = trainedSVMInstance.classifier match {
    case Some(x) => x
  }


  logger.info(s"The SVM model has been tuned to the following settings: C: ${classifierToUse.C}, Eps: ${classifierToUse.eps}, Bias: ${classifierToUse.bias}")

  override def assign(mentions: Seq[BioMention]): Seq[BioMention] = {

    paperMentions match {
      // If we haven't run infer, don't modify the mentions
      case None => mentions
      // If we have already run infer
      case Some(ctxMentions) =>

        // Generate all the event/ctx mention pairs
        val pairGenerator = new EventContextPairGenerator(mentions, ctxMentions)
        val pairs = pairGenerator.yieldContextEventPairs()
        val filteredPairs = sentenceWindow match {
          case Some(bound) =>
            pairs.filter {
              case (evt, ctx) =>
                Math.abs(evt.sentence - ctx.sentence) <= bound
            }
          case None =>
            pairs
        }

        // here, we will use a Seq(Map), where each map has ContextPairInstance as a key, and as value, we have a tuple of feature values
        // so for a given ContextPairInstance, I can look up the table and return the values of the contextPairInput present in the ContextPairInstance.
        val tempo = filteredPairs.map{p =>
          val featureExtractor = new ContextFeatureExtractor(p, ctxMentions)
          featureExtractor.extractFeaturesToCalcByBestFeatSet()
        }
        val flattenedMap = tempo.flatMap(t=>t).toMap
        //val flattenedMap = ContextFeatValUtils.getFeatValMapPerInput(filteredPairs, ctxMentions)
        val contextPairInput:Seq[ContextPairInstance] = tempo.flatMap(t => t.keySet)
      // val contextPairInput:Seq[ContextPairInstance] = ContextFeatValUtils.getCtxPairInstances(filteredPairs, ctxMentions)
        val aggregatedFeatures:Map[EventID, Seq[(ContextID, AggregatedContextInstance)]] =
          (pairs zip contextPairInput).groupBy{
            case (pair, _) => extractEvtId(pair._1) // Group by their EventMention
          }.mapValues{
            v =>
              v.groupBy(r => ContextEngine.getContextKey(r._1._2)).mapValues(s =>  {
                val seqOfInputRowsToPass = s map (_._2)
                val featureAggregatorInstance = new ContextFeatureAggregator(seqOfInputRowsToPass, flattenedMap)
                val aggRow = featureAggregatorInstance.aggregateContextFeatures()
              aggRow}).toSeq
          }

        val predictions:Map[EventID, Seq[(ContextID, Boolean)]] = {
          val map = collection.mutable.HashMap[EventID, Seq[(ContextID, Boolean)]]()
          for((k,a) <- aggregatedFeatures) {

            val x = a.map {
              case (ctxId, aggregatedFeature) =>
                val predArrayIntForm = trainedSVMInstance.predict(Seq(aggregatedFeature))
                // comment row to file function before testing
                //writeRowToFile(aggregatedFeature, k.toString, ctxId._2)
                val prediction = {
                  predArrayIntForm(0) match {
                    case 1 => true
                    case 0 => false
                    case _ => false
                  }
                }
                logger.info(s"For the paper ${aggregatedFeature.PMCID}, event ID: ${k.toString} and context ID: ${ctxId._2}, we have prediction: ${predArrayIntForm(0)}")

                (ctxId, prediction)
            }

            val entry = Map(k -> x)
            map ++= entry

          }
          map.toMap
        }


        // Loop over all the mentions to generate the context dictionary
        for(mention <- mentions) yield {
          mention match {
            // If is an event mention, it's subject to context
            case evt: BioEventMention =>
              // Get its ID
              val evtId = extractEvtId(evt)
              // fetch its predicted pairs
              val contexts = predictions.getOrElse(evtId, Seq.empty)

              val contextMap =
                (contexts collect {
                  case (ctx, true) => ctx
                } groupBy (_._1)).mapValues(x => x.map(_._2))

              // Assign the context map to the mention
              evt.context = if(contextMap != Map.empty) Some(contextMap) else None
              // Return the modified event
              evt
            // If it's not an event mention, leave it as is
            case m: BioMention =>
              m
          }
        }
    }
  }

  // Pre-filter the context mentions
  override def infer(mentions: Seq[BioMention]): Unit = {
    val contextMentions = mentions filter ContextEngine.isContextMention map (_.asInstanceOf[BioTextBoundMention])
    paperMentions = Some(contextMentions)

    // code from rule based engine
    val entries = contextMentions groupBy (m => m.sentence)
    orderedContextMentions = immutable.TreeMap(entries.toArray:_*)
    val contextCounts:Map[(String, String), Int] = contextMentions map ContextEngine.getContextKey groupBy identity mapValues (_.size)
    val defaultContexts:Map[String, String] = contextCounts.toSeq.groupBy(_._1._1)
      // Sort them in decreasing order by frequency
      .mapValues(_.map(t => (t._1._2, t._2)))
      // And pick the id with of the type with highest frequency
      .mapValues(l => l.maxBy(_._2)._1)
    this.defaultContexts = Some(defaultContexts)
  }

  override def update(mentions: Seq[BioMention]): Unit = ()


  def extractEvtId(evt:BioEventMention):EventID = {
    val sentIndex = evt.sentence
    val tokenIntervalStart = (evt.tokenInterval.start).toString()
    val tokenIntervalEnd = (evt.tokenInterval.end).toString()
    sentIndex+tokenIntervalStart+tokenIntervalEnd
  }


  private def writeRowToFile(row:AggregatedContextInstance, evtID: String, ctxID: String):Unit = {
    val typeOfPaper = config.getString("svmContext.paperType")
    val dirForType = if(typeOfPaper.length != 0) config.getString("papersDir").concat(s"/${typeOfPaper}") else config.getString("papersDir")
    val fileListUnfiltered = new File(dirForType)
    val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
    val currentPMCID = s"PMC${row.PMCID.split("_")(0)}"
    for(file <- fileList) {
      val fileNamePMCID = file.getName.slice(0,file.getName.length-5)
      val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/${fileNamePMCID}")
      // creating output directory if it doesn't already exist
      val outputPaperDir = new File(outPaperDirPath)
      if(!outputPaperDir.exists()) {
        outputPaperDir.mkdirs()
      }

        if(currentPMCID == fileNamePMCID) {
          val pathForRow = outPaperDirPath.concat(s"/AggregatedRow_${currentPMCID}_${evtID}_${ctxID}.txt")
          val sentenceFile = new File(pathForRow)
          if (!sentenceFile.exists()) {
            sentenceFile.createNewFile()
          }
          val os = new ObjectOutputStream(new FileOutputStream(pathForRow))

          os.writeObject(row)
          os.close()
        }
    }
  }
}
