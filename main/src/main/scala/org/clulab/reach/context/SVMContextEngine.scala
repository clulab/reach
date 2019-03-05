package org.clulab.reach.context
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}
import org.ml4ai.data.classifiers.LinearSVMWrapper
import org.ml4ai
import org.ml4ai.data.utils.correctDataPrep.AggregatedRowNew
import org.ml4ai.data.utils.oldDataPrep.InputRow

class SVMContextEngine extends ContextEngine {

  type Pair = (BioEventMention, BioTextBoundMention)
  type EventID = String

  var paperMentions:Option[Seq[BioTextBoundMention]] = None


  val svmWrapper = new LinearSVMWrapper(null)
  val trainedSVMInstance = svmWrapper.loadFrom("/Users/shraddha/datascience/ScalaContext/src/main/resources/svmTrainedModel.dat")

  override def assign(mentions: Seq[BioMention]): Seq[BioMention] = {
    paperMentions match {
      // If we haven't run infer, don't modify the mentions
      case None => mentions
      // If we have already run infer
      case Some(ctxMentions) =>

        // Collect the event mentions
        val evtMentions = mentions collect  {
          case evt:BioEventMention => evt
        }

        // Generate all the event/ctx mention pairs
        val pairs:Seq[Pair] = for(evt <- evtMentions; ctx <- ctxMentions) yield (evt, ctx)

        // Extract features for each of the pairs
        val features:Seq[InputRow] = pairs map extractFeatures

        // Aggregaget the features of all the instances of a pair
        val aggregatesFeatures:Map[EventID, (Pair, AggregatedRowNew)] =
          (pairs zip features).groupBy{
            case (pair, feats) => extractEvtId(pair._1) // Group by their EventMention
          }.mapValues(aggregateFeatures)


        // Run the classifier for each pair and store the predictions
        val predictions:Map[EventID, (Pair, Boolean)] =
          aggregatesFeatures map {
            case (evtId, (pair, aggregatedFeatures)) =>
              // TODO Shraddah: Uncomment this when ready
              //val prediction = trainedSVMInstance.predict(...)
              val prediction = true
              evtId -> (pair, prediction)
          }

        // Loop over all the mentions to generate the context dictionary
        for(mention <- mentions) yield {
          mention match {
            // If is an event mention, it's subject to context
            case evt: BioEventMention =>
              // Get its ID
              val evtId = extractEvtId(evt)
              // fetch its predicted pairs
              val contexts = predictions(evtId)

              // TODO Enrique: Assigns the context dictionary to the event mention
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
    paperMentions = Some(mentions collect {
      case tb:BioTextBoundMention if isContextMention(tb) => tb
    })
  }

  override def update(mentions: Seq[BioMention]): Unit = ()


  private def isContextMention(mention: BioTextBoundMention):Boolean =  ContextEngine.isContextMention(mention)

  private def extractFeatures(datum:(BioEventMention, BioTextBoundMention)):InputRow = throw new UnsupportedOperationException()

  private def extractEvtId(evt:BioEventMention):EventID = throw new UnsupportedOperationException()

  private def aggregateFeatures(instances:Seq[(Pair, InputRow)]):(Pair, AggregatedRowNew) = throw new UnsupportedOperationException()

}
