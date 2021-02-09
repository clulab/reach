package org.clulab.reach.context.train.dataset

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.context.utils
import org.clulab.reach.context.utils.{Pair, PaperExtraction}
import org.clulab.utils.Serializer

/**
  * Create the raw entity pairs to be used for feature extraction down the pipeline
  */
object GeneratePairs extends App with LazyLogging {

  val config = ConfigFactory.load().getConfig("generatePairs")
  val inputPaperData = config.getString("inputPaperData")
  val inputExtractions = config.getString("inputExtractions")
  val outputPairsPath = config.getString("outputFile")

  def generatePairs(paperData: ManuallyAnnotatedData, extractions: Seq[PaperExtraction], validContextIds: Set[String]): Iterable[Pair] = {
    val annotations = paperData.annotations
    val textBoundMentions = extractions filter (m => m.grounding != "Event")

    for {
      (event, positiveClasses) <- annotations
      mention <- textBoundMentions
      if validContextIds contains mention.grounding
    } yield {
      if (positiveClasses contains mention.grounding)
        Pair(event, mention, isContext = true)
      else
        Pair(event, mention, isContext = false)
    }
  }

  // First read the annotations
  logger.info(s"Loading annotations from $inputPaperData")
  val paperAnnotations = utils.readSerializedPaperAnnotations(inputPaperData)

  // Then generate the set of candidate context mentions acceptable
  val validContexts =
    utils.generateValidContextIds(paperAnnotations)

  // Then read the extractions
  logger.info(s"Loading extractions from $inputExtractions")
  val paperExtractions = utils.readSerializedExtractions(inputExtractions)

  // Now generate the pairs
  val pairs =
    for ((pmcid, annotations) <- paperAnnotations)
      yield {
        logger.info(s"Generating pairs for $pmcid")
        val extractions = paperExtractions(pmcid)
        val pairs = generatePairs(annotations, extractions, validContexts)

        // Count the distribution of labels for the current paper
        logger.info(s"$pmcid has ${pairs.count(_.isContext)} out of ${pairs.size}")
        pmcid -> pairs
      }

  // Finally serialize the results
  logger.info(s"Saving pairs to $outputPairsPath")
  Serializer.save(pairs, outputPairsPath)
}
