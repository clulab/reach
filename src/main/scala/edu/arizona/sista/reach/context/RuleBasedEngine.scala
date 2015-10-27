package edu.arizona.sista.reach.context.rulebased

import java.io._
import scala.collection.mutable
import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.context.ContextEngine

abstract class RuleBasedContextEngine extends ContextEngine {

  // Fields
  // To be overriden in the implementations. Returns a sequence of (Type, Val) features
  // Feature order should be kept consisting for all return values
  protected def extractEntryFeatures(entry:FriesEntry):Array[(String, Double)]

  // Name of the entry features
  protected def entryFeaturesNames:Seq[String] = Seq()

  protected var vocabulary:Map[(String, String), Int] = _

  // Inverse vocabulary to resolve the names back
  protected var inverseVocabulary:Map[Int, (String, String)] = _



  // Build sparse matrices
  // First, the observed value matrices
  protected var mentions:Seq[FriesEntry] = _
  protected var entryFeatures:Seq[Array[(String, Double)]] = _
  protected var observedSparseMatrix:Seq[Seq[Int]] = _

  // Now the latent states matrix, the first step is to clone the observed matrix
  protected var latentSparseMatrix:List[Seq[Int]] = _

  // Apply context fillin heuristic
  protected var inferedLatentSparseMatrix:List[Seq[Int]] = _
  //////////////////////////////////////////////////////////////////////////////////////////

  // Implementation of the infer method of the ContextEngine trait
  def infer(
      entries: Seq[FriesEntry],
      documents: Seq[Document],
      mentionsPerEntry: Seq[Seq[BioMention]]
  ) {
    // TODO: Build the vocabulary
    // vocabulary = _
    // TODO: Annotate the documents
    // vocabulary:Map[(String, String), Int], lines:Seq[(Seq[BioMention], FriesEntry)]
    // TODO:
    //inverseVocabulary = vocabulary map (_.swap)

    // Build sparse matrices
    // First, the observed value matrices
    //mentions = lines map (_._1)
    //entryFeatures = lines map (_._2) map extractEntryFeatures
    // observedSparseMatrix:Seq[Seq[Int]] = mentions.map{
    //   _.map {
    //     elem => vocabulary(ContextEngine.getContextKey(elem))
    //   }
    // }
    // inferedLatentSparseMatrix = inferContext

    //latentSparseMatrix = observedSparseMatrix.map(x=>x).map(_.filter(!inverseVocabulary(_)._1.startsWith("Context"))).toList
  }

  // Implementation of the update method of the ContextEngine trait
  def update(mentions: Seq[BioMention]) {}
  // Implementation of the assign method of the ContextEngine trait
  def assign(mentions: Seq[BioMention]): Seq[BioMention] = mentions


  // Internal methods
  /***
   * Queries the context of the specified line line. Returns a sequence of tuples
   * where the first element is the type of context and the second element a grounded id
   */
  protected def query(line:Int):Map[String, Seq[String]] = inferedLatentSparseMatrix(line) map (inverseVocabulary(_)) groupBy (_._1) mapValues (_.map(_._2))

  protected def densifyFeatures:Seq[Seq[Double]] = entryFeatures map { _.map(_._2).toSeq }

  protected def latentStateMatrix:Seq[Seq[Boolean]] = densifyMatrix(inferedLatentSparseMatrix)

  protected def featureMatrix:Seq[Seq[Double]] = {
    val categorical = densifyMatrix(observedSparseMatrix)
    val numerical = densifyFeatures

    categorical zip numerical  map { case (c, n) => c.map{ case false => 0.0; case true => 1.0 } ++ n }
  }

  protected def latentVocabulary = inverseVocabulary.values.filter(!_._1.startsWith("Context")).map(x => x._1 +  "||" + x._2)

  protected def observationVocavulary = inverseVocabulary.values.map(x => x._1 +  "||" + x._2) ++ entryFeaturesNames

  private def densifyMatrix(matrix:Seq[Seq[Int]]):Seq[Seq[Boolean]] = {
    // Recursive function to fill the "matrix"
    def _helper(num:Int, bound:Int, segment:List[Int]):List[Boolean] = {

      if(num == bound)
        return List()
      else{
        segment match {
          case Nil => false :: _helper(num+1, bound, segment)
          case _ =>
            val currentVal = if(num == segment.head) true else false

            if(currentVal)
              currentVal :: _helper(num+1, bound, segment.tail)
            else
              currentVal :: _helper(num+1, bound, segment)

        }
      }
    }

    matrix map {
      row => {
        val sortedRow = row.sorted.toList
        _helper(0, vocabulary.size, sortedRow)
      }
    }
  }
  //////////////////////////////////////////////////////////////////////////////////////////
}
