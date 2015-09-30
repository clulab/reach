package edu.arizona.sista.reach.context

import scala.collection.mutable
import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.reach.mentions._

/***
 * Base class for all context implementations
 * @author: Enrique Noriega <enoriega@email.arizona.edu>
 */
abstract class Context(vocabulary:Map[(String, String), Int], lines:Seq[(Seq[BioMention], FriesEntry)]){

  // To be overriden in the implementations
  protected def inferContext:Unit

  // To be overriden in the implementations. Returns a sequence of (Type, Val) features
  // Feature order should be kept consisting for all return values
  protected def extractEntryFeatures(entry:FriesEntry):Array[(String, Double)]

  // Inverse vocabulary to resolve the names back
  protected val inverseVocabulary = vocabulary map (_.swap)

  //protected val entryFeaturesVocabulary:Map[(String, String), Int]

  // Build sparse matrices
  // First, the observed value matrices
  val mentions = lines map (_._1)
  val entryFeatures = lines map (_._2) map extractEntryFeatures

  protected val observedSparseMatrix:Seq[Seq[Int]] = mentions.map{
    _.map {
      elem => vocabulary(Context.getContextKey(elem))
    }
  }

  // Now the latent states matrix, the first step is to clone the observed matrix
  protected val latentSparseMatrix:List[Seq[Int]] = observedSparseMatrix.map(x=>x).toList

  // Apply context fillin heuristic
  inferContext

  /***
   * Queries the context of the specified line line. Returns a sequence of tuples
   * where the first element is the type of context and the second element a grounded id
   */
  def query(line:Int):Map[String, Seq[String]] = latentSparseMatrix(line) map (inverseVocabulary(_)) groupBy (_._1) mapValues (_.map(_._2))

  def densifyFeatures:Seq[Seq[Double]] = entryFeatures map { _.map(_._2).toSeq }

  def latentStateMatrix:Seq[Seq[Boolean]] = densifyMatrix(latentSparseMatrix) //TODO: Maybe filter the context relation cols

  def featureMatrix:Seq[Seq[Double]] = {
    val categorical = densifyMatrix(observedSparseMatrix)
    val numerical = densifyFeatures

    categorical zip numerical  map { case (c, n) => c.map{ case false => 0.0; case true => 1.0 } ++ n }
  }

  private def densifyMatrix(matrix:Seq[Seq[Int]]):Seq[Seq[Boolean]] = {
    // Recursive function to fill the "matrix"
    def _helper(num:Int, bound:Int, segment:List[Int]):List[Boolean] = {
      val currentVal = if(num == segment.head) true else false

      // Return now?
      if(num == bound -1)
        List(currentVal)
      else{
        if(currentVal)
          currentVal :: _helper(num+1, bound, segment.tail)
        else
          currentVal :: _helper(num+1, bound, segment)
      }
    }

    matrix map {
      row => {
        val sortedRow = row.sorted.toList
        _helper(0, vocabulary.size, sortedRow)
      }
    }
  }
}

class DummyContext(vocabulary:Map[(String, String), Int], lines:Seq[(Seq[BioMention], FriesEntry)]) extends Context(vocabulary, lines){
  protected override def inferContext:Unit = {}
  protected override def extractEntryFeatures(entry:FriesEntry):Array[(String, Double)] = Array()
}

object Context {
  // Seq of the labels we care about in context
  val contextMatching = Seq("Species", "Organ", "CellLine", "CellType")

  def getContextKey(mention:BioMention):(String, String) ={
    val id = if(mention.isGrounded) mention.xref match{
      case Some(xref) => xref.printString
      case None => "UNGROUNDED"
    } else "UNGROUNDED"

    val labels = mention.labels filter (contextMatching.contains(_))

    (labels.head, id)
  }
}
