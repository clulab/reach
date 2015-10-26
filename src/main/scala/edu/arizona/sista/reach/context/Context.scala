package edu.arizona.sista.reach.context

import scala.collection.mutable
import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.reach.mentions._
import java.io._

/***
 * Base class for all context implementations
 * @author: Enrique Noriega <enoriega@email.arizona.edu>
 */
abstract class Context(vocabulary:Map[(String, String), Int], lines:Seq[(Seq[BioMention], FriesEntry)]){

  // To be overriden in the implementations
  protected def inferContext:List[Seq[Int]]

  // To be overriden in the implementations. Returns a sequence of (Type, Val) features
  // Feature order should be kept consisting for all return values
  protected def extractEntryFeatures(entry:FriesEntry):Array[(String, Double)]

  // Name of the entry features
  protected def entryFeaturesNames:Seq[String] = Seq()

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
  protected val latentSparseMatrix:List[Seq[Int]] = observedSparseMatrix.map(x=>x).map(_.filter(!inverseVocabulary(_)._1.startsWith("Context"))).toList

  // Apply context fillin heuristic
  protected val inferedLatentSparseMatrix:List[Seq[Int]] = inferContext

  /***
   * Queries the context of the specified line line. Returns a sequence of tuples
   * where the first element is the type of context and the second element a grounded id
   */
  def query(line:Int):Map[String, Seq[String]] = inferedLatentSparseMatrix(line) map (inverseVocabulary(_)) groupBy (_._1) mapValues (_.map(_._2))

  def densifyFeatures:Seq[Seq[Double]] = entryFeatures map { _.map(_._2).toSeq }

  def latentStateMatrix:Seq[Seq[Boolean]] = densifyMatrix(inferedLatentSparseMatrix)

  def featureMatrix:Seq[Seq[Double]] = {
    val categorical = densifyMatrix(observedSparseMatrix)
    val numerical = densifyFeatures

    categorical zip numerical  map { case (c, n) => c.map{ case false => 0.0; case true => 1.0 } ++ n }
  }

  def latentVocabulary = inverseVocabulary.values.filter(!_._1.startsWith("Context")).map(x => x._1 +  "||" + x._2)

  def observationVocavulary = inverseVocabulary.values.map(x => x._1 +  "||" + x._2) ++ entryFeaturesNames

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
}

class DummyContext(vocabulary:Map[(String, String), Int], lines:Seq[(Seq[BioMention], FriesEntry)]) extends Context(vocabulary, lines){
  protected override def inferContext = this.latentSparseMatrix
  protected override def extractEntryFeatures(entry:FriesEntry):Array[(String, Double)] = Array()
}

object Context {
  // Seq of the labels we care about in context
  val contextMatching = Seq("Species", "Organ", "CellLine", "CellType", "ContextPossesive", "ContextLocation", "ContextDirection")

  def getContextKey(mention:BioMention):(String, String) ={
    val id = if(mention.isGrounded) mention.xref match{
      case Some(xref) => xref.printString
      case None => "UNGROUNDED"
    } else "UNGROUNDED"

    val labels = mention.labels filter (contextMatching.contains(_))

    (labels.head, id)
  }

  // Writes the two matrix files to disk
  def outputContext(context:Context, path:String) = {

    val outObserved = path + ".obs"
    val outLatent = path + ".lat"

    val observedMatrix = context.featureMatrix
    val latentMatrix = context.latentStateMatrix

    // First output the latent states sequence
    val outStreamLatent:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outLatent)))

    for(step <- latentMatrix){
      val line = step map (if(_) "1" else "0") mkString(" ")
      outStreamLatent.println(line)
    }
    outStreamLatent.close()

    // Now the observed values
    val outStreamObserved:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outObserved)))
    for(step <- observedMatrix){
      val line = step map ( x => f"$x%1.0f") mkString (" ")
      outStreamObserved.println(line)
    }
    outStreamObserved.close()
  }

  def outputVocabularies(context:Context, path:String) = {
    val outObserved = path + ".obsvoc"
    val outLatent = path + ".latvoc"
    // First output the latent states voc
    val outStreamLatent:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outLatent)))

    context.latentVocabulary foreach {
      outStreamLatent.println(_)
    }

    outStreamLatent.close()

    // Now the observed voc
    val outStreamObserved:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outObserved)))

    context.observationVocavulary foreach {
      outStreamObserved.println(_)
    }

    outStreamObserved.close()
  }
import edu.arizona.sista.struct.Interval

trait GlobalContext{
  /**
   * ix is the index of the sentence relative to the document
   * Rreturns a map, where the key is the context type and the value a Sequence
   * of tuples where the first element is a grounded id and the second the interval of the
   * sentence where it appears
   */
  def query(ix:Int):Map[String, Seq[(String, Interval)]]
}

trait ContextEngine{
  def extractContext
  def integrateContext
}
