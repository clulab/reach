package edu.arizona.sista.reach.context

import edu.arizona.sista.reach.mentions._

/***
 * Base class for all context implementations
 * @author: Enrique Noriega <enoriega@email.arizona.edu>
 */
abstract class Context(vocabulary:Map[(String, String), Int], lines:Seq[Seq[BioMention]]){

  // To be overriden in the implementations
  protected def inferContext():Unit = {}

  // Inverse vocabulary to resolve the names back
  protected val inverseVocabulary = vocabulary map (_.swap)

  // Build sparse matrices
  // First, the observed value matrices
  protected val observedSparseMatrix:Seq[Seq[Int]] = lines map {
    _.map {
      elem => vocabulary(Context.getContextKey(elem))
    }
  }

  // Now the latent states matrix, the first step is to clone the observed matrix
  protected val latentSparseMatrix:List[Seq[Int]] = observedSparseMatrix.map(x=>x).toList

  // Apply context fillin heuristic
  inferContext()

  /***
   * Queries the context of the specified line line. Returns a sequence of tuples
   * where the first element is the type of context and the second element a grounded id
   */
  def query(line:Int):Seq[(String, String)] = latentSparseMatrix(line) map (inverseVocabulary(_))

  // Returns the matrices as a dense array to be printed
  def densifyMatrices():Seq[Array[Boolean]] = throw new NotImplementedError()
}

class DummyContext(vocabulary:Map[(String, String), Int], lines:Seq[Seq[BioMention]]) extends Context(vocabulary, lines){

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
