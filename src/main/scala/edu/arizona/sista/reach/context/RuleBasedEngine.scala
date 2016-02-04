package edu.arizona.sista.reach.context.rulebased

import java.io._
import scala.annotation.tailrec
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

  protected var mentions:Seq[Seq[BioMention]] = _

  // Map of documents to offsets
  protected var docOffsets:Map[String, Int] = _

  // Build sparse matrices
  // First, the observed value matrices
  protected var entries:Seq[FriesEntry] = _
  protected var entryFeatures:Seq[Array[(String, Double)]] = _
  protected var observedSparseMatrix:Seq[Seq[Int]] = _

  // Now the latent states matrix, the first step is to clone the observed matrix
  protected var latentSparseMatrix:List[Seq[Int]] = _

  // Apply context fillin heuristic
  var inferedLatentSparseMatrix:List[Seq[Int]] = _
  //////////////////////////////////////////////////////////////////////////////////////////

  // Implementation of the infer method of the ContextEngine trait
  def infer(
      entries: Seq[FriesEntry],
      documents: Seq[Document],
      mentionsPerEntry: Seq[Seq[BioMention]]
  ) {
    // Build the document offsets
    val docLengths = documents.map(_.sentences.size)
    val docCumLengths:Seq[Int] = docLengths.scanLeft(0)((a, b) => a+b).dropRight(1)

    assert(docCumLengths.size == documents.size, "Something is wrong with context document offsets")

    docOffsets = documents.map(_.id.getOrElse("N/A")).zip(docCumLengths).toMap

    val lines:Seq[(Seq[BioMention], FriesEntry)] =  (0 until entries.size).flatMap{
      ix =>
        val entry = entries(ix)
        val doc = documents(ix)
        val men = mentionsPerEntry(ix)

        // Cast mentions as TextBound and sort by sentence index
        val sortedMentions = men.filter{
          case tb:BioTextBoundMention => true
          case _ => false
        }.sortWith(_.sentence < _.sentence)

        // Group mentions by sentence index and attach the fries entry
        val groupedMentions:Map[Int, Seq[BioMention]] = sortedMentions.groupBy(_.sentence)

        // Return seq of (Seq[BioMention], FriesEntry) tuples. Each corresponds to each line
        for(i <- 0 until doc.sentences.size) yield (groupedMentions.lift(ix).getOrElse(Nil).filter{
          mention => (ContextEngine.contextMatching map (mention.labels.contains(_))).foldLeft(false)(_||_) // This is a functional "Keep elements that have at least one of these"
        }, entry)
    }


    // Build sparse matrices
    // First, the observed value matrices
    mentions = lines map (_._1)
    val entryFeatures = lines map (_._2) map extractEntryFeatures

    observedSparseMatrix = mentions.map{
      _.map {
        elem => ContextEngine.getIndex(ContextEngine.getContextKey(elem), ContextEngine.featureVocabulary)
      }
    }

    latentSparseMatrix = mentions.map{
      _.map{
        elem =>
          val key = ContextEngine.getContextKey(elem)
          if (!key._1.startsWith("Context")) ContextEngine.getIndex(key, ContextEngine.latentVocabulary) else -1
          //filteredVocabulary(key)
      }.filter(_ != -1)
    }.toList

    inferedLatentSparseMatrix = inferContext

  }

  // Implementation of the update method of the ContextEngine trait
  def update(mentions: Seq[BioMention]) {}
  // Implementation of the assign method of the ContextEngine trait
  def assign(mentions: Seq[BioMention]): Seq[BioMention] = {
    mentions.foreach{
      case em:BioEventMention =>
        // Reconstruct the line number of the mention relative to the document
        val key = em.document.id.getOrElse("N/A")

        // if(!(docOffsets contains key)){
        //   println(s"KNF: $key in ${docOffsets.keys.mkString(" ")}")
        // }

        val offset = docOffsets(key)
        val relativeLine = em.sentence

        val line = offset + relativeLine

        // Query the context engine and assign it to the BioEventMention
        em.context = Some(this.query(line))
      case _ => Unit
    }

    mentions
  }


  // Internal methods

  protected def inferContext:List[Seq[Int]]
  /***
   * Queries the context of the specified line line. Returns a sequence of tuples
   * where the first element is the type of context and the second element a grounded id
   */
  protected def query(line:Int):Map[String, Seq[String]] = inferedLatentSparseMatrix(line) map ( ContextEngine.getKey(_, ContextEngine.latentVocabulary)) groupBy (_._1) mapValues (_.map(_._2))

  protected def densifyFeatures:Seq[Seq[Double]] = entryFeatures map { _.map(_._2).toSeq }

  def latentStateMatrix:Seq[Seq[Boolean]] = densifyMatrix(inferedLatentSparseMatrix, ContextEngine.latentVocabulary)

  def preprocessedLatentStateMatrix:Seq[Seq[Boolean]] = densifyMatrix(latentSparseMatrix, ContextEngine.latentVocabulary)

  protected def featureMatrix:Seq[Seq[Double]] = {
    val categorical = densifyMatrix(observedSparseMatrix, ContextEngine.featureVocabulary)
    // TODO Implement this is extra features are added
    //val numerical = densifyFeatures

//    categorical /*zip numerical*/  map { case (c, n) => c.map{ case false => 0.0; case true => 1.0 } /*++ n*/ }
    categorical  map { c => c.map{ case false => 0.0; case true => 1.0 }  }

  }

  protected def latentVocabulary = ContextEngine.latentVocabulary.keys map ( k => k._1 + "||" + ContextEngine.getDescription(k, ContextEngine.latentVocabulary))

  protected def observationVocavulary = ContextEngine.featureVocabulary.keys.map( k => k._1 + "||" + ContextEngine.getDescription(k, ContextEngine.featureVocabulary)) ++ entryFeaturesNames

  private def densifyMatrix(matrix:Seq[Seq[Int]], voc:Map[(String, String), String]):Seq[Seq[Boolean]] = {
    // Recursive function to fill the "matrix"
    @tailrec
    def _helper(num:Int, bound:Int, segment:List[Int], acc:List[Boolean]):List[Boolean] = {

      if(num == bound)
        return acc
      else{
        segment match {
          case Nil => _helper(num+1, bound, segment, false::acc)
          case _ =>
            val currentVal = if(num == segment.head) true else false

            if(currentVal){
              _helper(num+1, bound, segment.tail, currentVal::acc)
            }
            else{
              _helper(num+1, bound, segment, currentVal::acc)
            }

        }
      }
    }

    matrix map {
      row => {
        val sortedRow = row.sorted.toList
        _helper(0, voc.size, sortedRow, Nil)
      }
    }
  }
  //////////////////////////////////////////////////////////////////////////////////////////

  def getObservationsMatrixStrings:Seq[String] = featureMatrix map {
    step => step map ( x => f"$x%1.0f") mkString (" ")
  }

  def getStatesMatrixStrings:Seq[String] = latentStateMatrix map {
    step => step map (if(_) "1" else "0") mkString(" ")
  }
}
