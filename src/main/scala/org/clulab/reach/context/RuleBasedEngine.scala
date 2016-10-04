package org.clulab.reach.context

import scala.annotation.tailrec
import org.clulab.reach.mentions._


abstract class RuleBasedContextEngine extends ContextEngine {

  // Fields
  // To be overriden in the implementations. Returns a sequence of (Type, Val) features
  // Feature order should be kept consisting for all return values
  // protected def extractEntryFeatures(entry:FriesEntry):Array[(String, Double)]

  // Name of the entry features
  protected def entryFeaturesNames:Seq[String] = Seq()

  protected var mentions:Seq[Seq[BioMention]] = _

  // Map of documents to offsets
  protected var docOffsets:Map[String, Int] = _

  // Default species context
  protected var fallbackSpecies:Option[String] = None

  // Build sparse matrices
  // First, the observed value matrices
  // protected var entries:Seq[FriesEntry] = _
  protected var entryFeatures:Seq[Array[(String, Double)]] = _
  protected var observedSparseMatrix:Seq[Seq[Int]] = _

  // Now the latent states matrix, the first step is to clone the observed matrix
  protected var latentSparseMatrix:List[Seq[Int]] = _

  // Apply context fillin heuristic
  var inferedLatentSparseMatrix:List[Seq[Int]] = _
  //////////////////////////////////////////////////////////////////////////////////////////

  // Implementation of the infer method of the ContextEngine trait
  def infer(biomentions: Seq[BioMention]): Unit = {

    if (biomentions.isEmpty) return
    // all mentions belong to the same doc
    val doc = biomentions.head.document

    // Compute the fallback species
    val speciesId: Seq[String] = biomentions.filter {
      case tb: BioTextBoundMention => tb.matches("Species")
      case _ => false
    }.map(ContextEngine.getContextKey(_)._2)

    // sort species ids by count in descending order
    val speciesIdSorted: Seq[String] = speciesId
      .groupBy(identity)
      .mapValues(_.length)
      .toList
      .sortBy(-_._2)
      .map(_._1)

    // Assign the fallback species, if any
    fallbackSpecies = speciesIdSorted.headOption

    // Build sparse matrices
    // First, the observed value matrices
    val myMentions = biomentions.filter(ContextEngine.isContextMention).groupBy(_.sentence).withDefaultValue(Nil)
    mentions = doc.sentences.indices.map(myMentions).toVector
    // FIXME what are the entry features???
    //val entryFeatures = lines map (_._2) map extractEntryFeatures

    observedSparseMatrix = mentions.map{
      _.map {
        elem => ContextEngine.getIndex(ContextEngine.getContextKey(elem), ContextEngine.featureVocabularyKeys)
      }
    }

    latentSparseMatrix = mentions.map{
      _.map{
        elem =>
          val key = ContextEngine.getContextKey(elem)
          if (!key._1.startsWith("Context")) ContextEngine.getIndex(key, ContextEngine.latentVocabularyKeys) else -1
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
      em:BioMention =>
        // Reconstruct the line number of the mention relative to the document
        // Only assing context to mentions that aren't actually a context mention
        if(!ContextEngine.isContextMention(em)){
          val key = em.document.id.getOrElse("N/A")

          val line = em.sentence

          // Query the context engine and assign it to the BioEventMention
          val mentionContext = this.query(line).toList.toMap
          val fallbackContext = if(mentionContext.contains("Species"))
              mentionContext
          else
              fallbackSpecies match {
                  case Some(s) => mentionContext + ("Species" -> Seq(s))
                  case None => mentionContext
              }

          em.context = Some(fallbackContext)
        }
    }

    mentions
  }


  // Internal methods

  protected def inferContext:List[Seq[Int]]
  /***
   * Queries the context of the specified line. Returns a sequence of tuples
   * where the first element is the type of context and the second element a grounded id
   */
  protected def query(line:Int):Map[String, Seq[String]] =
    try{
      inferedLatentSparseMatrix(line) map ( ContextEngine.getKey(_, ContextEngine.latentVocabularyKeys)) groupBy (_._1) mapValues (_.map(_._2))
    }
    catch{
      // There were no conetxt mentions when calling infer, thus we return an empty map
      case e:NullPointerException => Map()
    }

  protected def densifyFeatures:Seq[Seq[Double]] = entryFeatures map { _.map(_._2).toSeq }

  def latentStateMatrix:Seq[Seq[Boolean]] = densifyMatrix(inferedLatentSparseMatrix, ContextEngine.latentVocabulary)

  def preprocessedLatentStateMatrix:Seq[Seq[Boolean]] = densifyMatrix(latentSparseMatrix, ContextEngine.latentVocabulary)

  protected def featureMatrix:Seq[Seq[Double]] = {
    val categorical = densifyMatrix(observedSparseMatrix, ContextEngine.featureVocabulary)
    // TODO Implement this is extra features are added
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
    step => step map (if(_) 1 else 0) mkString(" ")
  }
}
