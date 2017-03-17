package org.clulab.reach.focusedreading

import org.clulab.utils.Serializer

/**
  * Created by enrique on 21/12/16.
  */
class TfIdf(raw:Iterable[(String, Iterable[Connection])]) {

  // Alternate constructor
  def this(path:String){
    this(Serializer.load[Iterable[(String, Iterable[Connection])]](path))
  }

  // Compute the term frequencies
  private val _tf:Map[String,Map[Connection,Int]] = raw.map{
    case (name, doc) =>
        // Compute the document frequencies
        val freq = doc groupBy identity mapValues (_.size)
        (name -> freq)
  }.toMap

  def tf(docName:String, activation:Connection):Int = _tf.lift(docName) match {
    case Some(counts) =>
      counts.lift(activation) match {
        case Some(count) => count
        case None => 0
      }
    case None => 0
  }

  // Comnpute IDF
  private val _idf:Map[Connection, Double] = {
    // Create the tuples to be grouped
    val pairs = raw flatMap {
      case (name, doc) =>
        doc map {c => (name, c)}
    }

    val groups:Map[Connection, Int] = pairs groupBy (_._2) mapValues (_.size)

    // Number of documents
    val N = raw.size
    // Compute IDF
    val idf = groups mapValues ( i => N/(1+math.log(i)) )

    idf
  }

  def idf(activation:Connection):Double = _idf.lift(activation) match {
    case Some(d) => d
    case None => 0
  }

  def tfidf(docName:String, activation:Connection) = tf(docName,activation)*idf(activation)
}
