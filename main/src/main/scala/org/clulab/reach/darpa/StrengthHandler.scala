package org.clulab.reach.darpa

import org.clulab.odin.{EventMention, Mention, State}
import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention, Hypothesis}
import org.clulab.struct.{DirectedGraph, Interval}

object StrengthHandler {
  val degree = 2 // Degree up to which we should follow the links in the graph


  val strongLemmas = Set("higher", "positively", "increase", "elevated")
  val weakLemmas = Set("lower", "negatively", "decrease", "reduce")

  // Recursive function that helps us get the words outside the event
  def getSpannedIndexes(index: Int, degree: Int, dependencies: DirectedGraph[String]): Seq[Int] = {
    degree match {
      case 0 => Seq[Int]() // Base case of the recursion
      case _ =>

        val outgoing = dependencies.outgoingEdges
        val incoming = dependencies.incomingEdges

        // Get incoming and outgoing edges
        val t: Seq[(Int, String)] = incoming.lift(index) match {
          case Some(x) => x
          case None => Seq()
        }

        val edges = t ++ (outgoing.lift(index) match {
          case Some(x) => x
          case None => Seq()
        })


        // Each edge is a tuple of (endpoint index, edge label), so we map it to the first
        // element of the tuple
        val indexes: Seq[Int] = edges map (_._1)

        // Recursively call this function to get outter degrees
        val higherOrderIndexes: Seq[Int] = indexes flatMap (getSpannedIndexes(_, degree - 1, dependencies))

        indexes ++ higherOrderIndexes
    }
  }

  def countPositiveCues(mention: Mention): Int = countCues(mention, strongLemmas)

  def countNegativeCues(mention: Mention): Int = countCues(mention, weakLemmas)

  def countCues(mention: Mention, hints:Set[String]): Int = {
    mention match {
      case event: EventMention =>

        // Get the dependencies of the sentence
        val dependencies = event.sentenceObj.dependencies.getOrElse(new DirectedGraph[String](Nil, Set[Int]()))

        val eventInterval: Seq[Int] = event.tokenInterval

        // Get the index of the word outside the event up to "degree" degrees
        val spannedIndexes: Seq[Int] = eventInterval flatMap (getSpannedIndexes(_, degree, dependencies))

        // Remove duplicates
        val indexes: Seq[Int] = (eventInterval ++ spannedIndexes).distinct

        // Get the lemmas
        val lemmas = indexes map (event.sentenceObj.lemmas.get(_))

        // Search for the hints
        (for {
          // Zip the lemma with its index, this is necessary to build the Modifictaion
          lemma <- lemmas
        } yield hints contains lemma).count(identity)

      case _ => 0
    }

  }

}
