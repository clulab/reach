package org.clulab.reach.darpa

import org.clulab.odin._
import org.clulab.reach.mentions._
import org.clulab.struct.{DirectedGraph, Interval}


object HypothesisHandler {

  def detectHypotheses(mentions: Seq[Mention], state:State): Seq[Mention] = {

    val degree = 2 // Degree up to which we should follow the links in the graph

    // These are the words that hint a hypothesis going on
    val hints = Set(
      "argue",
      "argument",
      "believe",
      "belief",
      "conjecture",
      "consider",
      "hint",
      "hypothesis",
      "hypotheses",
      "hypothesize",
      "implication",
      "implicate",
      "imply",
      "indicate",
      "predict",
      "prediction",
      "previous",
      "previously",
      "proposal",
      "propose",
      "question",
      "speculate",
      "speculation",
      "suggest",
      "suspect",
      "theorize",
      "theory",
      "think",
      "whether")

    // Recursive function that helps us get the words outside the event
    def getSpannedIndexes(index:Int, degree:Int, dependencies:DirectedGraph[String]):Seq[Int] = {
      degree match {
        case 0 => Seq[Int]() // Base case of the recursion
        case _ =>

          val outgoing = dependencies.outgoingEdges
          val incoming = dependencies.incomingEdges

          // Get incoming and outgoing edges
          val t:Seq[(Int, String)] = incoming.lift(index)  match {
            case Some(x) => x
            case None => Seq()
          }

          val edges = t ++ (outgoing.lift(index) match {
            case Some(x) => x
            case None => Seq()
          })


          // Each edge is a tuple of (endpoint index, edge label), so we map it to the first
          // element of the tuple
          val indexes:Seq[Int] = edges map (_._1)

          // Recursively call this function to get outter degrees
          val higherOrderIndexes:Seq[Int] = indexes flatMap (getSpannedIndexes(_, degree - 1, dependencies))

          indexes ++ higherOrderIndexes
      }
    }

    mentions foreach {
      case event:BioEventMention =>

        // Get the dependencies of the sentence
        val dependencies = event.sentenceObj.dependencies.getOrElse(new DirectedGraph[String](Nil, Set[Int]()))

        val eventInterval:Seq[Int] = event.tokenInterval

        // Get the index of the word outside the event up to "degree" degrees
        val spannedIndexes:Seq[Int] = eventInterval flatMap (getSpannedIndexes(_, degree, dependencies))

        // Remove duplicates
        val indexes:Seq[Int] = (eventInterval ++ spannedIndexes).distinct

        // Get the lemmas
        val lemmas = indexes map (event.sentenceObj.lemmas.get(_))

        // Perform assignments
        for {
          // Zip the lemma with its index, this is necessary to build the Modifictaion
          (lemma, ix) <- lemmas zip indexes
          // Only if the lemma is part of one of the hints
          if hints contains lemma
        }{
          event.modifications += Hypothesis(new BioTextBoundMention(
            Seq("Hypothesis_hint"),
            Interval(ix),
            sentence = event.sentence,
            document = event.document,
            keep = event.keep,
            foundBy = event.foundBy
          ))
        }

      case _ => ()
    }
    mentions
  }

}
