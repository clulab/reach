package org.clulab.reach.context.ml

import java.io.File
import org.clulab.odin._
import org.clulab.reach._
import org.clulab.reach.darpa.{DarpaActions, MentionFilter, NegationHandler}
import org.clulab.reach.context.dataset.ArticleAnnotations

object Trainer extends App {
  // Trains a LM model out of the annotations and the reach mentions
  // First parameter: Corpus directory
  // Second parameter: output file

  val corpusDir = new File(args(0))
  val outputFile = new File(args(1))

  println("== Context ML model training ==")
  println
  println(s"Loading annotations from ${corpusDir.getPath} ...")

  // Load the annotations
  val documentAnnotations = corpusDir.listFiles.filter(_.isDirectory).map(d => ArticleAnnotations.readPaperAnnotations(d.getPath))

  // Loading reach system
  val reach = new ReachSystem
  val processor = reach.processor

  // Annotate a processors doc out of the sentences
  for(annotations <- documentAnnotations){
    println("Annotating a document ...")
    val sentences = annotations.sentences.values
    var doc = processor.mkDocumentFromSentences(sentences)
    doc = processor.annotate(doc)
    println("Extracting entities ...")
    val entities = reach.extractEntitiesFrom(doc)
    var events = reach.extractEventsFrom(doc, entities)
    events = MentionFilter.keepMostCompleteMentions(events, State(events))


  }
}
