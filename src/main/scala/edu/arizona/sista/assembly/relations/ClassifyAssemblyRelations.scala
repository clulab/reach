package edu.arizona.sista.assembly.relations

import com.typesafe.config.ConfigFactory

object ClassifyAssemblyRelations {

  import CorpusReader._

  val config = ConfigFactory.load()

  val annotations: Seq[PrecedenceAnnotation] = CorpusReader.annotationsFromFile(config.getString("assembly.annotations"))

  // gather precedence relations corpus
  val precedenceAnnotations = filterRelations(annotations, precedenceRelations)

  // gather subsumption relations corpus
  val subsumptionAnnotations = filterRelations(annotations, subsumptionRelations)

  // gather equivalence relations corpus
  val equivalenceAnnotations = filterRelations(annotations, subsumptionRelations)
}
