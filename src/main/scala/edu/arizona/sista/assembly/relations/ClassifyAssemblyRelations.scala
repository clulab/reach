package edu.arizona.sista.assembly.relations

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.learning.Datasets._

object ClassifyAssemblyRelations {

  import CorpusReader._

  val config = ConfigFactory.load()

  val annotations: Seq[PrecedenceAnnotation] = CorpusReader.annotationsFromFile(config.getString("assembly.annotations"))

  // gather precedence relations corpus
  val precedenceAnnotations = filterRelations(annotations, precedenceRelations)
  val precedenceDataset = AssemblyRelationClassifier.mkRVFDataset(precedenceAnnotations)

  // gather subsumption relations corpus
  val subsumptionAnnotations = filterRelations(annotations, subsumptionRelations)
  val subsumptionDataset = AssemblyRelationClassifier.mkRVFDataset(subsumptionAnnotations)

  // gather equivalence relations corpus
  val equivalenceAnnotations = filterRelations(annotations, subsumptionRelations)
  val equivalenceDataset = AssemblyRelationClassifier.mkRVFDataset(equivalenceAnnotations)

  
}
