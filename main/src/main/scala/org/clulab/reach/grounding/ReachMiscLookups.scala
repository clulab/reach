package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBConstants._

/**
  * Object implementing Reach specific reverse lookup tables.
  *   Written by: Tom Hicks. 3/10/2016
  *   Last Modified: Refactor for consistent selfless traits and extension vs imports.
  */
object ReachMiscLookups {

  /** Single factory instance to generate Tsv RLKB classes. */
  val tsvRLKBFactory = new RLKBFactory

  /** Singleton grounding lookup table mapping species nsIds to species name strings. */
  val ReverseSpeciesLookup = tsvRLKBFactory.make("taxonomy", ContextSpeciesFilename)

  /** Set of short protein domain strings. */
  val ProteinKinaseIds: Set[String] = ReachKBUtils.readLines(ProteinKinasesFilename).toSet

  /** Tell whether the given ID string names a protein kinase or not. */
  def isProteinKinase (id: String): Boolean = ProteinKinaseIds.contains(id)

}
