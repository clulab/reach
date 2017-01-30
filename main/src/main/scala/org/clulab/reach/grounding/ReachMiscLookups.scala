package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBConstants._

/**
  * Object implementing Reach specific reverse lookup tables.
  *   Written by: Tom Hicks. 3/10/2016
  *   Last Modified: Refactor protein kinase lookup set from here.
  */
object ReachMiscLookups {

  /** Single factory instance to generate Tsv RLKB classes. */
  val tsvRLKBFactory = new RLKBFactory

  /** Singleton grounding lookup table mapping species nsIds to species name strings. */
  val ReverseSpeciesLookup = tsvRLKBFactory.make("taxonomy", ContextSpeciesFilename)

}
