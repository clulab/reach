package edu.arizona.sista.reach.grounding

import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Object implementing Reach specific reverse lookup tables.
  *   Written by: Tom Hicks. 3/10/2016
  *   Last Modified: Initial creation.
  */
object ReachRLKBLookups {

  /** Single factory instance to generate Tsv RLKB classes. */
  val tsvRLKBFactory = new RLKBFactory

  /** Grounding lookup table mapping species nsIds to species name strings. */
  val ReverseSpeciesLookup = tsvRLKBFactory.make("taxonomy", ContextSpeciesFilename)

}
