package edu.arizona.sista.reach

/**
  * Object defining constants used across Reach.
  * This is not a package object because its constants are not needed by all Reach classes.
  *   Written by Tom Hicks. 4/14/2016.
  *   Last Modified: Initial creation with event taxonomy sets.
  */
object ReachConstants {

  val ACTIVATION_EVENTS = Set(
    "Negative_activation",
    "Positive_activation"
  )

  val REGULATION_EVENTS = Set(
    "Negative_regulation",
    "Positive_regulation"
  )

  val COMPLEX_EVENTS =  ACTIVATION_EVENTS ++ REGULATION_EVENTS

  val ADDITION_EVENTS = Set(
    "Acetylation",
    "Farnesylation",
    "Glycosylation",
    "Hydrolysis",
    "Hydroxylation",
    "Methylation",
    "Phosphorylation",
    "Ribosylation",
    "Sumoylation",
    "Ubiquitination"
  )

  val REMOVAL_EVENTS = Set(
    "Deacetylation",
    "Defarnesylation",
    "Deglycosylation",
    "Dehydrolysis",
    "Dehydroxylation",
    "Demethylation",
    "Dephosphorylation",
    "Deribosylation",
    "Desumoylation",
    "Deubiquitination"
  )

  val MODIFICATION_EVENTS = ADDITION_EVENTS ++ REMOVAL_EVENTS

  val SIMPLE_EVENTS = MODIFICATION_EVENTS ++ Set(
    "Binding",
    "Generic_event",
    "Translocation"
  )

}
