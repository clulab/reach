package org.clulab.reach

/**
  * Convenience sets built from taxonomy
  */
object ReachConstants {

  val ACTIVATION_EVENTS = taxonomy.hyponymsFor("ActivationEvent").toSet
  val REGULATION_EVENTS = taxonomy.hyponymsFor("Regulation").toSet
  val COMPLEX_EVENTS = taxonomy.hyponymsFor("ComplexEvent").toSet

  val ADDITION_EVENTS = taxonomy.hyponymsFor("AdditionEvent").toSet
  val REMOVAL_EVENTS = taxonomy.hyponymsFor("RemovalEvent").toSet
  val MODIFICATION_EVENTS = ADDITION_EVENTS ++ REMOVAL_EVENTS
  val SIMPLE_EVENTS = taxonomy.hyponymsFor("SimpleEvent").toSet

}

