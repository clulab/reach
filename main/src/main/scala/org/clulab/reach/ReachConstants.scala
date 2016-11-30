package org.clulab.reach

/**
  * Convenience sets built from taxonomy
  *   Written by Gus Hahn-Powell. 7/16/2016.
  *   Last Modified: Add amount events set.
  */
object ReachConstants {

  val AMOUNT_EVENTS = taxonomy.hyponymsFor("Amount").toSet

  val ACTIVATION_EVENTS = taxonomy.hyponymsFor("ActivationEvent").toSet
  val REGULATION_EVENTS = taxonomy.hyponymsFor("Regulation").toSet
  val COMPLEX_EVENTS = taxonomy.hyponymsFor("ComplexEvent").toSet

  val ADDITION_EVENTS = taxonomy.hyponymsFor("AdditionEvent").toSet
  val REMOVAL_EVENTS = taxonomy.hyponymsFor("RemovalEvent").toSet
  val MODIFICATION_EVENTS = ADDITION_EVENTS ++ REMOVAL_EVENTS
  val SIMPLE_EVENTS = taxonomy.hyponymsFor("SimpleEvent").toSet

}

