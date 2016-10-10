package org.clulab.reach.assembly.sieves

import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.representations.EntityEventRepresentation
import org.clulab.reach.assembly._
import org.clulab.odin.Mention


trait AssemblySieve {
  /** Gets the mentions from reading and returns a new AssemblyManager instance
    * with updated information on deduplication and causal ordering.
    *
    * @param mentions the mentions received by the reading component
    * @param manager handles mapping of Mention -> [[EntityEventRepresentation]]
    * @return current AssemblySieve's AssemblyManger instance, which stores information on deduplication and causal ordering
    */
  def apply(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager
  def apply(mentions: Seq[Mention]): AssemblyManager

  /** Composes two instances of AssemblySieve into a single AssemblySieve */
  def andThen(that: AssemblySieve): AssemblySieve = new SieveMixture(this, that)
}

object AssemblySieve {
  def apply(sieve: Sieve): AssemblySieve = new AssemblySieve {

    /** Used when no AssemblyManager is provided **/
    def apply(mentions: Seq[Mention]): AssemblyManager = sieve(mentions, AssemblyManager())

    /** Matches the signature of Sieve **/
    def apply(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager =
      sieve(mentions, manager)
  }
}

class SieveMixture(step1: AssemblySieve, step2: AssemblySieve) extends AssemblySieve {
  def apply(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager =
    step2(mentions, step1(mentions, manager))

  /** Used when no AssemblyManager is provided **/
  def apply(mentions: Seq[Mention]): AssemblyManager = {
    val manager = AssemblyManager()
    step2(mentions, step1(mentions, manager))
  }
}

