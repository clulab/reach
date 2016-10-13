package org.clulab.reach.assembly.representations

/**
 * Trait for entity representations of a Mention.
 */
trait Entity extends EntityEventRepresentation {
  /**
   * Intended to provide a high-level summary of the [[Entity]]
    *
    * @return a String summary of the [[Entity]]
   */
  def summarize: String

  override val eerString = "assembly.Entity"
}
