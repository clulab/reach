package edu.arizona.sista.reach

import edu.arizona.sista.reach.mentions._

package object context {

  /** Type alias for the context map which maps context types to a sequence of values. */
  type ContextMap = Map[String, Seq[String]]

  /** Tell whether the given mention has a context map containing species info or not. */
  def hasSpeciesContext (mention:BioMention): Boolean =
    mention.context.exists(_.contains("Species"))

}
