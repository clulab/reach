package org.clulab.reach.assembly.representations

import org.clulab.reach.assembly.GroundingID


/**
 * Trait for modifications/features associated with an [[Entity]].
 */
trait AssemblyModification

/**
 * A representation of a post-translational modification (PTM) ( Phosphorylation, Ubiquitination, etc.) associated with an [[Entity]].
 *
 * @param label the ptm
 * @param site an Option[String] representing the Site of the PTM
 */
// for keeping track of post-translational modifications and any associated site
case class PTM(label: String, site: Option[String], negated: Boolean = false) extends AssemblyModification {
  def this(label: String) = this(label, None)
}

/**
 * A representation of a mutated form of an [[Entity]].
 * Mutation sites, if present, are encoded in [[mutantType]].
 *
 * @param mutantType the type of mutation (see [[org.clulab.reach.mentions.Mutant.label]])
 */
case class MutantEntity(mutantType: String) extends AssemblyModification

/**
 * The label associated with an [[Entity]] ( Protein, GGP, etc.)
 *
 * @param label the label associated with an [[Entity]]
 */
case class EntityLabel(label: String) extends AssemblyModification

/**
 * A location used in a Translocation
 *
 * @param groundedLoc the grounding ID associated with an [[Location]]
 */
case class Location(groundedLoc: GroundingID) extends AssemblyModification
