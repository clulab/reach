package edu.arizona.sista.assembly


trait AssemblyModification

// for keeping track of post-translational modifications and any associated site
case class PTM(label: String, site: Option[String]) extends AssemblyModification {
  def this(label: String) = this(label, None)
}

// TODO: how to handle mutations at sites?
case class MutantEntity(mutantType: String) extends AssemblyModification

// Protein, GGP, etc
case class EntityLabel(label: String) extends AssemblyModification