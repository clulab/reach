package edu.arizona.sista.reach.grounding

/**
  * Trait for species features used in external knowledge bases.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Add contains human species method.
  */
trait Speciated {

  /** Species name sets. */
  type SpeciesNameSet = Set[String]
  def  SpeciesNameSet(xs:String*): Set[String] = Set(xs: _*)

  /** A set of label strings for humans, found in KBs. */
  val HumanLabels:SpeciesNameSet = SpeciesNameSet("homo sapiens", "human")

  /** Tell whether the given sequence of species strings contain a human species label. */
  def containsHumanSpecies (speciesSeq: Seq[String]): Boolean =
    speciesSeq.exists(isHumanSpecies(_))

  /** Tell whether the given species string is label for humans or not. */
  def isHumanSpecies (species: String): Boolean =
    if (HumanLabels.contains(species.toLowerCase)) true else false

  /** Tell whether the given species string is a member of the given set of species. */
  def isMemberOf (species: String, speciesSet:SpeciesNameSet): Boolean =
    if (speciesSet.contains(species.toLowerCase)) true else false

}


/** Trait Companion Object allows Mixin OR Import pattern. */
object Speciated extends Speciated {

  /** Default value for human species. */
  val Human: String = "human"

}
