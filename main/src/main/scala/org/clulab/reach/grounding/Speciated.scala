package org.clulab.reach.grounding

import org.clulab.reach.grounding.Speciated._

/**
  * Trait for species features used in external knowledge bases.
  *   Written by Tom Hicks. 10/23/2015.
  *   Last Modified: Refactor for consistent selfless traits and extension vs imports.
  */
trait Speciated {

  /** Define species name set type. */
  type SpeciesNameSet = Set[String]

  /** Convenience function to create a species name set from string arguments. */
  def  SpeciesNameSet (xs:String*): Set[String] = Set(xs: _*)


  /** Tell whether the given sequence of namespace/ID strings contains a human nsId. */
  def containsHumanNsId (nsIdSeq: Seq[String]): Boolean = nsIdSeq.exists(hasHumanNsId(_))

  /** Tell whether the given sequence of species strings contain a human species label. */
  def containsHumanSpecies (speciesSeq: Seq[String]): Boolean = speciesSeq.exists(isHumanSpecies(_))

  /** Tell whether the given namespace/ID string is an ID for humans or not. */
  def hasHumanNsId (nsId: String): Boolean = HumanNsId == nsId

  /** Tell whether the given species string is label for humans or not. */
  def isHumanSpecies (species: String): Boolean =
    if (HumanLabels.contains(species.toLowerCase)) true else false

  /** Tell whether the given species string is a member of the given set of species. */
  def isMemberOf (species: String, speciesSet:SpeciesNameSet): Boolean =
    if (speciesSet.contains(species.toLowerCase)) true else false

}


/** Trait Companion Object allows Mixin OR Import pattern. */
object Speciated extends Speciated {

  /** Default value for human species string. */
  val Human: String = "human"

  /** A set of label strings for humans, found in KBs. */
  val HumanLabels:SpeciesNameSet = SpeciesNameSet("homo sapiens", "human")

  /** Default value for human species namespace:ID string (NCBI Taxonomy). */
  val HumanNsId: String = "taxonomy:9606"

}
