package edu.arizona.sista.assembly

import edu.arizona.sista.odin.Mention


/**
 * Assembler for reach output
 * @param mentions a sequence of Odin-style Mentions
 */
class Assembler(val mentions: Seq[Mention]) {

  val am = AssemblyRunner.applySieves(mentions)
  println("finished assembly!")

  val causalPredecessors: Map[Mention, Set[Mention]] = {
    val links = for {
      m <- mentions
    } yield (m, am.distinctPredecessorsOf(m).flatMap(_.evidence))
    links.toMap
  }
  println("Built causalPredecedessors map")

  def getCausalPredecessors(m: Mention): Set[Mention] =
    causalPredecessors.getOrElse(m, Set.empty[Mention])

  val causalSuccessors: Map[Mention, Set[Mention]] = {
    val links = for {
      m <- mentions
    } yield (m, am.distinctSuccessorsOf(m).flatMap(_.evidence))
    links.toMap
  }
  println("Built causalSuccessors map")

  def getCausalSuccessors(m: Mention): Set[Mention] =
    causalSuccessors.getOrElse(m, Set.empty[Mention])

  val equivalentMentions: Map[Mention, Set[Mention]] = {
    val links = for {
      m <- mentions
      if AssemblyManager.isValidMention(m)
      eer = am.getEER(m)
      equivMentions = am.getEvidence(eer)
      // remove m from equiv.
    } yield (m, equivMentions - m)
    links.toMap
  }
  println("Built equivalentMentions map")

  def getEquivalentMentions(m: Mention): Set[Mention] =
    equivalentMentions.getOrElse(m, Set.empty[Mention])
}
