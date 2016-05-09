package edu.arizona.sista.assembly

import edu.arizona.sista.odin.Mention


/**
 * Assembler for reach output
 * @param mentions a sequence of Odin-style Mentions
 */
class Assembler(mentions: Seq[Mention]) {

  val am = AssemblyRunner.applySieves(mentions)

  val causalPredecessors: Map[Mention, Set[Mention]] = {
    val links = for {
      m <- mentions
    } yield (m, am.distinctPredecessorsOf(m).flatMap(_.evidence))
    links.toMap
  }

  def getCausalPredecessors(m: Mention): Set[Mention] =
    causalPredecessors.getOrElse(m, Set.empty[Mention])

  val causalSuccessors: Map[Mention, Set[Mention]] = {
    val links = for {
      m <- mentions
    } yield (m, am.distinctSuccessorsOf(m).flatMap(_.evidence))
    links.toMap
  }

  def getCausalSuccessors(m: Mention): Set[Mention] =
    causalSuccessors.getOrElse(m, Set.empty[Mention])

  val equivalentMentions: Map[Mention, Set[Mention]] = {
    val links = for {
      m <- mentions
      eer = am.getEER(m)
      eers = am.getEquivalentEERs(eer.equivalenceHash)
      equivMentions = eers.flatMap(_.evidence)
    } yield (m, equivMentions)
    links.toMap
  }

  def getEquivalentMentions(m: Mention): Set[Mention] =
    equivalentMentions.getOrElse(m, Set.empty[Mention])
}
