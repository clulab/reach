package org.clulab.reach.assembly.sieves

import org.clulab.odin.{Actions, Mention, RelationMention, State}
import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.taxonomy


class AssemblyActions extends Actions {

  val BEFORE = "before"
  val AFTER = "after"
  val PRECEDENCE = "Precedence"

  def identityAction(mentions: Seq[Mention], state: State): Seq[Mention] = mentions

  def validatePrecedenceRelations(mentions: Seq[Mention], state: State): Seq[Mention] = {

    val am = AssemblyManager(mentions)

    for {
      m <- mentions
      if m.arguments contains BEFORE
      if m.arguments contains AFTER
      b <- m.arguments(BEFORE)
      a <- m.arguments(AFTER)
      if Constraints.shareArg(b, a)
      if Constraints.isValidRelationPair(b, a)
    } yield new RelationMention(
      labels = taxonomy.hypernymsFor(PRECEDENCE),
      arguments = Map("before" -> Seq(b), "after" -> Seq(a)),
      sentence = m.sentence,
      document = m.document,
      keep = m.keep,
      foundBy = m.foundBy
    )
  }

  def shareControlleds(mentions: Seq[Mention], state: State): Seq[Mention] = for {
    m <- validatePrecedenceRelations(mentions, state)
    b <- m.arguments(BEFORE)
    a <- m.arguments(AFTER)
    if Constraints.shareControlleds(b, a)
  } yield m

}
