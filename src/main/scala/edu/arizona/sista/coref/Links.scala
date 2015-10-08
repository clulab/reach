package edu.arizona.sista.coref

import edu.arizona.sista.reach.mentions.{CorefMention, Link}

/** Inherit from this class to implement your custom links.
  *
  * A link is a method of the form:
  * {{{
  * def customLink(orderedMentions: Seq[CorefMention]): Seq[CorefMention]
  * }}}
  */
class Links {
  /** The default action. Set to the identityLink. */
  val default: Link = identityLink

  def identityLink(orderedMentions: Seq[CorefMention]) = orderedMentions
}