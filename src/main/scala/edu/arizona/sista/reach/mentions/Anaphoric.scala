package edu.arizona.sista.reach.mentions

import edu.arizona.sista.odin.Mention

trait Anaphoric {
  this: Mention =>

  var antecedents: Set[Mention] = Set.empty
  var sieves: Set[String] = Set.empty

  def isGeneric: Boolean

  def number: Int

  def firstSpecific: Seq[Anaphoric] = {
    if (!this.isGeneric) Seq(this)
    else if (this.antecedents.isEmpty) Nil
    else {
      (for (ant <- antecedents) yield ant.asInstanceOf[Anaphoric].firstSpecific).flatten.toSeq
    }
  }

}