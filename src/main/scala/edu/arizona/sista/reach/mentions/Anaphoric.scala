package edu.arizona.sista.reach.mentions

import edu.arizona.sista.odin.Mention

trait Anaphoric {
  this: Mention =>

  var antecedents: Set[Anaphoric] = Set.empty
  var sieves: Set[String] = Set.empty

  def isGeneric: Boolean

  def text: String

  def number: Int

  def firstSpecific: Seq[Anaphoric] = {
    if (!this.isGeneric) Seq(this)
    else if (this.antecedents.isEmpty) Nil
    else {
      (for (ant <- antecedents) yield ant.asInstanceOf[Anaphoric].firstSpecific).flatten.toSeq
    }
  }

  def toSingletons: Seq[Anaphoric]

  def antecedent: Option[Anaphoric] = {
    val ant = if(this.isGeneric) {
      require(antecedents.size < 2,
        s"Multiple antecedents found for ${this.text}: ${this.antecedents.map(_.text).mkString(", ")}!")
      this.firstSpecific
    } else Nil
    ant.headOption
  }

  def isComplete: Boolean

  def isClosedClass: Boolean
}