package edu.arizona.sista.reach.mentions

import edu.arizona.sista.odin.Mention

trait Anaphoric {
  this: Mention =>

  var antecedents: Set[Anaphoric] = Set.empty
  var sieves: Set[String] = Set.empty

  def isGeneric: Boolean

  def hasGenericMutation: Boolean

  def text: String

  def number: Int

  def firstSpecific: Seq[Anaphoric] = {
    if (!this.isGeneric && !this.hasGenericMutation) Seq(this)
    else if (this.antecedents.isEmpty) Nil
    else {
      (for (ant <- antecedents) yield ant.asInstanceOf[Anaphoric].firstSpecific).flatten.toSeq
    }
  }

  def toSingletons: Seq[Anaphoric]

  def antecedent: Option[Anaphoric] = {
    val ant = if(this.isGeneric || this.hasGenericMutation) {
      // FIXME: this guy fails on PMC3178447
      //require(antecedents.size < 2,
      //  s"Multiple antecedents found for ${this.text}: ${this.antecedents.map(_.text).mkString(", ")}!")
      this.firstSpecific
    } else Nil
    ant.headOption
  }

  def isComplete: Boolean

  def isClosedClass: Boolean

  def isGenericNounPhrase: Boolean

  // generic antecedent matching with number approximation
  val detMap = Map("a" -> 1,
    "an" -> 1,
    "both" -> 2,
    "each" -> 2,
    "that" -> 1,
    "those" -> 2,
    "these" -> 2, // assume two for now...
    "this" -> 1,
    "few" -> 3,
    "some" -> 3, // assume three for now...
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "ten" -> 10,
    "its" -> 1,
    "their" -> 2)

  val headMap = Map("it" -> 1,
    "they" -> 2,
    "theirs" -> 1,
    "them" -> 2,
    "that" -> 1,
    "both" -> 2,
    "each" -> 2,
    "those" -> 2,
    "these" -> 2, // assume two for now...
    "this" -> 1,
    "some" -> 3, // assume three for now...
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "ten" -> 10
  )

  /**
    * Return the cardinality of a phrase based on its determiners
    */
  def detCardinality(words: Seq[String], tags: Seq[String]): Int = {
    require(words.length == tags.length, s"Mention '${words.mkString(" ")}' has different number of tags and words!")
    val somenum = words(tags
      .zipWithIndex
      .find(x => x._1 == "CD")
      .getOrElse(tags
        .zipWithIndex
        .find(x => Seq("DT", "PRP$")
          .contains(x._1))
        .getOrElse(return 0))
      ._2)
    def finalAttempt(num: String): Int = try {
      num.toInt
    } catch {
      case e: NumberFormatException => 0
    }
    detMap.getOrElse(somenum, finalAttempt(somenum))
  }

  /**
    * Return the cardinality of a phrase based on its head -- is it plural?
    */
  def headCardinality(somenum: String, tag: String): Int = {
    tag match {
      case "PRP" | "PRP$" => headMap.getOrElse(somenum, 0)
      case "NNS" | "NNPS" => 2
      case "NN" | "NNP" => 1
      case _ => headMap.getOrElse(somenum, 1)
    }
  }

}