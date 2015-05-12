package edu.arizona.sista.bionlp.reach.rulelearning

import scala.math.max
import scala.collection.mutable

class RegexTrie(val token: Option[String] = None) {

  // look up table (token -> node)
  private val children = new mutable.HashMap[String, RegexTrie]

  // true if this node ends at least one of the seen patterns
  private var endOfPattern: Boolean = false

  // the root doesn't have a token, every other node does
  private def isRoot: Boolean = token.isEmpty

  def insert(pattern: Seq[String]): Unit =
    if (pattern.nonEmpty) {
      // next token to insert
      val tok = pattern.head
      // get or create next child node
      val node = children.getOrElseUpdate(tok, new RegexTrie(Some(tok)))
      // insert the rest of the pattern on the child node
      node.insert(pattern.tail)
    } else {
      // this node ends a pattern
      endOfPattern = true
    }

  def maxDepth: Int = {
    // the root node doesn't count
    val thisDepth = if (isRoot) 0 else 1
    val maxChildDepth = children.values.map(_.maxDepth).foldLeft(0)(max)
    thisDepth + maxChildDepth
  }

  def toTokenPattern: String = {
    // get all child nodes in descending order by max depth
    val childNodes = children.values.toSeq.sortBy(-_.maxDepth)
    // get token pattern for children
    val childPattern =
      if (childNodes.isEmpty) ""
      else if (childNodes.size == 1) childNodes.head.toTokenPattern
      else "(" + childNodes.map(_.toTokenPattern).mkString(" | ") + ")"
    // return pattern for current node
    if (isRoot) childPattern
    else if (childPattern.isEmpty) token.get
    else if (endOfPattern) s"${token.get} ($childPattern)?"
    else s"${token.get} $childPattern"
  }

}
