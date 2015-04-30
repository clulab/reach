package edu.arizona.sista.bionlp.reach.rulelearning

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Sentence

class PathFinder(sentence: Sentence) {

  /** returns a dependency path between two nodes */
  def dependencyPaths(
    start: Int, end: Int,
    withWords: Boolean = false,
    withLemmas: Boolean = false,
    withTags: Boolean = false,
    withEntities: Boolean = false,
    withChunks: Boolean = false,
    withLastTokenConstraint: Boolean = true
  ): Seq[String] = {

    require(sentence.words.isDefinedAt(start), "`start` is not a valid token index")
    require(sentence.words.isDefinedAt(end), "`end` is not a valid token index")

    def mkEdgePaths(edges: Seq[Seq[(Int, Int, String)]]): Seq[Seq[(Int, Int, String)]] = edges match {
      case Nil => Seq(Nil)
      case Seq(first, rest @ _*) => for {
        i <- first
        j <- mkEdgePaths(rest)
      } yield i +: j
    }

    sentence.dependencies match {
      case None => Nil
      case Some(deps) =>
        // get sequence of nodes in the shortest path
        val nodesPath = deps.shortestPath(start, end, ignoreDirection = true)

        // make pairs of nodes in the shortest path
        val pairs = for (i <- 1 until nodesPath.size) yield (nodesPath(i - 1), nodesPath(i))

        // get edges for each pair
        val edges = for ((n1, n2) <- pairs) yield deps.getEdges(n1, n2, ignoreDirection = true)

        // get edges names and direction
        val paths = for (edgePath <- mkEdgePaths(edges)) yield {
          for (((n1, n2), edge) <- pairs zip edgePath) yield edge match {
            case (`n1`, `n2`, dep) =>
              val constraint = mkTokenConstraint(n2, withWords, withLemmas, withTags, withEntities, withChunks)
              if (constraint.isEmpty || n2 == end && !withLastTokenConstraint) s">$dep"
              else s">$dep ${constraint.get}"

            case (`n2`, `n1`, dep) =>
              val constraint = mkTokenConstraint(n2, withWords, withLemmas, withTags, withEntities, withChunks)
              if (constraint.isEmpty || n2 == end && !withLastTokenConstraint) s"<$dep"
              else s"<$dep ${constraint.get}"
          }
        }

        for (path <- paths) yield path.mkString(" ")
    }
  }

  /** escapes single quotes in the string and surrounds it with single quotes */
  def quote(s: String): String = s"'${s.replaceAllLiterally("'", "\\'")}'"

  /** returns a token constraint for a single token */
  def mkTokenConstraint(
    tok: Int,
    withWords: Boolean = true,
    withLemmas: Boolean = false,
    withTags: Boolean = false,
    withEntities: Boolean = false,
    withChunks: Boolean = false
  ): Option[String] = {
    val fields = Seq(
      if (withWords) Some(s"word=${quote(sentence.words(tok))}") else None,
      if (withLemmas) Some(s"lemma=${quote(sentence.lemmas.get(tok))}") else None,
      if (withTags) Some(s"tag=${quote(sentence.tags.get(tok))}") else None,
      if (withEntities) Some(s"entity=${quote(sentence.entities.get(tok))}") else None,
      if (withChunks) Some(s"chunk=${quote(sentence.chunks.get(tok))}") else None
    ).flatten
    if (fields.isEmpty) None else Some(s"[${fields.mkString(" & ")}]")
  }

  /** returns token constraints for all tokens in interval */
  def mkTokenConstraints(
    interval: Interval,
    withWords: Boolean = true,
    withLemmas: Boolean = false,
    withTags: Boolean = false,
    withEntities: Boolean = false,
    withChunks: Boolean = false
  ): Option[String] = {
    val constraints = interval.toSeq flatMap { i =>
      mkTokenConstraint(i, withWords, withLemmas, withTags, withEntities, withChunks)
    }
    if (constraints.isEmpty) None else Some(constraints.mkString(" "))
  }
}
