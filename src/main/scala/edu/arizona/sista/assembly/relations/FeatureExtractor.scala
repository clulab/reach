package edu.arizona.sista.assembly.relations

import edu.arizona.sista.learning.RVFDatum
import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.struct.Counter


object FeatureExtractor {

  val sep = ";;;"


  def mkRVFDatum(e1: Mention, e2: Mention, label: String): RVFDatum[String, String] = {
    val df = new Counter[String]()
    val features = mkFeatures(e1, e2)
    // get counts of each feature
    features.foreach(f => df.incrementCount(f))

    new RVFDatum[String, String](label, df)
  }

  def addFeaturePrefix(prefix: String, features: Seq[String]): Seq[String] = {
    for (f <- features) yield s"$prefix: $f"
  }

  /**
   * Takes Event 1 and Event 2 and produces features
   * @param e1 Event 1 (mention)
   * @param e2 Event 1 (mention)
   * @return
   */
  def mkFeatures(e1: Mention, e2: Mention): Seq[String] = {
    val sameSent = sameSentence(e1, e2)
    // get basic features for each event
    val basicE1 = mkBasicFeatures(e1)
    val basicE2 = mkBasicFeatures(e2)
    // add Daume-y domain adaptation prefix
    val adaptedE1 = addFeaturePrefix("e1", basicE1)
    val adaptedE2 = addFeaturePrefix("e2", basicE2)
    var basicFeatures: Seq[String] = basicE1 ++ basicE2 ++ adaptedE1 ++ adaptedE2
    // add inter-sentence v. intra sentence feature
    basicFeatures = basicFeatures ++ Seq(s"cross-sent:${! sameSent}")
    // check if events in same sentence
    val features: Seq[String] = sameSent match {
      case true =>
        val pathFinder = new PathFinder(e1.sentenceObj)
        val (startTok, endTok, _) = findClosestConnectedTokens(e1, e2)
        // make sure a path was found
        startTok match {
          case valid if valid > -1 =>
            val paths = getShortestPaths(startTok, endTok, pathFinder)
            // add event-specific info to paths, if needed
            val ruleFeats = paths.map(path => mkFeat(path, e1.label, e2.label))
            basicFeatures ++ addFeaturePrefix("path", ruleFeats)
          case other => basicFeatures
        }
      case false => Nil
    }

    features
  }

  def mkBasicFeatures(m: Mention): Seq[String] = {
  Nil
  }

  /** Check if two mentions are in the same doc and sentence */
  def sameSentence(e1: Mention, e2: Mention): Boolean = {
    (e1.document == e2.document) && (e1.sentence == e2.sentence)
  }
  def mkFeat(ss: String*): String = ss.mkString(sep)

  def parseFeat(f: String): Seq[String] = f.split(sep)

  /** Find the two tokens that anchor the shortest syntactic path between two mentions */
  def findClosestConnectedTokens(src: Mention, dst: Mention): (Int, Int, Int) = {
    val graph = src.sentenceObj.dependencies.get
    val connectedTokens = for {
      start <- src.tokenInterval
      end <- dst.tokenInterval
      path = graph.shortestPath(start, end, ignoreDirection = false)
      // dist is number of hops, not tokens
      dist = path.length - 1
    } yield (start, end, dist)

    connectedTokens.minBy(_._3)
  }

  /**
   * Get shortest syntactic paths between two tokens
   */
  def getShortestPaths(start: Int, end: Int, pathFinder: PathFinder): Seq[String] = {
    pathFinder.dependencyPaths(start, end).distinct
  }

  /** Get the shortest syntactic paths connecting two mentions. <br>
    * If the two mentions are in different docs and/or sentences, return Nil
    * */
  def getShortestPaths(src: Mention, dst: Mention): Seq[String] = {
    (src, dst) match {
      case (m1, m2) if sameSentence(m1, m2) =>
        val pathFinder = new PathFinder(m1.sentenceObj)
        val (startTok, endTok, dist) = findClosestConnectedTokens(src, dst)
        val pathDist = s"path_length=$dist"
        // get shortest paths and path length
        Seq(pathDist) ++ getShortestPaths(startTok, endTok, pathFinder)
      case other => Nil
    }
  }

  /** get the tokens on either end of the mention */
  def terminalsConstraint(m: Mention): Seq[String] = {
    val pathFinder = new PathFinder(m.sentenceObj)
    val startConstraint = pathFinder.mkTokenConstraint(m.start, withWords=false, withLemmas=true, withTags=true)
    val endConstraint = pathFinder.mkTokenConstraint(m.end, withWords=false, withLemmas=true, withTags=true)
    val terminals = s"start:$startConstraint&end:$endConstraint"
    Seq(terminals)
  }

  /** get the lemma tag sequence for a Mention */
  def getLemmaTagSequence(m: Mention): Seq[String] = {
    val pathFinder = new PathFinder(m.sentenceObj)
    for {
      i <- m.start to m.end
      constraint = pathFinder.mkTokenConstraint(i, withWords=false, withLemmas=true, withTags=true).get
    } yield constraint
  }

  /** get the lemma tag sequence for a Sentence */
  def getLemmaTagSequence(s: Sentence): Seq[String] = {
    val pathFinder = new PathFinder(s)
    for {
      i <- 0 until s.size
      constraint = pathFinder.mkTokenConstraint(i, withWords=false, withLemmas=true, withTags=true).get
    } yield constraint
  }

  /** get outgoing dependency relations for the given span */
  def getOutgoingDependencyRelations(start: Int, end: Int, s: Sentence): Seq[String] = {
    val pathFinder = new PathFinder(s)
    val depFeats: Seq[Seq[String]] = for {
      i <- start until end
      outgoing: Seq[String] = pathFinder.sentence.dependencies.get.getEdges(i, i + 1).map(tup => s">${tup._3}")
    } yield outgoing

    depFeats.flatten
  }

  /** get incoming dependency relations for the given span */
  def getIncomingDependencyRelations(start: Int, end: Int, s: Sentence): Seq[String] = {
    val pathFinder = new PathFinder(s)
    val depFeats: Seq[Seq[String]] = for {
      i <- start until end
      incoming: Seq[String] = pathFinder.sentence.dependencies.get.getEdges(i + 1, i).map(tup => s"<${tup._3}")
    } yield incoming

    depFeats.flatten
  }

}
