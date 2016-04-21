package edu.arizona.sista.assembly.relations

import edu.arizona.sista.assembly.AssemblyManager
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
    // add tokens in between the two mentions
    basicFeatures = basicFeatures ++ tokensLinkingMentions(e1, e2)
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

  /** get the words between two mentions */
  def tokensLinkingMentions(m1: Mention, m2: Mention): Seq[String] = {
    val doc = m1.document
    val before = if (m1 precedes m2) m1 else m2
    val after = if (m1 precedes m2) m2 else m1
    val connectingSpan = for {
      i <- before.sentence to after.sentence
      s = doc.sentences(i)
    } yield {
        s match {
          // if both mentions are in this sentence, go from "before" end to "after" start
          case endToStart if before.sentence == i && after.sentence == i =>
            endToStart.words.slice(before.end, after.start)
          // "before" mention in this sentence, but not "after" mention
          case endToEnd if before.sentence == i && after.sentence != i =>
            // get words from end of "before" to EoS
            endToEnd.words.slice(before.end, s.words.length)
          // "after" mention in this sentence, but not "before" mention
          case startToStart if after.sentence == i && before.sentence != i =>
            startToStart.words.slice(0, after.start)
          // if neither mention is in the sentence, get full text
          case fullText if before.sentence != i && after.sentence != i =>
            fullText.words
        }
      }
    // add feature prefix
    addFeaturePrefix("interceding-tokens:", connectingSpan.flatten)
  }

  /**
   * Features used to represent all mentions
   */
  def mkBasicFeatures(m: Mention): Seq[String] = {
    // syntactic features
    getDependencyRelations(m) ++
      // number of args of each type
      getArgStats(m) ++
      // most-specific label + all labels
      getLabelFeatures(m)
      // start and end of mention
      terminalsConstraint(m) ++
      // sequence features
      getShallowFeatures(m)
      // get coref features
      getCorefFeatures(m)
  }

  def getDependencyRelations(m: Mention): Seq[String] = {
    getIncomingDependencyRelations(m) ++ getOutgoingDependencyRelations(m)
  }

  def getLabelFeatures(m: Mention): Seq[String] = {
    getMentionLabel(m) ++ getMentionLabels(m)
  }

  // TODO: add n-grams?
  def getShallowFeatures(m: Mention): Seq[String] = {
    getLemmaTagSequence(m) ++ m.words
  }

  // make coref features
  def getCorefFeatures(m: Mention): Seq[String] = {
    hasResolution(m) match {
      case false => Seq(s"resolved: false")
      case true => {
        val resolvedForm = AssemblyManager.getResolvedForm(m)
        var anteFeats = mkBasicFeatures(resolvedForm)
        anteFeats = addFeaturePrefix("antecedent-basic", anteFeats)
        anteFeats ++ locationOfAntecedent(m) ++ Seq(s"resolved: true")
      }
    }
  }

  // is the antecedent in a previous sentence, the same sentence, or a later sentence?
  def locationOfAntecedent(m: Mention): Seq[String] = {
    val ante = AssemblyManager.getResolvedForm(m)
    (m.sentence, ante.sentence) match {
      case noAnte if m == ante => Seq(s"ante: none")
      case same if same._1 == same._2 => Seq(s"ante: same-sent")
      case precedes if precedes._2 < precedes._1 => Seq(s"ante: prev")
      case follows if follows._2 > follows._1 => Seq(s"ante: follows")
    }
  }

  def hasResolution(m: Mention): Boolean = {
    val resolvedForm = AssemblyManager.getResolvedForm(m)
    resolvedForm match {
      case resolved if resolved != m => true
      case _ => false
    }
  }

  /** Check if two mentions are in the same doc and sentence */
  def sameSentence(e1: Mention, e2: Mention): Boolean = {
    (e1.document == e2.document) && (e1.sentence == e2.sentence)
  }

  def mkFeat(ss: String*): String = ss.mkString(sep)

  def parseFeat(f: String): Seq[String] = f.split(sep)

  def getArgStats(m: Mention): Seq[String] = {
    for {
      a <- m.arguments.keys.toSeq
    } yield s"arg-$a-count: ${m.arguments(a).size}"
  }

  def getMentionLabel(m: Mention): Seq[String] = {
    Seq(s"label: ${m.label}")
  }
  def getMentionLabels(m: Mention): Seq[String] = {
    m.labels.map(label => s"all-labels: $label")
  }

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

  def getOutgoingDependencyRelations(m: Mention): Seq[String] = {
    getOutgoingDependencyRelations(m.start, m.end, m.sentenceObj)
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

  def getIncomingDependencyRelations(m: Mention): Seq[String] = {
    getIncomingDependencyRelations(m.start, m.end, m.sentenceObj)
  }
}
