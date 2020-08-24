package org.clulab.reach.assembly.relations.classifier

import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly.sieves.SieveUtils
import org.clulab.learning.RVFDatum
import org.clulab.odin._
import org.clulab.processors.Sentence
import org.clulab.struct.{Counter, Interval}


object FeatureExtractor {

  val sep = ";;;"

  // limits for n-grams
  val minN = 1
  val maxN = 2

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
    *
    * @param e1 Event 1 (mention)
   * @param e2 Event 1 (mention)
   * @return
   */
  def mkFeatures(e1: Mention, e2: Mention): Seq[String] = {

    var features = Seq.empty[String]
    // get basic features for each event
    // NOTE: in order to find shared args b/w the two mentions, replaceEntitiesWithLabel needs e2
    val basicE1 = addFeaturePrefix("e1", mkBasicFeatures(e1, Seq(e2)))
    val basicE2 = addFeaturePrefix("e2", mkBasicFeatures(e2, Seq(e1)))
    features ++= (basicE1 ++ basicE2)

    // get coref features of ANAPHOR
    val corefE1 = getCorefFeatures(e1, Seq(e2))
    val corefE2 = getCorefFeatures(e2, Seq(e1))
    features ++= (corefE1 ++ corefE2)

    // get pair of labels
    features ++= getLabelPair(e1, e2)
    // get e-e surface features
    features ++= mkSurfaceFeatures(e1, e2)
    // get e-e syntax features
    features ++= mkSyntaxFeatures(e1, e2)
    features
  }

  def mkSurfaceFeatures(e1: Mention, e2: Mention): Seq[String] = {
    var features = Seq.empty[String]
    // add tokens in between the two mentions
    val intercedingTokens = tokensLinkingMentions(e1, e2)
    features ++= addFeaturePrefix("interceding-token ngrams", ngrams(intercedingTokens, minN, maxN))
    features ++= Seq(s"interceding-tokens-full-span: ${intercedingTokens.mkString(" ")}")
    features
  }

  def mkSyntaxFeatures(e1: Mention, e2: Mention): Seq[String] = {
    var features = Seq.empty[String]
    // add inter-sentence v. intra-sentence feature
    val sameSent = sameSentence(e1, e2)
    features ++= Seq(s"cross-sent:${! sameSent}")
    // check if events in same sentence
    sameSent match {
      case true => features ++= mkSameSentenceFeatures(e1, e2)
      case false => features ++= mkCrossSentenceFeatures(e1, e2)
    }
    features
  }

  /** create cross sentence features for two mentions */
  def mkCrossSentenceFeatures(e1: Mention, e2: Mention): Seq[String] = {
    require(e1.sentence != e2.sentence && e1.document == e2.document, "Mentions passed to mkCrossSentenceFeatures should come from different sentences!")
    var features = Seq.empty[String]
    val csps = getCrossSentenceSyntacticPaths(e1, e2)
    features ++= csps
    // TODO: what else?
    features
  }

  /** get features for two mentions in same sentence */
  def mkSameSentenceFeatures(e1: Mention, e2: Mention): Seq[String] = {
    require(e1.sentence == e2.sentence && e1.document == e2.document, "Mentions passed to mkSameSentenceFeatures should come from the same sentence!")
    var features = Seq.empty[String]
    // get verbs in path
    features ++= addFeaturePrefix("verb-in-path", getVerbsInPath(e1, e2))
    // get shortest paths
    features ++= addFeaturePrefix("shortest-path", getShortestPathVariants(e1, e2))
    // get shortest path distances
    features ++= getShortestPathDistances(e1, e2)
    // get paths from trigger to trigger
    features ++= addFeaturePrefix("trigger-to-trigger-path", getTriggerToTriggerPath(e1, e2))
    // get distances from trigger to trigger
    features ++= getTriggerToTriggerPathDistances(e1, e2)
    features
  }

  /**
   * Generate variations of trigger -> arg syntactic paths
    *
    * @param m
   * @return
   */
  def getTriggerArgPaths(m: Mention): Seq[String] = {
    val trigger = SieveUtils.findTrigger(m)
    val paths = for {
      // for each role
      (role, args) <- m.arguments
      // for each arg belonging to a role
      a <- args
      // with or without lemmas
      useLemmas <- Seq(true, false)
      p <- getShortestPaths(trigger, a, withLemmas = useLemmas)
      if p.nonEmpty
      // consider multiple trigger representations
      lemmatizedTrigger = s"lemmatizedTrigger=${trigger.lemmas.get.mkString(" ")}"
      t <- Seq(trigger.text, s"${trigger.text}:${m.label}", lemmatizedTrigger, s"$lemmatizedTrigger:${m.label}", s"${m.label}")
      // consider multiple arg representations
      argRepresentation <- Seq(a.label, role, s"$role:${a.label}")
      path = s"$t $p $argRepresentation"
    } yield path
    paths.toSeq.distinct
  }

  def getTriggerArgPathDistances(m: Mention): Seq[String] = {
    val trigger = SieveUtils.findTrigger(m)
    val distances = for {
    // for each role
      (role, args) <- m.arguments.toSeq
      // for each arg belonging to a role
      a <- args
      p <- getShortestPaths(trigger, a)
      if p.nonEmpty
      dist = p.split(" ").length
    } yield s"${role.toUpperCase}: $dist"
    distances.distinct
  }

  /**
   * Get paths from e1 to e2 <br>
   */
  def getShortestPathVariants(e1: Mention, e2: Mention): Seq[String] = {
    val paths = for {
      useLemmas <- Seq(true, false)
      p <- getShortestPaths(e1, e2, withLemmas = useLemmas)
      if p.nonEmpty
    } yield p
    paths.distinct
  }

  def getShortestPathDistances(e1: Mention, e2: Mention): Seq[String] = {
    val distances = for {
      p <- getShortestPaths(e1, e2)
      if p.nonEmpty
      dist = p.split(" ").length
    } yield s"sortest-path-dist: $dist"
    distances.distinct
  }
  /**
   * Get paths from trigger to trigger <br>
   * (ex. e1:phosphorylation:Phosphorylation rcmod e2:decreases:Positive_Regulation)
   */
  def getTriggerToTriggerPath(e1: Mention, e2: Mention): Seq[String] = {
    val t1 = SieveUtils.findTrigger(e1)
    val t2 = SieveUtils.findTrigger(e2)
    val paths = for {
      useLemmas <- Seq(true, false)
      p <- getShortestPaths(t1, t2, withLemmas = useLemmas)
      if p.nonEmpty
      t1Label = s"${t1.text}:${e1.label}"
      t2Label = s"${t2.text}:${e2.label}"
    } yield s"e1:$t1Label $p e2:$t2Label"
    paths.distinct
  }

  def getTriggerToTriggerPathDistances(e1: Mention, e2: Mention): Seq[String] = {
    val t1 = SieveUtils.findTrigger(e1)
    val t2 = SieveUtils.findTrigger(e2)
    val distances = for {
      p <- getShortestPaths(t1, t2)
      if p.nonEmpty
      dist = p.split(" ").length
    } yield s"trigger-dist: $dist"
    distances.distinct
  }

  /**
   * Features used to represent all mentions
    *
    * @param support a sequence of related mentions used to find shared arguments
   */
  def mkBasicFeatures(m: Mention, support: Seq[Mention] = Nil): Seq[String] = {
    // use resolved form
    val antecedent = AssemblyManager.getResolvedForm(m)
    mkCoreFeatures(antecedent, support)
  }

  def mkCoreFeatures(m: Mention, support: Seq[Mention] = Nil): Seq[String] = {
    val ents2Label = replaceEntitiesWithLabel(m, support)
    val args2Role = replaceArgsWithRole(m)
    var coreFeatures = Seq.empty[String]
    // syntactic features
    coreFeatures ++= getDependencyRelations(m)
    // get syntactic paths from trigger to each arg
    coreFeatures ++= addFeaturePrefix("trigger-arg-path", getTriggerArgPaths(m))
    // get distances from trigger to each arg
    coreFeatures ++= getTriggerArgPathDistances(m)
    // get event trigger
    coreFeatures ++= Seq(s"trigger: ${getTrigger(m)}")
    // most-specific label + all labels
    coreFeatures ++= getLabelFeatures(m)
    // get tokens in mention, but replace entities with their labels (MEK -> Protein)
    coreFeatures ++= addFeaturePrefix("ents2Label-mention-ngram", ngrams(ents2Label, minN, maxN))
    // use normalized mention span as single feature:
    // get tokens in mention, but replace args with their roles (ex. "Phosphorylation of KRAS" -> controlled)
    coreFeatures ++= addFeaturePrefix("args2Role-mention-ngram", ngrams(args2Role, minN, maxN))
    coreFeatures
  }

  def ngrams(toks: Seq[String], n: Int): Seq[String] = n match {
    case noNgrams if noNgrams > toks.size => Nil
    case _ =>
      for {
        i <- 0 to (toks.size - n)
        start = i
        end = i + n
        ngram = toks.slice(start, end).mkString(" ")
      } yield ngram
  }

  def ngrams(toks: Seq[String], startN: Int, stopN: Int): Seq[String] = {
    val grams = for {
      n <- startN to stopN
    } yield ngrams(toks, n)
    grams.flatten
  }

  /** get trigger for a mention */
  def getTrigger(m: Mention): String = {
    SieveUtils.findTrigger(m).text
  }

  /** get the words between two mentions */
  def tokensLinkingMentions(m1: Mention, m2: Mention): Seq[String] = {
    // TODO: replace entities with entity label (ex. Protein) or (SHARED)
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
    connectingSpan.flatten
  }

  /**
   * Replaces args in a mention with their role <br>
   * Array(the, Ras, protein, phosphorylates, Mek-32, at, 123) => <br>
   * Vector(CONTROLLER, protein, CONTROLLED)
   */
  def replaceArgsWithRole(m: Mention): Seq[String] = {
    // get final tokens of each arg
    val end2role: Map[Int, String] = {
      val pairs = for {
        (role, args) <- m.arguments
        a <- args
      } yield (a.end - 1, role)
      pairs
    }
    val toks = m.sentenceObj.words
    // get args
    val args = m.arguments.values.flatten
    // check each index in the sentence
    val processed = for {
      i <- 0 until m.sentenceObj.size
      // does the index overlap with the mention?
      if m.tokenInterval contains i
    } yield {
        i match {
          // if this is the final token in an arg, emit the arg's role
          case end if end2role contains end => end2role(end).toUpperCase
          // fall-through. emit the word if it isn't part of an argument
          case w if ! args.exists(_.tokenInterval.contains(i)) => toks(w)
          // emit nothing
          case _ => ""
        }
      }
    processed.filter(_.nonEmpty)
  }

  /** find all entities in a mention */
  def findEntities(m: Mention): Seq[Mention] = {
    val results = m match {
      // the mention isn't an entity
      case nonEntity if ! (nonEntity matches "Entity") => Nil
      // the mention is an entity
      case entity if entity.arguments.isEmpty && (entity matches "Entity") => Seq(entity)
      case other => other.arguments.values.flatten.flatMap(findEntities).toSeq
    }
    results ++ m.arguments.values.flatten.flatMap(findEntities)
  }

  /**
   * Replaces entities in a mention with their label <br>
   * Array(the, Ras, protein, phosphorylates, Mek-32, at, 123) => <br>
   * Vector(FAMILY, protein, phosphorylates, GENE_OR_GENE_PRODUCT)
    *
    * @param support a sequence of related mentions used to find shared arguments
   */
  def replaceEntitiesWithLabel(e1: Mention, support: Seq[Mention]): Seq[String] = {

    val SHARED = "SHARED"
    val entities = findEntities(e1)
    // get final tokens of each entity
    val end2entity: Map[Int, Mention] = {
      val pairs = for {
        entity <- findEntities(e1)
      } yield (entity.end - 1, entity)
      pairs.toMap
    }
    // find the entities for each Mention in support
    val supportEntities: Seq[Mention] =
      support.foldLeft(Seq.empty[Mention])((mns, e) => mns ++ findEntities(e))
    val supportManager = AssemblyManager(supportEntities)
    val toks = e1.sentenceObj.words
    // check each index in the sentence
    val processed = for {
      i <- 0 until e1.sentenceObj.size
      // does the index overlap with the mention?
      if e1.tokenInterval contains i
    } yield {
        i match {
          // if this is the final token in an entity, emit the entity's role
          case end if end2entity contains end =>
            // check if equiv to one of e2's entities
            val entity = end2entity(i)
            val eer = AssemblyManager(Seq(entity)).getEER(entity)
            val sharedArgs = supportManager.getEquivalentEERs(eer.equivalenceHash)
            if (sharedArgs.nonEmpty) SHARED else end2entity(end).label.toUpperCase
          // fall-through. emit the word if it isn't part of an entity
          case w if ! entities.exists(_.tokenInterval.contains(i)) => toks(w)
          // emit nothing
          case _ => ""
        }
      }
    processed.filter(_.nonEmpty)
  }

  def getDependencyRelations(m: Mention): Seq[String] = {
    val incomingDeps = for {
      incoming <- getIncomingDependencyRelations(m)
    } yield s"incoming: $incoming"
    val outgoingDeps = for {
      outgoing <-  getOutgoingDependencyRelations(m)
    } yield s"outgoing: $outgoing"
    incomingDeps ++ outgoingDeps
  }

  def getLabelFeatures(m: Mention): Seq[String] = {
    getMentionLabel(m) ++ getMentionLabels(m)
  }

  /** Generate a concatenation of the labels for a pair of mentions */
  def getLabelPair(m1: Mention, m2: Mention): Seq[String] = {
    Seq(s"e1-label: ${m1.label} + e2-label: ${m2.label}")
  }

  // TODO: add n-grams?
  def getShallowFeatures(m: Mention): Seq[String] = {
    getLemmaTagSequence(m) ++ m.words
  }

  // make coref features
  // TODO: coref feature on args (has resolution?, etc)
  def getCorefFeatures(m: Mention, support: Seq[Mention] = Nil): Seq[String] = {
    hasResolution(m) match {
      case false => Seq(s"resolved: false")
      case true => {
        // get basic features of antecedent
        var corefFeatures: Seq[String] = addFeaturePrefix("anaphor-basic", mkCoreFeatures(m, support))
        corefFeatures ++= locationOfAntecedent(m) ++ Seq(s"resolved: true")
        // check if each has a resolved form
        corefFeatures ++= checkArgumentsForResolutions(m)
        corefFeatures
      }
    }
  }

  // Check each arg role to see if it is resolved
  def checkArgumentsForResolutions(m: Mention): Seq[String] = {
    for {
      (role: String, args: Seq[Mention]) <- m.arguments.toSeq
      // does any of the mentions for this arg have a resolution?
      isResolved = args.exists(a => AssemblyManager.involvesCoreference(a))
    } yield s"${role.toUpperCase} hasResolution? $isResolved"
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

  /** find paths from sentence root to mention's trigger */
  def getRootPaths(m: Mention): Seq[String] = m.sentenceObj.dependencies match {
    // no dependencies
    case None => Nil
    // find root
    case Some(deps) => deps match {
      // rare, but apparently possible
      case noRoots if noRoots.roots.isEmpty => Nil
      // at least one root
      case hasRoots if deps.roots.nonEmpty =>

        val trigger = SieveUtils.findTrigger(m)

        val rootMentions = for {
          root <- hasRoots.roots
        } yield new TextBoundMention(
          label = "ROOT",
          tokenInterval = Interval(root, root + 1),
          sentence = m.sentence,
          document = m.document,
          keep = true,
          foundBy = "getRootPath"
        )
        // find paths connecting each root to the trigger
        for {
          rootMention <- rootMentions.toSeq
          p <- getShortestPaths(rootMention, trigger)
          path = (Seq("ROOT") :+ p).mkString(" ")
        } yield path
    }
  }

  /** get token indices of shortest path between two mentions */
  def getShortestPathTokenIndices(m1: Mention, m2: Mention): Seq[Int] = {
    val (start, end, dist) = findClosestConnectedTokens(m1, m2)
    val graph = m1.sentenceObj.dependencies.get
    graph.shortestPath(start, end, ignoreDirection = false)
  }

  /** find any verbs in shortest path between mentions */
  def getVerbsInPath(m1: Mention, m2: Mention): Seq[String] = {
    val s = m1.sentenceObj
    val tags = s.tags.get
    for {
      i <- getShortestPathTokenIndices(m1, m2)
      if tags(i).startsWith("V")
      verb = s.words(i)
    } yield verb
  }

  /** get concatenation of root paths for two mentions */
  def getCrossSentenceSyntacticPaths(m1: Mention, m2: Mention): Seq[String] = {
    for {
      cs1 <- getRootPaths(m1)
      cs2 <- getRootPaths(m2)
      csPath = s"CSSPATH: $cs1 ++ $cs2"
    } yield csPath
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
  def getShortestPaths(
    start: Int,
    end: Int,
    pathFinder: PathFinder,
    withWords: Boolean,
    withLemmas: Boolean,
    withTags: Boolean,
    withEntities: Boolean,
    withChunks: Boolean,
    withLastTokenConstraint: Boolean
  ): Seq[String] = {
    pathFinder.dependencyPaths(
      start,
      end,
      withWords,
      withLemmas,
      withTags,
      withEntities,
      withChunks,
      withLastTokenConstraint
    ).distinct
  }

  /** Get the shortest syntactic paths connecting two mentions. <br>
    * If the two mentions are in different docs and/or sentences, return Nil
    * */
  def getShortestPaths(
    src: Mention,
    dst: Mention,
    withWords: Boolean = false,
    withLemmas: Boolean = false,
    withTags: Boolean = false,
    withEntities: Boolean = false,
    withChunks: Boolean = false,
    withLastTokenConstraint: Boolean = true
  ): Seq[String] = {
    (src, dst) match {
      case (m1, m2) if sameSentence(m1, m2) =>
        val pathFinder = new PathFinder(m1.sentenceObj)
        val (startTok, endTok, _) = findClosestConnectedTokens(src, dst)
        // get shortest paths
        getShortestPaths(
          startTok,
          endTok,
          pathFinder,
          withWords,
          withLemmas,
          withTags,
          withEntities,
          withChunks,
          withLastTokenConstraint
        )
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
