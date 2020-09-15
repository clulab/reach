package org.clulab.reach.assembly.sieves

import com.typesafe.scalalogging.LazyLogging
import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.{AssemblyManager, EER}
import org.clulab.reach.assembly.relations.classifier.AssemblyRelationClassifier
import org.clulab.reach.RuleReader
import org.clulab.odin._
//import org.chocosolver.solver.Solver
//import org.chocosolver.solver.cstrs.GraphConstraintFactory
//import org.chocosolver.solver.variables.GraphVarFactory
//import org.chocosolver.util.objects.graphs.DirectedGraph
//import org.chocosolver.util.objects.setDataStructures.SetType
import scala.annotation.tailrec


class Sieves extends LazyLogging {


}

/**
  * Contains all deduplication sieves of the signature (mentions: Seq[Mention], manager: AssemblyManager) => AssemblyManager.
  */
class DeduplicationSieves extends Sieves {
  /**
    * Populates an AssemblyManager with mentions (default behavior of AssemblyManager)
    *
    * @param mentions a sequence of Odin Mentions
    * @param manager  an AssemblyManager
    * @return an AssemblyManager
    */
  def trackMentions(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {
    logger.debug(s"\tapplying 'trackMentions' sieve...")
    val am = AssemblyManager()
    am.trackMentions(mentions)
    am
  }

  // TODO: add approximate deduplication sieve
  def approximateDeduplication(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = ???
}

/**
  * Contains all precedence sieves of the signature (mentions: Seq[Mention], manager: AssemblyManager) => AssemblyManager.
  */
class PrecedenceSieves extends Sieves {

  import Constraints._
  import SieveUtils._


  private def applyPrecedenceRules(
    mentions: Seq[Mention],
    manager: AssemblyManager,
    sieveName: String,
    ruleFile: String,
    actions: Actions = new Actions
  ): AssemblyManager = {

    // find rule-based PrecedenceRelations
    for {
      rel <- assemblyViaRules(ruleFile, mentions, actions)
      // ignore discourse markers
      if rel.label == precedenceMentionLabel
      before = rel.arguments.getOrElse(SieveUtils.beforeRole, Nil)
      after = rel.arguments.getOrElse(SieveUtils.afterRole, Nil)
      if before.nonEmpty && after.nonEmpty
      // both "before" and "after" should have single mentions
      b = before.head
      a = after.head
      // cannot be an existing regulation
      //if notAnExistingComplexEvent(rel)
      //if noExistingPrecedence(a, b, manager
    } {
//      if (rel.foundBy == "cross-sentence-next-step") {
//        logger.info(s"cross-sentence-next-step found a precedence relation mention")
//      }
      // store the precedence relation
      // TODO: add score
      manager.storePrecedenceRelation(b, a, Set(rel), sieveName)
    }

    manager
  }

  /**
    * Rule-based method for detecting precedence relations within sentences
    *
    * @param mentions a sequence of Odin Mentions
    * @param manager  an AssemblyManager
    * @return an AssemblyManager
    */
  def intrasententialRBPrecedence(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    val name = "intrasententialRBPrecedence"

    logger.debug(s"\tapplying '$name' sieve...")

    val actions = new AssemblyActions
    val ruleFile = "/org/clulab/reach/assembly/grammars/intrasentential.yml"

    val am2 = applyPrecedenceRules(mentions, manager, name, ruleFile, actions)

    am2
  }

  /**
    * Rule-based method for detecting inter-sentence precedence relations
    *
    * @param mentions a sequence of Odin Mentions
    * @param manager  an AssemblyManager
    * @return an AssemblyManager
    */
  def intersententialRBPrecedence(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    val name = "intersententialRBPrecedence"

    logger.debug(s"\tapplying '$name' sieve...")

    val actions = new AssemblyActions
    val ruleFile = "/org/clulab/reach/assembly/grammars/intersentential.yml"

    val am2 = applyPrecedenceRules(mentions, manager, name, ruleFile, actions)

    am2
  }

  /**
    * Patterns derived from teh BioDRB corpus
    *
    * @param mentions a sequence of Odin Mentions
    * @param manager  an AssemblyManager
    * @return an AssemblyManager
    */
  def bioDRBpatterns(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    val name = "bioDRBpatterns"

    logger.debug(s"\tapplying '$name' sieve...")

    val actions = new AssemblyActions
    val ruleFile = "/org/clulab/reach/assembly/grammars/biodrb-patterns.yml"

    val am2 = applyPrecedenceRules(mentions, manager, name, ruleFile, actions)

    am2
  }

  /**
    * Rule-based method using discourse features to establish precedence
    *
    * @param mentions a sequence of Odin Mentions
    * @param manager  an AssemblyManager
    * @return an AssemblyManager
    */
  def combinedRBPrecedence(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    val name = "combinedRBPrecedence"

    logger.debug(s"\tapplying '$name' sieve...")

    //AssemblySieve(bioDRBpatterns) andThen AssemblySieve(intersententialRBPrecedence) andThen AssemblySieve(intrasententialRBPrecedence)

    val actions = new AssemblyActions
    val intraSententialRules =  "/org/clulab/reach/assembly/grammars/intrasentential.yml"
    val interSententialRules = "/org/clulab/reach/assembly/grammars/intersentential.yml"
    val bioDRBRules = "/org/clulab/reach/assembly/grammars/biodrb-patterns.yml"
    //val precedenceMarkerRules = "/org/clulab/reach/assembly/grammars/precedence-markers.yml"
    // intrasentence patterns
    val am2 = applyPrecedenceRules(mentions, manager, name, intraSententialRules, actions)
    // cross-sentence patterns
    val am3 = applyPrecedenceRules(mentions, am2, name, interSententialRules, actions)
    // patterns derived from BioDRB paper
    val am4 = applyPrecedenceRules(mentions, am3, name, bioDRBRules, actions)
    // check for interceding precedence markers
    //val am5 = applyPrecedenceRules(mentions, am4, name, precedenceMarkerRules, actions)

    //am5
    am4
  }
  
  /**
    * Rule-based method using grammatical tense and aspect to establish precedence
    *
    * @param mentions a sequence of Odin Mentions
    * @param manager  an AssemblyManager
    * @return an AssemblyManager
    */
  def reichenbachPrecedence(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    val BEFORE = "before"
    val AFTER = "after"

    val name = "reichenbachPrecedence"

    logger.debug(s"\tapplying '$name' sieve...")

    val tam_rules = "/org/clulab/reach/assembly/grammars/tense_aspect.yml"

    def getTam(ev: Mention, tams: Seq[Mention], label: String): Option[Mention] = {
      val relevant: Set[Mention] = tams.filter{ tam =>
        val triggerInterval = SieveUtils.findTrigger(ev).tokenInterval
        (tam.document == ev.document) &&
          (tam.sentence == ev.sentence) &&
          (tam matches label) &&
          (tam.tokenInterval overlaps triggerInterval)}.toSet
      // rules should produce at most one tense or aspect mention
      if (relevant.size > 1 ) logger.debug(s"""Found TAMs of ${relevant.map(_.label).mkString(", ")}\nEvent text:'${ev.text}'\nEvent sentence: '${ev.sentenceObj.getSentenceText}'\nTam span: ${relevant.map(_.text).mkString("'", "', '", "'")}\n=================""")
      relevant.headOption
    }

    def tamLabel(tam: Option[Mention]): String = {
      tam match {
        case None => "none"
        case Some(hasLabel) => hasLabel.label
      }
    }

    def getReichenbach(e1t: String, e1a: String, e2t: String, e2a: String): String = {
      (e1t, e1a, e2t, e2a) match {
        case ("PastTense", "none", "PastTense", "Perfective") => AFTER
        case ("PastTense", "none", "FutureTense", "none") => BEFORE
        case ("PastTense", "none", "FutureTense", "Perfective") => BEFORE
        //
        case ("PastTense", "Perfective", "PastTense", "none") => BEFORE
        case ("PastTense", "Perfective", "PresentTense", "none") => BEFORE
        case ("PastTense", "Perfective", "PresentTense", "Perfective") => BEFORE
        case ("PastTense", "Perfective", "FutureTense", "none") => BEFORE
        case ("PastTense", "Perfective", "FutureTense", "Perfective") => BEFORE
        //
        case ("PresentTense", "none", "PastTense", "Perfective") => AFTER
        case ("PresentTense", "none", "FutureTense", "none") => BEFORE
        //
        case ("PresentTense", "Perfective", "PastTense", "Perfective") => AFTER
        case ("PresentTense", "Perfective", "FutureTense", "none") => BEFORE
        case ("PresentTense", "Perfective", "FutureTense", "Perfective") => BEFORE
        //
        case ("FutureTense", "none", "PastTense", "none") => AFTER
        case ("FutureTense", "none", "PastTense", "Perfective") => AFTER
        case ("FutureTense", "none", "PresentTense", "none") => AFTER
        case ("FutureTense", "none", "PresentTense", "Perfective") => AFTER
        //
        case ("FutureTense", "Perfective", "PastTense", "none") => AFTER
        case ("FutureTense", "Perfective", "PastTense", "Perfective") => AFTER
        case ("FutureTense", "Perfective", "PresentTense", "Perfective") => AFTER
        case _ => "none"
      }
    }

    // TODO: only look at events with verbal triggers
    val evs = mentions.filter(isEvent)
    val eventTriggers = evs.map(SieveUtils.findTrigger)
    val tams = assemblyViaRules(tam_rules, eventTriggers)
    val tenseMentions = tams.filter(_ matches "Tense")
    val aspectMentions = tams.filter(_ matches "Aspect")

    // for keeping track of counts of TAM relations
    // val relCounts = scala.collection.mutable.Map[(String, String, String, String), Int]()

    for {
      events <- evs.groupBy(_.document).values
      e1 <- events
      e2 <- events
      if e1.precedes(e2) && isValidRelationPair(e1, e2) && noExistingPrecedence(e1, e2, manager)

      e1tense = getTam(e1, tenseMentions, "Tense")
      e1aspect = getTam(e1, aspectMentions, "Aspect")
      e2tense = getTam(e2, tenseMentions, "Tense")
      e2aspect = getTam(e2, aspectMentions, "Aspect")

      pr = getReichenbach(tamLabel(e1tense), tamLabel(e1aspect), tamLabel(e2tense), tamLabel(e2aspect))
    } {
      // record e1 and e2 TAMs
      // if (e1.precedes(e2)) {
      //   relCounts((tamLabel(e1tense), tamLabel(e1aspect), tamLabel(e2tense), tamLabel(e2aspect))) = relCounts.getOrElseUpdate((tamLabel(e1tense), tamLabel(e1aspect), tamLabel(e2tense), tamLabel(e2aspect)), 0) + 1
      // }

      val e1tenseMentions: Seq[Mention] = if (e1tense.nonEmpty) Seq(e1tense.get) else Seq.empty[Mention]
      val e1aspectMentions: Seq[Mention] = if (e1aspect.nonEmpty) Seq(e1aspect.get) else Seq.empty[Mention]
      val e2tenseMentions: Seq[Mention] = if (e2tense.nonEmpty) Seq(e2tense.get) else Seq.empty[Mention]
      val e2aspectMentions: Seq[Mention] = if (e2aspect.nonEmpty) Seq(e2aspect.get) else Seq.empty[Mention]

      pr match {
        case `BEFORE` =>
          // create evidence mention
          val evidence = new RelationMention(
            SieveUtils.precedenceMentionLabel,
            Map(
              SieveUtils.beforeRole -> Seq(e1),
              SieveUtils.afterRole -> Seq(e2),
              "e1tense" -> e1tenseMentions,
              "e1aspect" -> e1aspectMentions,
              "e2tense" -> e2tenseMentions,
              "e2aspect" -> e2aspectMentions
            ),
            e1.sentence,
            e1.document,
            true,
            name
          )
          // TODO: add score
          manager.storePrecedenceRelation(before = e1, after = e2, Set[Mention](evidence), name)
        case `AFTER` =>
          // create evidence mention
          val evidence = new RelationMention(
            SieveUtils.precedenceMentionLabel,
            Map(
              SieveUtils.beforeRole -> Seq(e2),
              SieveUtils.afterRole -> Seq(e1),
              "e1tense" -> e1tenseMentions,
              "e1aspect" -> e1aspectMentions,
              "e2tense" -> e2tenseMentions,
              "e2aspect" -> e2aspectMentions
            ),
            e1.sentence,
            e1.document,
            true,
            name
          )
          // TODO: add score
          manager.storePrecedenceRelation(before = e2, after = e1, Set[Mention](evidence), name)
        case _ => ()
      }
    }

    // print the counts of the tam relations
    // relCounts.foreach(tam => println(s"${tam._1}\t${tam._2}"))

    manager
  }

  def featureBasedClassifierNoSharedArgs(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    def meetsRequirements(m1: Mention, m2: Mention): Boolean = {
      // Assume same window size imposed on corpus
      // NOTE: this also checks document equality,
      // which is needed because of .precedes check in FeatureExtractor
      Constraints.withinWindow(m1, m2, kWindow) &&
        Constraints.isValidRelationPair(m1, m2)
    }

    val name = "feature-based-classifier-w/o-shared-arg-requirement"

    logger.debug(s"\tapplying '$name' sieve...")

    val validMentions = mentions.filter(validPrecedenceCandidate)

    classifyCausalPrecedencePairs(validMentions, manager, name, meetsRequirements)
  }

  def featureBasedClassifierWithSharedArgs(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {

    def meetsRequirements(m1: Mention, m2: Mention): Boolean = {
      // Assume same window size imposed on corpus
      // NOTE: this also checks document equality,
      // which is needed because of .precedes check in FeatureExtractor
      Constraints.withinWindow(m1, m2, kWindow) &&
        Constraints.shareEntityGrounding(m1, m2) &&
        Constraints.isValidRelationPair(m1, m2)
    }

    val name = "feature-based-classifier-with-shared-arg-requirement"

    logger.debug(s"\tapplying '$name' sieve...")

    val validMentions = mentions.filter(validPrecedenceCandidate)

    classifyCausalPrecedencePairs(validMentions, manager, name, meetsRequirements)
  }

  // TODO: (selectively?) establish causal predecessors between controller and controlled of regulations
  // ex. A is required for B
  def regulationsToCausalPredecessors(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = ???

  // TODO: Propagate input features from causal predecessors
  // Extend ParticipantFeatureTracker.getInputFeatures to handle events
  //   - right now it assumes things are flattened
  def propagateInputFeatures(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = ???

  // TODO: Keep only the "most complete" input features (ex Phos vs. Phos @ B)
  def keepMostCompleteInputFeatures(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = ???

  // TODO: Write sieve to extend causal predecessors to equivalent EERs
  // Could be selective via voting, textual proximity, etc.
  def extendPredecessorsToEquivalentEERs(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = ???

//  def graphCSP(mentions: Seq[Mention], manager: AssemblyManager): AssemblyManager = {
//
//    // http://perso.ensta-paristech.fr/~diam/corlab/online/choco/ChocoGraphDoc-20150118.pdf
//
//    val allEvents = manager.getEvents.toSeq.sortBy(_.equivalenceHash)
//
//    val dg = new DirectedGraph(
//      // the maximum number of nodes in the graph (solution)
//      allEvents.size,
//      // use SetType.LINKED_LIST for sparse graphs
//      // SetType.BIPARTITESET has optimal time complexity, but memory usage can be expensive
//      // SetType.BITSET: supposedly a good default (why?)
//      SetType.BIPARTITESET,
//      // whether or not the node set is fixed
//      false
//    )
//
//    val solver = new Solver//("precedence-solver")
//
//    // define graph lower-bound (i.e., what nodes and links must exist in solution?)
//    val GLB = new DirectedGraph(
//      solver,
//      allEvents.size,
//      SetType.BIPARTITESET,
//      // whether or not the node set is fixed
//      // TODO: Should this be false?
//      true
//    )
//
//    // define graph upper-bound (i.e., what nodes and links could possibly exist?)
//    val GUB = new DirectedGraph(
//      solver,
//      allEvents.size,
//      SetType.BIPARTITESET,
//      // whether or not the node set is fixed
//      // TODO: Should this be false?
//      false
//    )
//
//    for {
//      (e1: EER, i: Int) <- allEvents.zipWithIndex
//      (e2: EER, j: Int) <- allEvents.zipWithIndex
//    } {
//
//      // nodes exist in upper-bound
//      GUB.addNode(i)
//      GUB.addNode(j)
//      // TODO: Should this only be allowed if the opposite is not true?
//      GUB.addArc(i, j)
//
//      // check if a required solution
//      (e1, e2) match {
//        // TODO: should this check if any of successors is equiv to succesor?
//        case (pred, successor) if pred.successors contains successor =>
//          // predicted precedence relation, so nodes must exist in
//          GLB.addNode(i)
//          GLB.addNode(j)
//          GLB.addArc(i, j)
//      }
//    }
//
//    val g = GraphVarFactory.directed_graph_var("G", GLB, GUB, solver)
//
//    // make antisymmetric ( if (i,j), then ~(j,i)
//    solver.post(GraphConstraintFactory.antisymmetric(g))
//    // use transitivity
//    solver.post(GraphConstraintFactory.transitivity(g))
//    // solve!
//    ???
//  }
}



/**
  * Utilities commonly used by Sieves
  */
object SieveUtils extends LazyLogging {

  // load feature-based classifier
  val config = ConfigFactory.load()
  val classifierPath = config.getString("assembly.classifier.model")
  val kWindow = config.getInt("assembly.windowSize")

  lazy val clf = AssemblyRelationClassifier.loadFrom(classifierPath)

  val E1PrecedesE2 = "E1 precedes E2"
  val E2PrecedesE1 = "E2 precedes E1"
  // subsumption and equivalence
  val E1SpecifiesE2 = "E1 specifies E2"
  val E2SpecifiesE1 = "E2 specifies E1"
  val Equivalent = "Equivalent"
  // default label for negative class
  val NEG = AssemblyRelationClassifier.NEG

  val precedenceRelations =  Set(E1PrecedesE2, E2PrecedesE1)
  val subsumptionRelations = Set(E1SpecifiesE2, E2SpecifiesE1)
  val equivalenceRelations = Set(Equivalent)
  val noRelations = Set(NEG)

  // the label used to identify Mentions modelling precedence relations
  val precedenceMentionLabel = "PrecedenceRelation"
  val beforeRole = "before"
  val afterRole = "after"

  val dedup = new DeduplicationSieves()
  val precedence = new PrecedenceSieves()

  val sieves = Map(
    "intrasententialRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.intrasententialRBPrecedence)),
    "reichenbachPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.reichenbachPrecedence)),
    "intersententialRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.intersententialRBPrecedence)),
    "combinedRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.combinedRBPrecedence))
  )

  /**
    * Check if mention is a causal precedence candidate
    *
    * @param m an Odin-style Mention
    * @return true or false
    */
  def validPrecedenceCandidate(m: Mention): Boolean = m match {
    case event if event matches "Event" => true
    case validEntity if validEntity matches "/^(Complex|Bioprocess)$".r => true
    case _ => false
  }

  /**
    * Applies a set of assembly rules to provide mentions (existingMentions). <br>
    * Care is taken to apply the rules to each set of mentions from the same Document.
    *
    * @param rules a path to a rule file (assumed to be under resources)
    * @param existingMentions a Seq of Odin Mentions
    * @return a Seq of RelationMentions
    */
  def assemblyViaRules(rules: String, existingMentions: Seq[Mention], actions: Actions = new Actions): Seq[Mention] = {

    // read rules and initialize state with existing mentions
    val odinRules: String = if (rules.endsWith(".yml")) RuleReader.readResource(rules) else rules
    val ee = ExtractorEngine(odinRules, actions)

    // since we break a paper into sections, we'll need to group the mentions by doc
    // rule set only produces target RelationMentions

    val assembledMentions: Iterable[Mention] =
      for {
      // NOTE: Odin expects all mentions in the state to belong to the same doc!
        (doc, mentionsFromReach) <- existingMentions.groupBy(_.document)
        // create a new state with just the mentions from a particular doc
        // note that a doc is as granular as a section of a paper
        oldState = State(mentionsFromReach)
        // initialize a state with the subset of mentions
        // belonging to the same doc.
        m <- ee.extractFrom(doc, oldState)
      } yield m

    assembledMentions
      .toSeq
  }

  /**
    * Classifies pairs of mentions meeting given criteria using the feature-based precedence classifier
    *
    * @param validMentions a set of candidate mentions to generate pairs for classification
    * @param manager an [[AssemblyManager]]
    * @param sieveName the name of the sieve applying the classifier (used when storing a precedence relation)
    * @param isValidPair a function that tests whether or not a pair of mentions should be considered for classification
    * @return
    */
  def classifyCausalPrecedencePairs(
    validMentions: Seq[Mention],
    manager: AssemblyManager,
    sieveName: String,
    isValidPair: (Mention, Mention) => Boolean): AssemblyManager = {
    for {
      e1 <- validMentions
      e2 <- validMentions
      if isValidPair(e1, e2)
      // classify (score used to assess confidence in relation)
      (label: String, score: Double) = clf.getLabelWithScore(e1, e2)
      if label != AssemblyRelationClassifier.NEG
      // make sure the prediction is not a contradiction
      if Constraints.noExistingPrecedence(e1, e2, manager)
    } {
      label match {
        case E1PrecedesE2 =>
          val evidence = createEvidenceForCPR(e1, e2, sieveName)
          manager.storePrecedenceRelation(before = e1, after = e2, evidence, sieveName, score)
        case E2PrecedesE1 =>
          val evidence = createEvidenceForCPR(e2, e1, sieveName)
          manager.storePrecedenceRelation(before = e2, after = e1, evidence, sieveName, score)
      }
    }
    manager
  }

  /**
    * Returns true if the mention is an Event and therefore a candidate for precedence
    *
    * @param m an Odin Mention
    * @return a Boolean
    */
  def isEvent(m:Mention) = m.matches("Event")

  /** Retrieve trigger from Mention */
  @tailrec
  def findTrigger(m: Mention): TextBoundMention = m match {
    // if mention is TB, just use the mention
    case tb: TextBoundMention =>
      logger.debug(s"no trigger for mention '${tb.text}' with label '${m.label}'")
      tb
    case event: EventMention =>
      event.trigger
    case rel: RelationMention if (rel matches "ComplexEvent") && rel.arguments("controlled").nonEmpty =>
      // could be nested ...
      findTrigger(rel.arguments("controlled").head)
  }

  /**
    * Create evidence from Causal Precedence relation
    *
    * @param before an Odin-style Mention preceding 'after'
    * @param after an Odin-style Mention following 'before'
    * @param foundBy the name of the sieve that found the relation
    * @return a set of Mention
    */
  def createEvidenceForCPR(
    before: Mention,
    after: Mention,
    foundBy: String
  ): Set[Mention] = {
    val evidence = new RelationMention(
      SieveUtils.precedenceMentionLabel,
      Map(beforeRole -> Seq(before), afterRole -> Seq(after)),
      before.sentence,
      before.document,
      true,
      foundBy
    )
    Set(evidence)
  }
}
