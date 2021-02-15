package org.clulab.reach.assembly

import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.assembly.representations._
import collection.Map
import collection.immutable
import org.clulab.odin._
import org.clulab.reach.mentions.{CorefMention, MentionOps}
// used to differentiate AssemblyModifications from Modifications on mentions
import org.clulab.reach.mentions
import java.io.File


/**
  * Stores precedence information for two distinct [[EntityEventRepresentation]]
  * @param before the [[EntityEventRepresentation] that precedes [[PrecedenceRelation.after]]
  * @param after the [[EntityEventRepresentation]] that follows [[PrecedenceRelation.before]]
  * @param evidence the mentions that serve as evidence for this precedence relation
  * @param foundBy the name of the Sieve which found this relation
  */
case class PrecedenceRelation(
  before: EER,
  after: EER,
  var evidence: Set[Mention],
  foundBy: String
) {

  var confidence: Double = AssemblyManager.DEFAULT_CONFIDENCE

  /**
    * Returns true if the input argument is a [[PrecedenceRelation]] with identical before and after [[EntityEventRepresentation]]s
    * @param other Any comparison object
    * @return a Boolean
    */
  // TODO: Should this be in terms of an Equiv Hash?
  def strictlyEquivalent(other: Any): Boolean = other match {
    case pr: PrecedenceRelation => this.before == pr.before && this.after == pr.after
    case _ => false
  }

  /**
    * Returns true if the input argument is a [[PrecedenceRelation]] with either the same before or after [[EntityEventRepresentation]]s
    * @param other Any comparison object
    * @param ignoreMods whether or not modifications should be ignored when assessing equivalence
    * @return a Boolean
    */
  def isEquivalentTo(other: Any, ignoreMods: Boolean): Boolean = other match {
    case pr: PrecedenceRelation => this.before.isEquivalentTo(pr.before, ignoreMods) && this.after.isEquivalentTo(pr.after, ignoreMods)
    case _ => false
  }
}

/**
  * @constructor Creates a new AssemblyManager from two LUTs: (Mention -> [[IDPointer]]) and ([[IDPointer]] -> [[EntityEventRepresentation]]).
  *             These LUTs are used to populate the mentionStateToID and idToEERepresentation LUTs containing the same information.
  *             Subsequent updates to these LUTs create new LUTs.
  *             The motivation for the LUTs was to allow for changes in the mapping of a Mention -...-> [[EntityEventRepresentation]]
  *             to easily propagate in nested cases.
  *             For example, an update to the (Mention -> [[EntityEventRepresentation]]) mapping of a SimpleEvent Mention will propagate
  *             to the (Mention -> [[EntityEventRepresentation]]) mapping of any ComplexEvent Mention containing this SimpleEvent Mention.
  * @param m2id a lookup table from Mention -> [[IDPointer]].  Each key (Mention) should map to a unique [[IDPointer]].
  * @param id2eer a lookup table from [[IDPointer]] -> [[EntityEventRepresentation]].
  *                Keys ([[IDPointer]]) may point to the same value (EntityEventRepresentation)
  */
class AssemblyManager(
  m2id: Map[MentionState, IDPointer],
  id2eer: Map[IDPointer, EER]
) extends Serializable with LazyLogging {

  import AssemblyManager._

  // Because modifications don't feature into the hashcode,
  // a mention's identify at assembly consists of both the mention and its mods (i.e., the "state" of the mention)
  private var mentionStateToID: immutable.Map[MentionState, IDPointer] = m2id.toMap
  private var idToEER: immutable.Map[IDPointer, EER] = id2eer.toMap
  // faster lookup of equivalent events
  // FIXME: should ignoreMods be false?
  private var ehToEERs: immutable.Map[Int, Set[EER]] = {
    // group by EH
    id2eer.toSeq.groupBy(_._2.equivalenceHash(ignoreMods = false))
      // get EERs
      .mapValues(_.map(_._2).toSet)
  }

  private var idToMentionState: immutable.Map[IDPointer, MentionState] = mentionStateToID.map{ case (k, v) => (v, k)}
  // PrecedenceRelations associated with a distinct EER
  private var EERtoPrecedenceRelations: immutable.Map[EER, Set[PrecedenceRelation]] =
    Map.empty
      .withDefaultValue(Set.empty[PrecedenceRelation])
  // initialize to size of LUT 2
  private var nextID: IDPointer = idToEER.size

  /**
    * Retrieve the set of mentions currently tracked by the manager
    */
  def getMentions: Set[Mention] = mentionStateToID.keys.map(_._1).toSet

  //
  // Handle PrecedenceRelations
  //

  /**
    * Stores a PrecedenceRelation in [[EERtoPrecedenceRelations]] connecting "before" and "after".
    * Tracks "before" and "after" Mentions and produces EERs, is not already present.
    * @param before an Odin Mention that causally precedes "after"
    * @param after an Odin Mention that (causally) follows "before"
    * @param evidence a Set of Odin Mentions serving as evidence for the precedence relation
    * @param foundBy the name of the sieve or procedure that discovered this precedence relation
    */
  def storePrecedenceRelation(
    before: Mention,
    after: Mention,
    evidence: Set[Mention] = Set.empty[Mention],
    foundBy: String,
    confidence: Double = AssemblyManager.DEFAULT_CONFIDENCE
  ): Unit = {

    // ensure Mentions are being tracked
    // and get their corresponding EERs
    val eer1 = getOrCreateEER(before)
    val eer2 = getOrCreateEER(after)
    val ev: Set[Mention] = if (evidence.isEmpty) sieves.SieveUtils.createEvidenceForCPR(before, after, foundBy) else evidence
    storePrecedenceRelation(before = eer1, after = eer2, ev, foundBy, confidence)
  }

  /**
    * Stores a PrecedenceRelation in [[EERtoPrecedenceRelations]] for the EERs corresponding to "before" and "after"
    * @param before an [[EntityEventRepresentation]] that causally precedes "after"
    * @param after an [[EntityEventRepresentation]] that (causally) follows "before"
    * @param foundBy the name of the sieve or procedure that discovered this precedence relation
    */
  def storePrecedenceRelation(
    before: EER,
    after: EER,
    evidence: Set[Mention],
    foundBy: String,
    confidence: Double
  ): Unit = {

    val pr = PrecedenceRelation(
      before,
      after,
      evidence,
      foundBy
    )

    // adjust confidence level
    pr.confidence = confidence

    updateEERtoPrecedenceRelations(pr)
  }

  def storePrecedenceRelation(
    before: EER,
    after: EER,
    foundBy: String,
    confidence: Double
  ): Unit = storePrecedenceRelation(before, after, Set.empty[Mention], foundBy, confidence)

  /**
    * Update entries in [[EERtoPrecedenceRelations]] for pr.before and pr.after
    * @param pr a [[PrecedenceRelation]]
    */
  private def updateEERtoPrecedenceRelations(pr: PrecedenceRelation): Unit = {
    // update PRs for before
    val before = pr.before
    val oldBefore = EERtoPrecedenceRelations.getOrElse(before, Set.empty)
    EERtoPrecedenceRelations = EERtoPrecedenceRelations + (before -> (oldBefore ++ Set(pr)))
    // update PRs for after
    val after = pr.after
    val oldAfter = EERtoPrecedenceRelations.getOrElse(after, Set.empty)
    EERtoPrecedenceRelations = EERtoPrecedenceRelations + (after -> (oldAfter ++ Set(pr)))
  }

  // retrieval of PrecedenceRelations
  /**
    * Retrieves the Set of PrecedenceRelations corresponding to the provided [[EntityEventRepresentation.equivalenceHash]] (eh)
    * @param eer an [[EntityEventRepresentation]]
    */
  def getPrecedenceRelationsFor(eer: EER): Set[PrecedenceRelation] = EERtoPrecedenceRelations(eer)

  /**
    * Retrieves the Set of PrecedenceRelations corresponding to the provided Mention
    * @param m an Odin Mention
    */
  def getPrecedenceRelationsFor(m: Mention): Set[PrecedenceRelation] = {
    getPrecedenceRelationsFor(getOrCreateEER(m))
  }

  /**
    * Retrieves the set of PrecedenceRelations for all Events
    */
  def getPrecedenceRelations: Set[PrecedenceRelation] = {
    for {
      e <- getEvents
      pr <- getPrecedenceRelationsFor(e)
    } yield pr
  }

  /**
    * Retrieves the distinct Set of EER predecessors for the provided EER.
    * @param eer an [[EntityEventRepresentation]]
    * @return the Set of distinct EntityEventRepresentations known to causally precede any EER corresponding to [[EntityEventRepresentation.equivalenceHash]]
    */
  def distinctPredecessorsOf(eer: EER): Set[EER] = {
    val predecessors = predecessorsOf(eer)
    distinctEERsFromSet(predecessors, ignoreMods = false)
  }

  /**
    * Retrieves the distinct Set of EER predecessors for the provided Mention (m).
    * @param m an Odin Mention
    * @return the Set of distinct EntityEventRepresentations known to causally precede the EER corresponding to M
    */
  def distinctPredecessorsOf(m: Mention): Set[EER] = m match {
    case isValid if AssemblyManager.isValidMention(m) =>
      distinctPredecessorsOf(getOrCreateEER(m))
    case _ => Set.empty[EER]
  }

  /**
    * Retrieves the non-distinct Set of EER predecessors for the provided EER.
    * @param eer an [[EntityEventRepresentation]]
    * @return the Set of non-distinct EntityEventRepresentations known to causally precede eer
    */
  def predecessorsOf(eer: EER): Set[EER] = for {
    pr <- EERtoPrecedenceRelations(eer)
    if pr.after.equivalenceHash(ignoreMods = false) == eer.equivalenceHash(ignoreMods = false)
  } yield pr.before

  /**
    * Retrieves the non-distinct Set of EER predecessors for the provided Mention (m).
    * @param m an Odin Mention
    * @return the Set of non-distinct EntityEventRepresentations known to causally precede the EER corresponding to m
    */
  def predecessorsOf(m: Mention): Set[EER] = m match {
    // check if valid mention
    case isValid if AssemblyManager.isValidMention(isValid) =>
      predecessorsOf(getOrCreateEER(isValid))
    case _ => Set.empty[EER]
  }

  /**
    * Retrieves the distinct Set of EER successors for the provided EER.
    * @param eer an [[EntityEventRepresentation]]
    * @return the Set of distinct EntityEventRepresentations known to causally succeed any EER corresponding to eh
    */
  def distinctSuccessorsOf(eer: EER): Set[EER] = {
    val successors = successorsOf(eer)
    distinctEERsFromSet(successors, ignoreMods = false)
  }

  /**
    * Retrieves the distinct Set of EER successors for the provided Mention (m).
    * @param m an Odin Mention
    * @return the Set of distinct EntityEventRepresentations known to causally succeed any EER corresponding to eh
    */
  def distinctSuccessorsOf(m: Mention): Set[EER] = m match {
    // check if Mention is valid
    case isValid if AssemblyManager.isValidMention(isValid) =>
      distinctSuccessorsOf(getOrCreateEER(isValid))
    case _ => Set.empty[EER]
  }

  /**
    * Retrieves the non-distinct Set of EER successors for the provided EER.
    * @param eer an [[EntityEventRepresentation]]
    * @return the Set of non-distinct EntityEventRepresentations known to causally succeed eer
    */
  def successorsOf(eer: EER): Set[EER] = for {
    pr <- getPrecedenceRelationsFor(eer)
    if pr.before.equivalenceHash(ignoreMods = false) == eer.equivalenceHash(ignoreMods = false)
  } yield pr.after

  /**
    * Retrieves the non-distinct Set of EER successors for the provided Mention (m).
    * @param m an Odin Mention
    * @return the Set of non-distinct EntityEventRepresentations known to causally succeed the EER corresponding to m
    */
  def successorsOf(m: Mention): Set[EER] = m match {
    // check if Mention is valid
    case isValid if AssemblyManager.isValidMention(isValid) =>
      successorsOf(getOrCreateEER(isValid))
    case _ => Set.empty[EER]
  }

  //
  // Utils for processing mentions
  //

  /**
    * Creates an [[EntityEventRepresentation]] if m is a valid Mention
    * See [[isValidMention]] for details on validation check
    * @param m an Odin Mention
    */
  def trackMention(m: Mention): Unit = isValidMention(m) match {
    // do not store Sites, Activations, etc. in LUT 1
    case true =>
      // get or create an EntityEventRepresentation
      val _ = getOrCreateEER(m)
    case false => ()
  }

  /**
    * Creates an [[EntityEventRepresentation]] for each valid Mention
    * See [[isValidMention]] for details on validation check
    * @param mentions a sequence of Mention to store in the AssemblyManager LUTs
    */
  def trackMentions(mentions: Seq[Mention]): Unit = mentions.foreach(trackMention)

  /**
    * Gets the polarity of a mention.  Should only be relevant to ComplexEvents
    * @param m an Odin Mention
    * @return [[AssemblyManager.positive]], [[AssemblyManager.negative]], or [[AssemblyManager.unknown]]
    */
  def getPolarityLabel(m: Mention): String = m match {
    case pos if pos matches "(?i)^positive".r => AssemblyManager.positive
    case neg if neg matches "(?i)^negative".r => AssemblyManager.negative
    case _ => AssemblyManager.unknown
  }

  //
  // Utils for creating IDs
  //

  /**
    * Creates a unique [[IDPointer]].
    * This implementation does not rely on updates to either the [[mentionStateToID]] or [[idToEER]] LUT to determine a unique [[IDPointer]].
    * @return a unique [[IDPointer]]
    */
  // use the size of LUT 2 to create a new ID
  private def createID: IDPointer = {
    val currentID = nextID
    nextID += 1
    currentID
  }

  /**
    * Attempts to retrieve an [[IDPointer]] for a Mention, and creates a new [[IDPointer]] if none is found.
    * @param m an Odin Mention
    * @return an [[IDPointer]] unique to m
    */
  private def getOrCreateID(m: Mention): IDPointer = {
    mentionStateToID.getOrElse(getMentionState(m), createID)
  }

  //
  // Utils for modifying storage tables
  //

  /**
    * Updates the [[mentionStateToID]] and [[idToEER]] LUTs
    * @param id a unique [[IDPointer]] for m
    * @param m an Odin Mention
    * @param eer the [[EntityEventRepresentation]] corresponding to m
    */
  private def updateLUTs(id: IDPointer, m: Mention, eer: EER): Unit = {
    // update LUT #1a
    updateMentionStateToIDTable(m, id)
    // update LUT #1b
    updateIDtoMentionStateTable(m, id)
    // update LUT #2
    updateIDtoEERTable(id, eer)
    // update LUT #3
    updateEHtoEERsTable(eer)
  }

  /**
    * Updates the [[ehToEERs]] LUT
    * @param eer an [[EER]]
    */
  private def updateEHtoEERsTable(eer: EER): Unit = {

    // get EERs for the given hash
    def getIDs(eh: Int): Set[EER] = ehToEERs.getOrElse(eh, Set.empty[EER]) ++ Set(eer)

    // update the set of EERs with identical mods
    val eh1 = eer.equivalenceHash(ignoreMods = false)
    val eers1 = getIDs(eh1)
    ehToEERs = ehToEERs + (eh1 -> eers1)

    // update the set of EERs without identical mods
    val eh2 = eer.equivalenceHash(ignoreMods = true)
    val eers2 = getIDs(eh2)
    ehToEERs = ehToEERs + (eh2 -> eers2)
  }

  /**
    * Updates the [[mentionStateToID]] LUT
    * @param m an Odin Mention
    * @param id an [[IDPointer]] unique to m
    */
  private def updateMentionStateToIDTable(m: Mention, id: IDPointer): Unit = {
    mentionStateToID = mentionStateToID + (getMentionState(m) -> id)
  }

  /**
    * Updates the [[idToMentionState]] LUT
    * @param m an Odin Mention
    * @param id an [[IDPointer]] unique to m
    */
  private def updateIDtoMentionStateTable(m: Mention, id: IDPointer): Unit = {
    idToMentionState = idToMentionState + (id -> getMentionState(m))
  }

  /**
    * Updates the [[idToEER]] LUT
    * @param id a unique [[IDPointer]] pointing to eer
    * @param eer an [[EntityEventRepresentation]] associated with the provided id
    */
  private def updateIDtoEERTable(id: IDPointer, eer: EER): Unit = {
    idToEER = idToEER + (id -> eer)
    // update the ehToEERs table
    updateEHtoEERsTable(eer)
  }

  //
  // Utils for removing IDs
  //

  /**
    * Removes mention and corresponding [[EntityEventRepresentation]] associated with the provided id.
    * If the corresponding EntityEventRepresentation is a [[SimpleEvent]], remove its output as well.
    * @param id an [[IDPointer]] used to identify mentions and EntityEventRepresentations for removal
    */
  def removeEntriesContainingID(id: IDPointer): Unit = {

    // get ids of EEReprs containing the given id
    val ids = for {
      r <- idToEER.values.toSeq
      if r.containsID(id)
    } yield r match {
      // if an event, get event's id and ids of event's outputs
      case event: SimpleEvent =>
        event.outputPointers ++ Set(event.uniqueID)
      case eer => Set(eer.uniqueID)
    }

    removeEntriesCorrespondingToIDs(ids.flatten)
  }

  /**
    * Removes mention and corresponding EERepresentation from the AssemblyManager
    * @param m
    */
  def removeEntriesContainingIDofMention(m: Mention): Unit = {
    val id = getOrCreateID(m)
    removeEntriesContainingID(id)
  }

  /**
    * Removes entries referencing the given [[EntityEventRepresentation]].
    * @param eer an [[EntityEventRepresentation]] used to identify mentions and EntityEventRepresentations for removal
    */
  def removeEntriesContainingIDofEER(eer: EER): Unit = {

    // get ids of EERepresentations containing the id of the given EERepresentation
    val idsForRemoval: Seq[IDPointer] = for {
      r <- idToEER.values.toSeq
      if r.containsID(eer.uniqueID)
    } yield r.uniqueID

    removeEntriesCorrespondingToIDs(idsForRemoval)
  }

  /**
    * Removes entries referencing the any of the given [[IDPointer]].
    * @param ids a Seq[IDPointer] used to identify mentions and EntityEventRepresentations for removal
    */
  def removeEntriesCorrespondingToIDs(ids: Seq[IDPointer]): Unit = {

    // remove mentions associated with the IDs

    val id2m = idToMentionState

    for {
      id <- ids
      if id2m contains id
      m = id2m(id)
    } {
      mentionStateToID = mentionStateToID - m
    }

    // remove EEReprs containing the given id
    for {
      id <- ids
    } {
      idToEER = idToEER - id
    }

  }

  //
  // Utils for handling modifications
  //

  /**
    * Builds a Set[AssemblyModfication] from the modifcations belonging to a Mention m.
    * Currently, only a subset of Mention [[org.clulab.reach.mentions.Modification]] are considered relevant to assembly:
    * PTM
    * Mutant
    *
    * Additionally, a Mention corresponding to an Entity will include an [[EntityLabel]] [[AssemblyModification]] encoding its label (ex. Family)
    * @param m an Odin Mention
    * @return Set[AssemblyModification]
    */
  protected def mkAssemblyModifications(m: Mention): Set[AssemblyModification] = {
    // we only care to represent a subset of the Modifications associated with a mention
    val mods: Set[AssemblyModification] =
      m.toBioMention.modifications flatMap {
        // TODO: is site part of label?
        case mut: mentions.Mutant => Set(MutantEntity(mut.label))
        // TODO: should site be handled differently?
        case ptm: mentions.PTM =>
          val site: Option[String] = if (ptm.site.nonEmpty) Some(ptm.site.get.text) else None
          Set(PTM(ptm.label, site, ptm.negated))
        case _ => Nil
      }
    if (m matches "Entity") Set(EntityLabel(m.label)) ++ mods else mods
  }

  //
  // SimpleEntity creation
  //

  /**
    * Create a [[SimpleEntity]] representation from a Mention
    * and an optional set of optional modifications (useful for building output of SimpleEvent)
    *
    * Whenever modifications are provided, the [[mentionStateToID]] LUT is NOT updated, so as to avoid a conflict with the existing mapping (see the description of mods for the motivation)
    * @param m an Odin Mention
    * @param mods an optional set of [[AssemblyModification]].
    *             This is useful for building the output of a [[SimpleEvent]] (any simple event other than a Binding), which is a set of [[SimpleEvent]] where the key [[PTM]] comes from the [[SimpleEvent]]
    *             (i.e., the PTM cannot be recovered by simply examining m out of context)
    * @return a tuple of ([[IDPointer]], [[SimpleEntity]])
    */
  protected def createSimpleEntityWithID(
    m: Mention,
    mods: Option[Set[AssemblyModification]]
  ): (SimpleEntity, IDPointer) = {

    /** Used to create "new" mention whenever mods are provided **/
    def createEvidence(m: Mention): Mention = m match {
      case tb: TextBoundMention => tb.copy(foundBy = s"${tb.foundBy}-output-representation")
      case rel: RelationMention => rel.copy(foundBy = s"${rel.foundBy}-output-representation")
      case em: EventMention => em.copy(foundBy = s"${em.foundBy}-output-representation")
    }

    // check for coref
    val e = getResolvedForm(m)

    // mention should be an Entity or Cellular_component
    require((e matches "Entity") || (e matches "Cellular_component"), "createSimpleEntity requires an 'Entity' or 'Cellular_component' Mention")

    val modifications = mkAssemblyModifications(e)

    // prepare id
    // if mods have been provided, a new id should be created since createSimpleEvent calls this method
    // and the current representation could be an output of a SimpleEvent
    // for a sentence like "Ras is phosphorylated", the Mention for "Ras" should only point to the PTM-less form;
    // however, when createSimpleEvent calls this method to construct an output representation,
    // it gives it the PTMs to associate with this mention
    // TODO: should this use m or e?
    val id = if (mods.nonEmpty) createID else getOrCreateID(m)

    // only use if mods are nonEmpty
    // use resolved form
    val newEvidence = createEvidence(e)

    // prepare SimpleEntity
    val eer =
      new SimpleEntity(
        id,
        // TODO: decide whether or not we should use a richer representation for the grounding ID
        e.nsId,
        // modifications relevant to assembly
        if (mods.isDefined) modifications ++ mods.get else modifications,
        // source mention
        // FIXME: not sure if newEvidence should be stored...
        if (mods.isEmpty) Some(m) else Some(newEvidence),
        this
      )
    // Only update table 1 if no additional mods were provided
    if (mods.isEmpty) updateLUTs(id, m, eer) else  updateIDtoEERTable(id, eer) // updateLUTs(id, newEvidence, repr)

    // eer and id pair
    (eer, id)
  }

  /**
    * Create a [[SimpleEntity]] representation from a Mention
    * and an optional set of optional modifications (useful for building output of SimpleEvent)
    *
    * Whenever modifications are provided, the [[mentionStateToID]] LUT is NOT updated, so as to avoid a conflict with the existing mapping (see the description of mods for the motivation)
    * @param m an Odin Mention
    * @param mods an optional set of [[AssemblyModification]].
    *             This is useful for building the output of a [[SimpleEvent]] (any simple event other than a Binding), which is a set of [[SimpleEvent]] where the key [[PTM]] comes from the [[SimpleEvent]]
    *             (i.e., the PTM cannot be recovered by simply examining m out of context)
    * @return a [[SimpleEntity]]
    */
  protected def createSimpleEntity(
    m: Mention,
    mods: Option[Set[AssemblyModification]]
  ): SimpleEntity = createSimpleEntityWithID(m, mods)._1


  //
  // Complex creation
  //

  /**
    * Creates a [[Complex]] from a Binding Mention and updates the [[mentionStateToID]] and [[idToEER]] LUTs
    * @param m an Odin Mention
    * @return a tuple of ([[Complex]], [[IDPointer]])
    */
  protected def createComplexWithID(m: Mention): (Complex, IDPointer) = {

    // check for coref
    val c = getResolvedForm(m)

    require(c matches "Complex", "createComplex only handles Mentions with the label 'Complex'.")

    // prepare id
    val id = getOrCreateID(m)

    // prepare Complex
    // TODO: do binding events have sites?

    val themes = getAllThemes(c)

    val mbrs: Set[IDPointer] = themes.map(m => createSimpleEntityWithID(m, None)).map(_._2).toSet
    val eer =
      new Complex(
        id,
        mbrs,
        Some(m),
        this
      )

    // update LUTs
    // use original mention for later lookup
    updateLUTs(id, m, eer)

    (eer, id)
  }

  /**
    * Creates a [[Complex]] from a Binding Mention and updates the [[mentionStateToID]] and [[idToEER]] LUTs
    * @param m an Odin Mention
    * @return a [[Complex]]
    */
  private def createComplex(m: Mention): Complex = createComplexWithID(m)._1

  //
  // SimpleEvent creation
  //

  /**
    * Creates a [[SimpleEvent]] from a Simple Event Mention (excludes Bindings) and updates the [[mentionStateToID]] and [[idToEER]] LUTs
    * @param m an Odin Mention
    * @return a tuple of ([[SimpleEvent]], [[IDPointer]])
    */
  private def createSimpleEventWithID(m: Mention): (SimpleEvent, IDPointer) = {

    //
    // helper functions for label-based dispatch
    //

    /**
      * Creates a [[SimpleEvent]] from a Binding Mention and updates the [[mentionStateToID]] and [[idToEER]] LUTs
      * @param m an Odin Mention
      * @return a tuple of ([[SimpleEvent]], [[IDPointer]])
      */
    def handleBinding(m: Mention): (SimpleEvent, IDPointer) = {

      // check for coref
      val e = getResolvedForm(m)

      // mention should be a Binding
      require(e matches "Binding", "handleBinding only accepts Binding mentions.")
      // there should not be a cause among the arguments
      require(!(e.arguments contains "cause"), "Binding should not contain a cause!")
      // prepare input (roles -> repr. pointers)

      // construct inputs from themes
      // TODO: how to handle sites?
      val correctedThemes = e.arguments
        .filter(_._1.toLowerCase.startsWith("theme"))
        .values
        .flatten
        .toSeq
      val themeMap: Map[String, Seq[Mention]] = Map("theme" -> correctedThemes)
      val input: Map[String, Set[IDPointer]] = themeMap map {
        case (role: String, mns: Seq[Mention]) =>
          (role, mns.map(getOrCreateEERwithID).map(_._2).toSet)
      }

      // prepare output
      val complexMembers: Set[IDPointer] = correctedThemes
        .map(m => createSimpleEntityWithID(m, None))
        .map(_._2)
        .toSet

      // prepare id for SimpleEvent
      val id = getOrCreateID(m)

      //prepare id for output (Complex)
      // prepare id
      val complexPointer = createID

      // prepare Complex
      val complex =
        new Complex(
          complexPointer,
          complexMembers,
          None,
          this
        )

      // update table #2
      updateIDtoEERTable(complexPointer, complex)

      // prepare SimpleEvent
      // TODO: throw exception if arguments contains "cause"
      val eer =
        new SimpleEvent(
          id,
          input,
          Set(complexPointer),
          e.label,
          Some(m),
          this
        )

      // update LUTs
      // use original mention for later lookup
      updateLUTs(id, m, eer)

      (eer, id)
    }

    /**
      * Creates a [[SimpleEvent]] from a Translocation Mention and updates the [[mentionStateToID]] and [[idToEER]] LUTs
      * @param m an Odin Mention
      * @return a tuple of ([[SimpleEvent]], [[IDPointer]])
      */
    def handleTranslocation(m: Mention): (SimpleEvent, IDPointer) = {

      /**
        * Create input Map for Translocation event
        */
      def createInputForTranslocation(m: Mention): Map[String, Set[IDPointer]] = {

        val isNegated = hasNegation(m)

        // handle source Location
        val src = "source"
        val mods: Set[AssemblyModification] = m match {
          case hasSource if hasSource.arguments contains src =>
            // create a PTM for each source
            for (
              src <- hasSource.arguments(src).toSet[Mention]
            ) yield {
              val gid = src.toBioMention.nsId
              representations.Location(gid).asInstanceOf[AssemblyModification]
            }
          // no mods
          case _ => Set.empty[AssemblyModification]
        }

        val correctedThemes = m.arguments
          .filter(_._1.toLowerCase.startsWith("theme"))
          .values
          .flatten
          .toSeq
        val themeMap: Map[String, Seq[Mention]] = Map("theme" -> correctedThemes)
        val input: Map[String, Set[IDPointer]] = themeMap map {
          case ("theme", mns: Seq[Mention]) =>
            ("theme", mns.map(m => createSimpleEntityWithID(m, Some(mods))).map(_._2).toSet)
        }
        input
      }

      /**
        * Create output Set for addition event
        */
      def createOutputForTranslocation(m: Mention): Set[IDPointer] = {

        val isNegated = hasNegation(m)

        // handle dest Location
        val dest = "destination"
        val mods: Set[AssemblyModification] = m match {
          case hasSource if hasSource.arguments contains dest =>
            // create a PTM for each source
            for (
              d <- hasSource.arguments(dest).toSet[Mention]
            ) yield {
              val gid = d.toBioMention.nsId
              representations.Location(gid)
            }
          // no mods
          case _ => Set.empty[AssemblyModification]
        }

        // NOTE: we need to be careful if we use something other than theme
        m.arguments("theme")
          .map(m => createSimpleEntityWithID(m, Some(mods))).map(_._2)
          .toSet
      }

      // check for coref
      val e = getResolvedForm(m)

      // only accept Translocation mention
      require(e matches "Translocation", s"handleTranslocation received Mention of label '${e.label}', but method only accepts a Translocation Mention.")
      // prepare input (roles -> repr. pointers)

      // create input
      val input = createInputForTranslocation(e)

      // prepare output
      val output: Set[IDPointer] = createOutputForTranslocation(e)

      // prepare id
      val id = getOrCreateID(m)

      // prepare SimpleEvent
      val eer =
        new SimpleEvent(
          id,
          input,
          output,
          e.label,
          Some(m),
          this
        )

      // update LUTs
      // use original mention for later lookup
      updateLUTs(id, m, eer)

      (eer, id)
    }

    /**
      * Creates a [[SimpleEvent]] from a SimpleEvent Mention (excluding Bindings) and updates the [[mentionStateToID]] and [[idToEER]] LUTs
      * @param m an Odin Mention
      * @return a tuple of ([[SimpleEvent]], [[IDPointer]])
      */
    def handleNBSimpleEvent(m: Mention): (SimpleEvent, IDPointer) = {

      /**
        * Create input Map for removal event
        */
      def createInputForRemovalEvent(m: Mention): Map[String, Set[IDPointer]] = {

        val isNegated = hasNegation(m)

        // get input PTM for removal event
        val ptm: String = m.label.replaceAll("^De", "").capitalize

        // handle sites
        val ptms: Set[AssemblyModification] = m match {
          case hasSites if hasSites.arguments contains "site" =>
            // create a PTM for each site
            for (site <- hasSites.arguments("site").toSet[Mention]) yield representations.PTM(ptm, Some(site.text), isNegated)
          // create a PTM without a site
          case noSites => Set(representations.PTM(ptm, None, isNegated))
        }

        // filter out sites from input
        val siteLessArgs = m.arguments - "site"
        val input: Map[String, Set[IDPointer]] = siteLessArgs map {
          case ("theme", mns: Seq[Mention]) =>
            ("theme", mns.map(m => createSimpleEntityWithID(m, Some(ptms))).map(_._2).toSet)
          case (role: String, mns: Seq[Mention]) =>
            (role, mns.map(getOrCreateEERwithID).map(_._2).toSet)
        }
        input
      }

      /**
        * Create input Map for addition event
        */
      def createInputForAdditionEvent(m: Mention): Map[String, Set[IDPointer]] = {
        // filter out sites from input
        val siteLessArgs = m.arguments - "site"
        val input: Map[String, Set[IDPointer]] = siteLessArgs map {
          case (role: String, mns: Seq[Mention]) =>
            //println(s"\tprocessing mentions for '$role' role of '${e.label}'")
            (role, mns.map(getOrCreateEERwithID).map(_._2).toSet)
        }
        input
      }

      /**
        * Create output Set for addition event
        */
      def createOutputForRemovalEvent(m: Mention): Set[IDPointer] = {
        // NOTE: we need to be careful if we use something other than theme
        m.arguments("theme")
          .map(getOrCreateEERwithID).map(_._2)
          .toSet
      }

      /**
        * Create output Set for addition event
        */
      def createOutputForAdditionEvent(m: Mention): Set[IDPointer] = {

        val isNegated = hasNegation(m)

        // handle sites
        val ptms: Set[AssemblyModification] = m match {
          case hasSites if hasSites.arguments contains "site" =>
            // create a PTM for each site
            for (site <- hasSites.arguments("site").toSet[Mention]) yield representations.PTM(m.label, Some(site.text), isNegated)
          // create a PTM without a site
          case noSites => Set(representations.PTM(m.label, None, isNegated))
        }

        // NOTE: we need to be careful if we use something other than theme
        m.arguments("theme")
          .map(m => createSimpleEntityWithID(m, Some(ptms))).map(_._2)
          .toSet
      }

      // check for coref
      val e = getResolvedForm(m)

      // mention should be a SimpleEvent, but not a Binding
      require((e matches "SimpleEvent") && !(e matches "Binding"), s"handleNBSimpleEvent received Mention of label '${e.label}', but method only accepts a SimpleEvent Mention that is NOT a Binding.")
      // prepare input (roles -> repr. pointers)

      // create input
      val input: Map[String, Set[IDPointer]] = e match {
        case removalEvent if removalEvent matches "RemovalEvent" =>
          createInputForRemovalEvent(removalEvent)
        case additionEvent if additionEvent matches "AdditionEvent" =>
          createInputForAdditionEvent(additionEvent)
        // other (assume addition event -like behavior)
        case other =>
          logger.debug(s"Treating mention with label '${other.label}' as an AdditionEvent")
          createInputForAdditionEvent(other)
      }

      // prepare output
      val output: Set[IDPointer] = e match {
        case removalEvent if removalEvent matches "RemovalEvent" =>
          createOutputForRemovalEvent(removalEvent)
        case additionEvent if additionEvent matches "AdditionEvent" =>
          createOutputForAdditionEvent(additionEvent)
        // other (assume addition event -like behavior)
        case other =>
          createOutputForAdditionEvent(other)
      }

      // prepare id
      val id = getOrCreateID(m)

      // prepare SimpleEvent
      val eer =
        new SimpleEvent(
          id,
          input,
          output,
          e.label,
          Some(m),
          this
        )

      // update LUTs
      // use original mention for later lookup
      updateLUTs(id, m, eer)

      (eer, id)
    }

    //
    // handle dispatch
    //

    val event = getResolvedForm(m.toCorefMention)
    require(event matches "SimpleEvent", s"createSimpleEventWithID requires Mention with the label SimpleEvent, but received Mention with label '${event.label}'")
    // there should not be a cause among the arguments
    require(!(event.arguments contains "cause"), "SimpleEvent should not contain a cause!")
    // SimpleEvent must have theme
    require(event.arguments contains "theme", s"'${event.label}' must have a theme.")
    m match {
      case binding if binding matches "Binding" => handleBinding(binding)
      case translocation if translocation matches "Translocation" => handleTranslocation(translocation)
      case other => handleNBSimpleEvent(other)
    }
  }

  /**
    * Creates a [[SimpleEvent]] from a Simple Event Mention (excludes Bindings) and updates the [[mentionStateToID]] and [[idToEER]] LUTs
    * @param m an Odin Mention
    * @return a [[SimpleEvent]]
    */
  private def createSimpleEvent(m: Mention): SimpleEvent = createSimpleEventWithID(m)._1

  //
  // Regulation creation
  //

  /**
    * Creates a [[Regulation]] from a Regulation Mention and updates the [[mentionStateToID]] and [[idToEER]] LUTs
    * @param m an Odin Mention
    * @return a tuple of ([[Regulation]], [[IDPointer]])
    */
  private def createRegulationWithID(m: Mention): (Regulation, IDPointer) = {

    // check for coref
    val reg = getResolvedForm(m)

    // get polarity
    val polarity = getPolarityLabel(reg)

    // mention should be a Regulation
    require(reg matches "Regulation", "createRegulation only handles Regulations")
    // mention's polarity should be either positive or negative
    require(polarity == AssemblyManager.positive || polarity == AssemblyManager.negative, "Polarity of Regulation must be positive or negative")
    // all controlled args must be simple events
    require(reg.arguments("controlled").forall(_ matches "Event"), "The 'controlled' of any Regulation must be an Event")

    val controllers: Set[IDPointer] = {
      reg.arguments("controller")
        .toSet[Mention]
        .map(c => getOrCreateEERwithID(c)._2)
    }

    val controlleds: Set[IDPointer] = {
      reg.arguments("controlled")
        .toSet[Mention]
        .map(c => getOrCreateEERwithID(c)._2)
    }

    // prepare id
    val id = getOrCreateID(m)

    // prepare Regulation

    val eer =
      new Regulation(
        id,
        controllers,
        controlleds,
        polarity,
        Some(m),
        this
      )

    // update LUTs
    // use original mention for later lookup
    updateLUTs(id, m, eer)

    // eer and id pair
    (eer, id)
  }

  /**
    * Creates a [[Regulation]] from a Regulation Mention and updates the [[mentionStateToID]] and [[idToEER]] LUTs
    * @param m an Odin Mention
    * @return a [[Regulation]]
    */
  private def createRegulation(m: Mention): Regulation = createRegulationWithID(m)._1

  /**
    * Creates a [[Activation]] from an Activation Mention and updates the [[mentionStateToID]] and [[idToEER]] LUTs
    * @param m an Odin Mention
    * @return a [[Activation]]
    */
  private def createActivation(m: Mention): Activation = createActivationWithID(m)._1

  //
  // Regulation creation
  //

  /**
    * Creates a [[Activation]] from an Activation Mention and updates the [[mentionStateToID]] and [[idToEER]] LUTs
    * @param m an Odin Mention
    * @return a tuple of ([[Activation]], [[IDPointer]])
    */
  private def createActivationWithID(m: Mention): (Activation, IDPointer) = {

    // check for coref
    val act = getResolvedForm(m)

    // get polarity
    val polarity = getPolarityLabel(act)

    // mention should be a Activation
    require(act matches "ActivationEvent", "createActivation only handles Activations")
    // mention's polarity should be either positive or negative
    require(polarity == AssemblyManager.positive || polarity == AssemblyManager.negative, "Polarity of ComplexEvent must be positive or negative")
    val controllers: Set[IDPointer] = {
      act.arguments("controller")
        .toSet[Mention]
        .map(c => getOrCreateEERwithID(c)._2)
    }

    val controlleds: Set[IDPointer] = {
      act.arguments("controlled")
        .toSet[Mention]
        .map(c => getOrCreateEERwithID(c)._2)
    }

    // prepare id
    val id = getOrCreateID(m)

    // prepare Regulation

    val eer =
      new Activation(
        id,
        controllers,
        controlleds,
        polarity,
        Some(m),
        this
      )

    // update LUTs
    // use original mention for later lookup
    updateLUTs(id, m, eer)

    // eer and id pair
    (eer, id)
  }

  //
  // EntityEventRepresentation creation
  //

  /**
    * Attempts to retrieve an [[EntityEventRepresentation]] for m.
    * If a representation cannot be retrieved, a new one is created.
    * Whenever a new representation is created,
    * the [[mentionStateToID]] and [[idToEER]] LUTs will be updated (see [[createEER]] for details)
    * @param m an Odin Mention
    * @return the [[EntityEventRepresentation]] corresponding to m
    */
  private def getOrCreateEER(m: Mention): EER = {
    // ensure this mention should be stored in LUT 1
    require(isValidMention(m), s"mention with the label ${m.label} cannot be tracked by the AssemblyManager")
    hasMention(m) match {
      // if an ID already exists, retrieve the associated representation
      case true =>
        val id = mentionStateToID(getMentionState(m))
        idToEER(id)
      // create new representation
      case false => createEER(m)
    }
  }

  /**
    * Attempts to retrieve a ([[EntityEventRepresentation]], [[IDPointer]]) tuple given a Mention m.
    * The tuple will be created if the Mention m is not already present in the [[mentionStateToID]] LUT
    * @param m an Odin Mention
    * @return a tuple of ([[EntityEventRepresentation]], [[IDPointer]])
    */
  private def getOrCreateEERwithID(m: Mention): (EER, IDPointer) = hasMention(m) match {
    case true =>
      val id = mentionStateToID(getMentionState(m))
      val eer = getEER(id)
      (eer, id)
    case false =>
      val eer = createEER(m)
      val id = eer.uniqueID
      (eer, id)
  }

  /**
    * Creates a ([[EntityEventRepresentation]], [[IDPointer]]) tuple from a Mention m.
    * Assumes the Mention m is not already present in the [[mentionStateToID]] LUT
    * Updates to [[mentionStateToID]] and [[idToEER]] in the relevant create* call
    * @param m an Odin Mention
    * @return a tuple of ([[EntityEventRepresentation]], [[IDPointer]])
    */
  private def createEERwithID(m: Mention): (EER, IDPointer) = {
    // pass the unresolved form through according to a check against the resolved form
    getResolvedForm(m) match {
      case complex if complex matches "Complex" => createComplexWithID(m)
      case e if e matches "Entity" => createSimpleEntityWithID(m, None)
      case cc if cc matches "Cellular_component" => createSimpleEntityWithID(m, None)
      case se if se matches "SimpleEvent" => createSimpleEventWithID(m)
      case regulation if regulation matches "Regulation" => createRegulationWithID(m)
      case activation if activation matches "ActivationEvent" => createActivationWithID(m)
      case other => throw new Exception(s"createEERwithID failed for ${other.label}")
    }
  }

  /**
    * Creates an ([[EntityEventRepresentation]], [[IDPointer]]) tuple given a Mention m.
    * The tuple will be created if the Mention m is not already present in the [[mentionStateToID]] LUT
    * @param m an Odin Mention
    * @return an [[EntityEventRepresentation]]
    */
  private def createEER(m: Mention): EER = createEERwithID(m)._1

  //
  // Utils for summarization
  //

  /**
    * A (mostly) human readable printout of the (key, value) pairs in the [[mentionStateToID]]] LUT
    */
  def mentionIndexSummary: Seq[String] = {
    for {
      (k, id) <- mentionStateToID.toSeq
    } yield s"${mentionSummary(k._1)} => $id"
  }

  //
  // Utilities for component retrieval
  //

  /**
    * Retrieves all tracked Mentions from [[AssemblyManager.mentionStateToID]]
    * @return the Set of Odin Mentions tracked by this AssemblyManager
    */
  def trackedMentions: Set[Mention] = mentionStateToID.keys.map(_._1).toSet

  /**
    * Retrieves all EntityEventRepresentations found in [[AssemblyManager.idToEER]]
    * @return the Set of Odin Mentions tracked by this AssemblyManager
    */
  def EERs: Set[EER] = idToEER.values.toSet

  /**
    * Retrieves ID from an [[EntityEventRepresentation.uniqueID]]
    * @param eer an EntityEventRepresentation
    * @return the IDPointer for the repr
    */
  def getID(eer: EER): IDPointer = eer.uniqueID

  /**
    * Retrieves an [[EntityEventRepresentation]] for a Mention.
    * Assumes an [[EntityEventRepresentation]] for the given Mention already exists.
    * @param m an Odin Mention
    * @return an [[EntityEventRepresentation]]
    */
  def getEER(m: Mention): EER = getOrCreateEER(m)

  /**
    * Retrieves an [[EntityEventRepresentation]] associated with the given [[IDPointer]].
    * Assumes an [[EntityEventRepresentation]] associated with the provide [[IDPointer]] already exists.
    * @param id an [[IDPointer]]
    * @return an [[EntityEventRepresentation]]
    */
  def getEER(id: IDPointer): EER = idToEER(id)

  /**
    * Retrieves the Set of [[EntityEventRepresentation]] tracked by the manager.
    * @return Set[EntityEventRepresentation]
    */
  def getEERs: Set[EER] = idToEER.values.toSet

  /**
    * Returns an Entity for a Mention with the appropriate labels.
    * @param m an Odin Mention.  Must have the label "Entity"
    */
  def getEntity(m: Mention): Entity = {
    require(m matches "Entity", "Mention is not an Entity")
    getOrCreateEER(m).asInstanceOf[Entity]
  }

  /**
    * Returns a SimpleEntity for a Mention with the appropriate labels.
    * @param m an Odin Mention.  Must have the label "Entity" and not the label "Complex".
    */
  def getSimpleEntity(m: Mention): SimpleEntity = {
    require(m matches "Entity", "Mention is not an Entity")
    require(! (m matches "Complex"), "Mention is a Complex")
    getOrCreateEER(m).asInstanceOf[SimpleEntity]
  }

  /**
    * Returns an Event for a Mention with the appropriate labels.
    * @param m an Odin Mention.
    */
  // TODO: add label check?
  def getEvent(m: Mention): Event = getOrCreateEER(m).asInstanceOf[Event]

  /**
    * Returns a Regulation for a Mention m with the appropriate label.
    * @param m an Odin Mention.  Must have the label "Complex".
    */
  def getComplex(m: Mention): Complex = {
    require(m matches "Complex", "Mention is not a Complex")
    getOrCreateEER(m).asInstanceOf[Complex]
  }

  /**
    * Returns a SimpleEvent for a Mention m with the appropriate labels.
    * @param m an Odin Mention.  Must have the label "SimpleEvent".
    */
  def getSimpleEvent(m: Mention): SimpleEvent = {
    require(m matches "SimpleEvent", "Mention is not a SimpleEvent")
    getOrCreateEER(m).asInstanceOf[SimpleEvent]
  }

  /**
    * Returns a Regulation for a Mention m with the label "Regulation"
    * @param m an Odin Mention.  Must have the label "Regulation"
    */
  def getRegulation(m: Mention): Regulation = {
    require(m matches "Regulation", "Mention is not a Regulation")
    getOrCreateEER(m).asInstanceOf[Regulation]
  }

  /**
    * Collects mentions pointing to a given [[EntityEventRepresentation]].
    * @param eer an [[EntityEventRepresentation]]
    * @return a sequence of Mention serving as textual evidence of the given representation
    */
  def getEvidence(eer: EER): Set[Mention] = {
    val equivEERs: Set[EER] = ehToEERs.getOrElse(eer.equivalenceHash(ignoreMods = false), Set.empty[EER])
    // retrieve the mention by id
    val evidence = for {
      equivEER <- equivEERs
      id = equivEER.uniqueID
      // check is needed, because output of a SimpleEvent has no Mention
      if idToMentionState contains id
      (e, mods) = idToMentionState(id)
    } yield e
    evidence
  }

  //
  // Grouping utilities
  //

  /**
    * Get non-distinct equivalent EERs matching the provided equivalenceHash (eh)
    * @param eh an [[EntityEventRepresentation.equivalenceHash]]
    * @return
    */
  def getEquivalentEERs(eh: Int): Set[EER] = ehToEERs.getOrElse(eh, Set.empty[EER])
  def getEquivalentEERs(eer: EER, ignoreMods: Boolean): Set[EER] = ehToEERs.getOrElse(eer.equivalenceHash(ignoreMods), Set.empty[EER])

  /**
    * Returns groups of equivalent [[EntityEventRepresentation]], ignoring differences due to [[IDPointer]] references.
    *
    * Mentions may point to (essentially) the same [[EntityEventRepresentation]], which would only differ in terms of the [[IDPointer]], which link an [[EntityEventRepresentation]] to a particular Mention
    */
  def groupedEERs: Seq[Set[EER]] = ehToEERs.values.toSeq

  /**
    * Gets distinct members of eers after grouping by [[EntityEventRepresentation.equivalenceHash]].
    * @param eers an [[EntityEventRepresentation]] Set
    * @param ignoreMods whether or not to ignore modifications when determining the distinct set of EERs
    * @return
    */
  def distinctEERsFromSet(eers: Set[EER], ignoreMods: Boolean): Set[EER] = {
    eers.groupBy(_.equivalenceHash(ignoreMods))
      .mapValues(_.head)
      .values
      .toSet
  }

  /**
    * Returns head of each group returned by [[groupedEERs]].
    *
    * @return a Set of [[EntityEventRepresentation]]
    */
  def distinctEERs: Set[EER] = {
    groupedEERs.map(_.head)
      .toSet
  }

  /**
    * Returns Set of "distinct" [[EntityEventRepresentation]] with corresponding evidence.
    */
  def distinctEERsWithEvidence: Set[(EER, Set[Mention])] = {
    distinctEERs.map(eer => (eer, getEvidence(eer)))
  }

  // Entity

  /**
    * Retrieves all Entities from the manager.
    * Note that these are non-distinct.
    */
  def getEntities: Set[Entity] = {
    for {
      e: EER <- getEERs
      if e.isInstanceOf[Entity]
      entity = e.asInstanceOf[Entity]
    } yield entity
  }

  /**
    * Returns "distinct" Set of Entities. Ignores multiple instances of the same Entity.
    * @return a Set of Entity
    */
  def distinctEntities: Set[Entity] = {
    for {
      e: EER <- distinctEERs
      if e.isInstanceOf[Entity]
      entity = e.asInstanceOf[Entity]
    } yield entity
  }

  /**
    * Returns "distinct" Set of Entities and all evidence (Set[Mention]) corresponding to each [[Entity]].
    * @return Set[(Entity, Set[Mention])]
    */
  def distinctEntitiesWithEvidence: Set[(Entity, Set[Mention])] = {
    distinctEntities
      .map( entity => (entity, getEvidence(entity)))
  }

  // Event

  /**
    * Retrieves all Events from the manager.
    * Note that these are non-distinct (Events may differ in terms of their IDPointers).
    */
  def getEvents: Set[Event] = {
    for {
      e: EER <- getEERs
      if e.isInstanceOf[Event]
      event = e.asInstanceOf[Event]
    } yield event
  }

  /**
    * Returns "distinct" Set of Events. Ignores multiple instances of the same Entity.
    * @return a Set of Event
    */
  def distinctEvents: Set[Event] = {
    for {
      e: EER <- distinctEERs
      if e.isInstanceOf[Event]
      event = e.asInstanceOf[Event]
    } yield event
  }

  /**
    * Returns "distinct" Set of Events and all evidence (Set[Mention]) corresponding to each [[Event]].
    * @return Set[(SimpleEntity, Set[Mention])]
    */
  def distinctEventsWithEvidence: Set[(Event, Set[Mention])] = {
    distinctEvents
      .map( event => (event, getEvidence(event)))
  }

  // SimpleEntity

  /**
    * Retrieves all SimpleEntities from the manager.
    * Note that these are non-distinct.
    */
  def getSimpleEntities: Set[SimpleEntity] = {
    for {
      e: EER <- getEERs
      if e.isInstanceOf[SimpleEntity]
      entity = e.asInstanceOf[SimpleEntity]
    } yield entity
  }

  /**
    * Retrieves all SimpleEntities containing the given
    * @param mod an [[AssemblyModification]]
    * @return a Set of SimpleEntities sharing mod
    */
  def getSimpleEntitiesByModification[M <: AssemblyModification](mod: M): Set[SimpleEntity] = {
    for {
      se <- getSimpleEntities
      if se.modifications contains mod
    } yield se
  }

  /**
    * Returns "distinct" Set of SimpleEntity. Ignores multiple instances of the same SimpleEntity.
    * @return a Set of SimpleEntity
    */
  def distinctSimpleEntities: Set[SimpleEntity] = {
    for {
      e: EER <- distinctEERs
      if e.isInstanceOf[SimpleEntity]
      entity = e.asInstanceOf[SimpleEntity]
    } yield entity
  }

  /**
    * Returns "distinct" Set of SimpleEntities and all evidence (Set[Mention]) corresponding to each [[SimpleEntity]].
    * @return Set[(SimpleEntity, Set[Mention])]
    */
  def distinctSimpleEntitiesWithEvidence: Set[(SimpleEntity, Set[Mention])] = {
    distinctSimpleEntities
      .map( entity => (entity, getEvidence(entity)))
  }

  // Complex

  /**
    * Retrieves all Complexes from the manager.
    * Note that these are non-distinct (Complexes may differ in terms of their IDPointers).
    */
  def getComplexes: Set[Complex] = {
    for {
      e: EER <- getEERs
      if e.isInstanceOf[Complex]
      complex = e.asInstanceOf[Complex]
    } yield complex
  }

  /**
    * Returns "distinct" Set of Complexes. Ignores differences in IDPointers.
    * @return a Set of Complexes
    */
  def distinctComplexes: Set[Complex] = {
    for {
      e: EER <- distinctEERs
      if e.isInstanceOf[Complex]
      complex = e.asInstanceOf[Complex]
    } yield complex
  }

  /**
    * Returns "distinct" Set of Complexes and all evidence (Set[Mention]) corresponding to each Complex.
    * @return Set[(Complex, Set[Mention])]
    */
  def distinctComplexesWithEvidence: Set[(Complex, Set[Mention])] = {
    distinctComplexes
      .map( comp => (comp, getEvidence(comp)))
  }

  // SimpleEvents

  /**
    * Retrieves all SimpleEvents from the manager.
    * Note that these are non-distinct (SimpleEvents may differ in terms of their IDPointers).
    */
  def getSimpleEvents: Set[SimpleEvent] = {
    for {
      e: EER <- getEERs
      if e.isInstanceOf[SimpleEvent]
      se = e.asInstanceOf[SimpleEvent]
    } yield se
  }

  /**
    * Retrieves all SimpleEvents from the manager matching the provided event label.
    * Note that these are non-distinct (SimpleEvents may differ in terms of their IDPointers).
    * @param label a String to match against each [[SimpleEvent.label]]
    */
  def getSimpleEvents(label: String): Set[SimpleEvent] = {
    for {
      e: EER <- getEERs
      if e.isInstanceOf[SimpleEvent]
      se = e.asInstanceOf[SimpleEvent]
      if se.label == label
    } yield se
  }

  /**
    * Returns "distinct" Set of SimpleEvents. Ignores differences in IDPointers.
    * @return a Set of SimpleEvents
    */
  def distinctSimpleEvents: Set[SimpleEvent] = {
    for {
      e: EER <- distinctEERs
      if e.isInstanceOf[SimpleEvent]
      se = e.asInstanceOf[SimpleEvent]
    } yield se
  }

  /**
    * Returns "distinct" Set of SimpleEvents matching the provided event label. Ignores differences in IDPointers.
    * @param label a String to match against each [[SimpleEvent.label]]
    * @return a Set of SimpleEvents
    */
  def distinctSimpleEvents(label: String): Set[SimpleEvent] = {
    for {
      e: EER <- distinctEERs
      if e.isInstanceOf[SimpleEvent]
      se = e.asInstanceOf[SimpleEvent]
      if se.label == label
    } yield se
  }

  /**
    * Returns "distinct" Set of SimpleEvent matching the provided event label and all evidence (Set[Mention]) corresponding to each SimpleEvent.
    * @param label a String to match against each [[SimpleEvent.label]]
    * @return Set[(SimpleEvent, Set[Mention])]
    */
  def distinctSimpleEventsWithEvidence(label: String): Set[(SimpleEvent, Set[Mention])] = {
    distinctSimpleEvents(label)
      .map( se => (se, getEvidence(se)))
  }

  // Regulations

  /**
    * Retrieves all Regulations from the manager.
    * Note that these are non-distinct (Regulations may differ in terms of their IDPointers).
    */
  def getRegulations: Set[Regulation] = {
    for {
      e: EER <- getEERs
      if e.isInstanceOf[Regulation]
      reg = e.asInstanceOf[Regulation]
    } yield reg
  }

  /**
    * Retrieves all Regulations from the manager matching the provided polarity label.
    * Note that these are non-distinct (Regulations may differ in terms of their IDPointers).
    * @param polarity a String to match against each [[Regulation.polarity]]
    */
  def getRegulations(polarity: String): Set[Regulation] = {
    for {
      e: EER <- getEERs
      if e.isInstanceOf[Regulation]
      reg = e.asInstanceOf[Regulation]
      if reg.polarity == polarity
    } yield reg
  }

  /**
    * Returns "distinct" Set of Regulation. Ignores differences in IDPointers.
    * @return a Set of Regulation
    */
  def distinctRegulations: Set[Regulation] = {
    for {
      e: EER <- distinctEERs
      if e.isInstanceOf[Regulation]
      reg = e.asInstanceOf[Regulation]
    } yield reg
  }

  /**
    * Returns "distinct" Set of Regulations matching the provided polarity. Ignores differences in IDPointers.
    * @param polarity a String to match against each [[Regulation.polarity]]
    * @return a Set of Regulations
    */
  def distinctRegulations(polarity: String): Set[Regulation] = {
    for {
      e: EER <- distinctEERs
      if e.isInstanceOf[Regulation]
      reg = e.asInstanceOf[Regulation]
      if reg.polarity == polarity
    } yield reg
  }

  /**
    * Returns "distinct" Set of Regulations and all evidence (Set[Mention]) corresponding to each Regulation.
    * @return Set[(Regulation, Set[Mention])]
    */
  def distinctRegulationsWithEvidence: Set[(Regulation, Set[Mention])] = {
    distinctRegulations
      .map( reg => (reg, getEvidence(reg)))
  }

  /**
    * Returns "distinct" Set of Regulations matching the provided polarity and all evidence (Set[Mention]) corresponding to each Regulation.
    * @param polarity a String to match against each [[Regulation.polarity]]
    * @return Set[(Regulation, Set[Mention])]
    */
  def distinctRegulationsWithEvidence(polarity: String): Set[(Regulation, Set[Mention])] = {
    distinctRegulations(polarity)
      .map( reg => (reg, getEvidence(reg)))
  }

  // Activations

  /**
    * Retrieves all Activations from the manager.
    * Note that these are non-distinct (Activations may differ in terms of their IDPointers).
    */
  def getActivations: Set[Activation] = {
    for {
      e: EER <- getEERs
      if e.isInstanceOf[Activation]
      act = e.asInstanceOf[Activation]
    } yield act
  }

  /**
    * Retrieves all Activations from the manager matching the provided polarity label.
    * Note that these are non-distinct (Activations may differ in terms of their IDPointers).
    * @param polarity a String to match against each [[Activation.polarity]]
    */
  def getActivations(polarity: String): Set[Activation] = {
    for {
      e: EER <- getEERs
      if e.isInstanceOf[Activation]
      act = e.asInstanceOf[Activation]
      if act.polarity == polarity
    } yield act
  }

  /**
    * Returns "distinct" Set of Activations. Ignores differences in IDPointers.
    * @return a Set of Regulation
    */
  def distinctActivations: Set[Activation] = {
    for {
      e: EER <- distinctEERs
      if e.isInstanceOf[Activation]
      act = e.asInstanceOf[Activation]
    } yield act
  }

  /**
    * Returns "distinct" Set of Activations matching the provided polarity. Ignores differences in IDPointers.
    * @param polarity a String to match against each [[Activation.polarity]]
    * @return a Set of Activations
    */
  def distinctActivations(polarity: String): Set[Activation] = {
    for {
      e: EER <- distinctEERs
      if e.isInstanceOf[Activation]
      act = e.asInstanceOf[Activation]
      if act.polarity == polarity
    } yield act
  }

  /**
    * Returns "distinct" Set of Activations and all evidence (Set[Mention]) corresponding to each Activation.
    * @return Set[(Regulation, Set[Mention])]
    */
  def distinctActivationsWithEvidence: Set[(Regulation, Set[Mention])] = {
    distinctRegulations
      .map( reg => (reg, getEvidence(reg)))
  }

  /**
    * Returns "distinct" Set of Activations matching the provided polarity and all evidence (Set[Mention]) corresponding to each Activation.
    * @param polarity a String to match against each [[Activation.polarity]]
    * @return Set[(Activation, Set[Mention])]
    */
  def distinctActivationsWithEvidence(polarity: String): Set[(Activation, Set[Mention])] = {
    distinctActivations(polarity)
      .map( act => (act, getEvidence(act)))
  }

  //
  // summary utilities
  //

  /**
    * A high-level summary of a Mention m
    * @param m an Odin Mention
    * @return a high-level String representation of m
    */
  def mentionSummary(m: Mention): String = {
    val bio = m.toBioMention
    val docRepr = s"DOC:${m.document.id.get} (sent. ${m.sentence})"
    s"Mention(label=${m.label}, text='${m.text}', modifications=${bio.modifications}, doc=$docRepr)"
  }

  def summarizeMentionIndex: Unit = println(mentionIndexSummary.sorted.mkString("\n"))

  def summarizeEntities: Unit = println(getSimpleEntities.map(_.summarize).toSeq.sorted.mkString("\n"))


  //
  // LUT utils
  //

  def hasMention(m: Mention): Boolean = mentionStateToID contains getMentionState(m)

  //
  // Set diff
  //

  def EERdiff(eers1: Set[EER], eers2: Set[EER], ignoreMods: Boolean): Set[EER] = {
    eers1.filterNot(eer => eers2.exists(_.isEquivalentTo(eer, ignoreMods)))
  }

  def EERintersection(eers1: Set[EER], eers2: Set[EER], ignoreMods: Boolean): Set[EER] = {
    eers1.filter(eer => eers2.exists(_.isEquivalentTo(eer, ignoreMods)))
  }

  //
  // Serialization methods
  //

  def saveTo(f: File): Unit = saveTo(f.getAbsolutePath)

  def saveTo(fileName: String): Unit = {
    org.clulab.utils.Serializer.save[AssemblyManager](this, fileName)
  }
}

object AssemblyManager {
  val positive = "Positive"
  val negative = "Negative"
  val unknown = "UNKNOWN"

  val DEFAULT_CONFIDENCE = 1.0

  def apply(): AssemblyManager = new AssemblyManager(Map.empty[MentionState, IDPointer], Map.empty[IDPointer, EER])

  /**
    * Instantiate [[AssemblyManager]] and track the provided Mentions
    * @param mns a sequence of Odin Mentions
    * @return an [[AssemblyManager]]
    */
  def apply(mns: Seq[Mention]): AssemblyManager = {
    val am = new AssemblyManager(Map.empty[MentionState, IDPointer], Map.empty[IDPointer, EER])
    am.trackMentions(mns)
    am
  }

  def loadFrom(f: File): AssemblyManager = loadFrom(f.getAbsolutePath)

  def loadFrom(fileName: String): AssemblyManager = {
    org.clulab.utils.Serializer.load[AssemblyManager](fileName)
  }

  /**
    * Get antecedent if present.  Otherwise return the CorefMntion as-is. <br>
    *
    * Used to retrieve the appropriate features of a mention's antecedent.
    * @param m an Odin Mention
    * @return a [[org.clulab.reach.mentions.CorefMention]] (possibly cm)
    */
  def getResolvedForm(m: Mention): CorefMention = {
    val cm = m.toCorefMention
    cm.antecedentOrElse(cm)
  }

  /**
    * Checks whether a mention involves a corefence resolution
    * @param m an Odin Mention
    * @return
    */
  def involvesCoreference(m: Mention): Boolean = getResolvedForm(m) match {
    // if the resolved form differs from m, this is a case of coref
    case resolved if resolved != m => true
    // ... otherwise check if any arg involves coref
    case checkArgs => checkArgs.arguments.values.flatten.exists(involvesCoreference)
  }

  /**
    * Checks to see if the mention can be safely handled by the AssemblyManager
    * Currently Sites are not stored in the LUTs,
    * though they can appear as part of a modification
    * (see the [[PTM]] [[AssemblyModification]] for an example)
    * @param mention an Odin Mention
    * @return true if the mention can be safely handled by the manager; false otherwise
    */
  def isValidMention(mention: Mention): Boolean = {

    getResolvedForm(mention) match {

      // no generic event
      case gen if gen matches "Generic_event" => false
      // allow entities
      case entity if entity matches "Entity" => true
      // needed for Translocations
      case cc if cc matches "Cellular_component" => true
      // simple events must have a theme and should not have a cause
      case se if se matches "SimpleEvent" =>
        (se.arguments contains "theme") && !(se.arguments contains "cause")

      // activations must have controlled and controller
      case act if act matches "ActivationEvent" =>
        (act.arguments contains "controller") &&
          (act.arguments contains "controlled") &&
          // controllers must be Entities
          act.arguments("controller").forall {
            case entity if entity matches "Entity" => true
            case _ => false
          } &&
          // make sure all controlleds are valid
          act.arguments("controlled").forall(isValidMention)

      // regs must have controlled and controller
      case reg if reg matches "Regulation" =>
        (reg.arguments contains "controller") &&
          (reg.arguments contains "controlled") &&
          // controlled must be an Event (or Complex), but not an Activation
          reg.arguments("controlled").forall {
            // controlled cannot be an entity UNLESS it is a Complex
            case complex if complex matches "Complex" => true
            case entity if entity matches "Entity" => false
            case event if event matches "Event" => isValidMention(event)
          }

      // assume invalid otherwise
      case _ => false
    }
  }

  /**
    * Retrieves all themes from a Mention
    * @param m an Odin Mention
    * @return a Seq[Mention] produced by a flattening of all values corresponding to theme* keys
    */
  def getAllThemes(m: Mention): Seq[Mention] = {
    m.arguments
      .filter(_._1.toLowerCase.startsWith("theme"))
      .values
      .flatten
      .toSeq
  }

  /**
    * Check if mention is negated
    */
  def hasNegation(m: Mention): Boolean = m.toBioMention.modifications exists {
    case mentions.Negation(_) => true
    case _ => false
  }

  /**
    * Retrieve the set of input entities from an eer
    * @param eer an [[EntityEventRepresentation]]
    * @return a set of Entity-type EERs
    */
  def getInputEntities(eer: EntityEventRepresentation): Set[Entity] = eer match {
    case complex: Complex =>
      // a complex could contain another complex, so flatten
      // until members are all simple entities
      // then cast each as Entity for uniformity
      complex.flattenMembers.map(_.asInstanceOf[Entity])
    case entity: Entity => Set(entity)
    case simpleEvent: SimpleEvent => simpleEvent.I
    case complexEvent: ComplexEvent => complexEvent.I
  }
}