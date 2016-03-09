package edu.arizona.sista.assembly

import collection.Map
import collection.immutable
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions.{Negation, MentionOps, CorefMention}
// used to differentiate AssemblyModifications from Modifications on mentions
import edu.arizona.sista.reach.mentions
import edu.arizona.sista.assembly


/**
 * @constructor Creates a new AssemblyManager from two LUTs: (Mention -> [[IDPointer]]) and ([[IDPointer]] -> [[EntityEventRepresentation]]).
 *             These LUTs are used to populate the mentionToID and idToEERepresentation LUTs containing the same information.
 *             Subsequent updates to these LUTs create new LUTs.
 *             The motivation for the LUTs was to allow for changes in the mapping of a Mention -...-> [[EntityEventRepresentation]]
 *             to easily propagate in nested cases.
 *             For example, an update to the (Mention -> [[EntityEventRepresentation]]) mapping of a SimpleEvent Mention will propagate
 *             to the (Mention -> [[EntityEventRepresentation]]) mapping of any ComplexEvent Mention containing this SimpleEvent Mention.
 * @param m2id a lookup table from Mention -> [[IDPointer]].  Each key (Mention) should map to a unique [[IDPointer]].
 * @param id2repr a lookup table from [[IDPointer]] -> [[EntityEventRepresentation]].
 *                Keys ([[IDPointer]]) may point to the same value (EntityEventRepresentation)
 */
class AssemblyManager(
  m2id: Map[Mention, IDPointer],
  id2repr: Map[IDPointer, EntityEventRepresentation]
) extends Serializable {

  var mentionToID: immutable.Map[Mention, IDPointer] = m2id.toMap
  var idToEERepresentation: immutable.Map[IDPointer, EntityEventRepresentation] = id2repr.toMap
  // TODO: this is potentially expensive...
  // Should probably be a var initialized to this, but where to do the updates?
  def idToMention: Map[IDPointer, Mention] = mentionToID.map{ case (k, v) => (v, k)}

  // initialize to size of LUT 2
  private var nextID: IDPointer = idToEERepresentation.size

  /**
   * Retrieves an [[EntityEventRepresentation]] for a Mention.
   * Assumes an [[EntityEventRepresentation]] for the given Mention already exists.
   * @param m an Odin Mention
   * @return an [[EntityEventRepresentation]]
   */
  def getEERepresentation(m: Mention): EntityEventRepresentation = {
    val id = mentionToID(m)
    idToEERepresentation(id)
  }

  /**
   * Retrieves an [[EntityEventRepresentation]] associated with the given [[IDPointer]].
   * Assumes an [[EntityEventRepresentation]] associated with the provide [[IDPointer]] already exists.
   * @param id an [[IDPointer]]
   * @return an [[EntityEventRepresentation]]
   */
  def getEERepresentation(id: IDPointer): EntityEventRepresentation =
    idToEERepresentation(id)

  /**
   * Retrieves the Set of [[EntityEventRepresentation]] tracked by the manager.
   * @return Set[EntityEventRepresentation]
   */
  def getEERepresentations: Set[EntityEventRepresentation] = idToEERepresentation.values.toSet

  /**
   * Returns a SimpleEntity for a Mention with the appropriate labels.
   * @param m an Odin Mention.  Must have the label "Entity" and not the label "Complex".
   */
  def getSimpleEntity(m: Mention): SimpleEntity = {
    require(m matches "Entity", "Mention is not an Entity")
    require(! (m matches "Complex"), "Mention is a Complex")
    getOrCreateEERepresentation(m).asInstanceOf[SimpleEntity]
  }

  /**
   * Returns a Regulation for a Mention m with the appropriate label.
   * @param m an Odin Mention.  Must have the label "Binding".
   */
  def getComplex(m: Mention): Complex = {
    require(m matches "Binding", "Mention is not a Binding")
    getOrCreateEERepresentation(m).asInstanceOf[Complex]
  }

  /**
   * Returns a SimpleEvent for a Mention m with the appropriate labels.
   * @param m an Odin Mention.  Must have the label "SimpleEvent" and not the label "Binding".
   */
  def getSimpleEvent(m: Mention): SimpleEvent = {
    require(m matches "SimpleEvent", "Mention is not a SimpleEvent")
    require(!(m matches "Binding"), "Mention is a Binding")
    getOrCreateEERepresentation(m).asInstanceOf[SimpleEvent]
  }

  /**
   * Returns a Regulation for a Mention m with the label "Regulation"
   * @param m an Odin Mention.  Must have the label "Regulation"
   */
  def getRegulation(m: Mention): Regulation = {
    require(m matches "Regulation", "Mention is not a Regulation")
    getOrCreateEERepresentation(m).asInstanceOf[Regulation]
  }

  /**
   * Collects mentions pointing to a given [[EntityEventRepresentation]].
   * @param repr an [[EntityEventRepresentation]]
   * @return a sequence of Mention serving as textual evidence of the given representation
   */
  def getEvidence(repr: EntityEventRepresentation): Set[Mention] = {
    idToEERepresentation.filter {
      // which IDs point to EEReprs that are identical to the one given?
      case (k, v) => v isEquivalentTo repr }
      .keys
      // retrieve the mention by id
      .map(id => idToMention(id))
      .toSet
  }

  /**
   * Creates an [[EntityEventRepresentation]] if m is a valid Mention
   * See [[isValidMention]] for details on validation check
   * @param m an Odin Mention
   */
  def trackMention(m: Mention): Unit = {
    // do not store Sites, Activations, etc. in LUT 1
    if (isValidMention(m)) getOrCreateEERepresentation(m)
  }

  /**
   * Creates an [[EntityEventRepresentation]] for each valid Mention
   * See [[isValidMention]] for details on validation check
   * @param mentions a sequence of Mention to store in the AssemblyManager LUTs
   */
  def trackMentions(mentions: Seq[Mention]): Unit = {
    // do not store Sites, Activations, etc. in LUT 1
    mentions.filter(isValidMention)
      // get or create an EntityEventRepresentation for each mention
      .map(getOrCreateEERepresentation)
  }

  /**
   * Checks to see if the mention can be safely handled by the AssemblyManager
   * Currently Sites are not stored in the LUTs,
   * though they can appear as part of a modification
   * (see the [[assembly.PTM]] [[AssemblyModification]] for an example)
   * @param mention an Odin Mention
   * @return true if the mention can be safely handled by the manager; false otherwise
   */
  def isValidMention(mention: Mention): Boolean = {

    val m = getResolvedForm(mention.toCorefMention)

    m match {
      // don't store sites
      case site if site matches "Site" => false
      // simple events should not have a cause
      case se if se matches "SimpleEvent" => !(se.arguments contains "cause")
      // don't store activations
      case activation if activation matches "Activation" => false
      // regs must have controlled and controller
      case reg if reg matches "Regulation" =>
        (m.arguments contains "controller") && (m.arguments contains "controlled")
      // assume valid otherwise
      case _ => true
    }
  }
  /**
   * Get antecedent if present.  Otherwise return the mention as-is.
   *
   * Used to retrieve the appropriate features of a mention's antecedent.
   * @param cm an [[edu.arizona.sista.reach.mentions.CorefMention]]
   * @return a [[edu.arizona.sista.reach.mentions.CorefMention]] (possibly cm)
   */
  def getResolvedForm(cm: CorefMention): CorefMention = {
    val ante = cm.antecedent
    if (ante.nonEmpty) ante.get.asInstanceOf[Mention].toCorefMention else cm
  }

  /**
   * Checks to see if a coref mention has an antecedent.
   *
   * If the mentions made it through the coref component of reach,
   * the only mentions that might have an antecedent should be those with a "Generic_*"
   * this is just a broader, fail-safe check...
   * @param cm an [[edu.arizona.sista.reach.mentions.CorefMention]]
   * @return true if cm has an antecedent; false otherwise
   */
  protected def hasCorefResolution(cm: CorefMention): Boolean = if (cm.antecedent.nonEmpty) true else false

  /**
   * Gets the polarity of a mention.  Should only be relevant to ComplexEvents
   * @param m an Odin Mention
   * @return [[AssemblyManager.positive]], [[AssemblyManager.negative]], or [[AssemblyManager.unknown]]
   */
  def getPolarityLabel(m: Mention): String = m match {
    case pos if pos matches "Positive_regulation" => AssemblyManager.positive
    case neg if neg matches "Negative_regulation" => AssemblyManager.negative
    case _ => AssemblyManager.unknown
  }

  /**
   * Creates a unique [[IDPointer]].
   * This implementation does not rely on updates to either the [[mentionToID]] or [[idToEERepresentation]] LUT to determine a unique [[IDPointer]].
   * @return a unique [[IDPointer]]
   */
  // use the size of LUT 2 to create a new ID
  def createID: IDPointer = {
    val currentID = nextID
    nextID += 1
    currentID
  }

  /**
   * Attempts to retrieve an [[IDPointer]] for a Mention, and creates a new [[IDPointer]] if none is found.
   * @param m an Odin Mention
   * @return an [[IDPointer]] unique to m
   */
  def getOrCreateID(m: Mention): IDPointer = mentionToID.getOrElse(m, createID)

  /**
   * Attempts to retrieve an [[EntityEventRepresentation]] for m.
   * If a representation cannot be retrieved, a new one is created.
   * Whenever a new representation is created,
   * the [[mentionToID]] and [[idToEERepresentation]] LUTs will be updated (see [[createEERepresentation]] for details)
   * @param m an Odin Mention
   * @return the [[EntityEventRepresentation]] corresponding to m
   */
  def getOrCreateEERepresentation(m: Mention): EntityEventRepresentation = {
    // ensure this mention should be stored in LUT 1
    require(isValidMention(m), s"mention with the label ${m.label} cannot be tracked by the AssemblyManager")

    mentionToID.getOrElse(m, None) match {
      // if an ID already exists, retrieve the associated representation
      case id: IDPointer => idToEERepresentation(id)
      // create new representation
      case None => createEERepresentation(m)
    }
  }

  /**
   * Updates the [[mentionToID]] and [[idToEERepresentation]] LUTs
   * @param id a unique [[IDPointer]] for m
   * @param m an Odin Mention
   * @param repr the [[EntityEventRepresentation]] corresponding to m
   */
  def updateLUTs(id: IDPointer, m: Mention, repr: EntityEventRepresentation): Unit = {
    // update LUT #1
    updateMentionToIDTable(m, id)
    // update LUT #2
    updateIdToEERepresentationTable(id, repr)
  }

  /**
   * Updates the [[mentionToID]] LUT
   * @param m an Odin Mention
   * @param id an [[IDPointer]] unique to m
   */
  private def updateMentionToIDTable(m: Mention, id: IDPointer): Unit = {
    mentionToID  = mentionToID + (m -> id)
  }

  /**
   * Updates the [[idToEERepresentation]] LUT
   * @param id a unique [[IDPointer]] pointing to repr
   * @param repr an [[EntityEventRepresentation]] associated with the provided id
   */
  private def updateIdToEERepresentationTable(id: IDPointer, repr: EntityEventRepresentation): Unit = {
    idToEERepresentation = idToEERepresentation + (id -> repr)
  }

  /**
   * Builds a Set[AssemblyModfication] from the modifcations belonging to a Mention m.
   * Currently, only a subset of Mention [[edu.arizona.sista.reach.mentions.Modification]] are considered relevant to assembly:
   * PTM
   * Mutant
   *
   * Additionally, a Mention corresponding to an Entity will include an [[assembly.EntityLabel]] [[AssemblyModification]] encoding its label (ex. Family)
   * @param m an Odin Mention
   * @return Set[AssemblyModification]
   */
  protected def mkAssemblyModifications(m: Mention): Set[AssemblyModification] = {
    // we only care to represent a subset of the Modifications associated with a mention
    val mods: Set[AssemblyModification] =
      m.toBioMention.modifications flatMap {
        // TODO: is site part of label?
        case mut: mentions.Mutant => Set(assembly.MutantEntity(mut.label))
        // TODO: should site be handled differently?
        case ptm: mentions.PTM => Set(assembly.PTM(ptm.toString, None))
        case _ => Nil
      }
    if (m matches "Entity") Set(EntityLabel(m.label)) ++ mods else mods
  }

  /**
   * Create a [[SimpleEntity]] representation from a Mention
   * and an optional set of optional modifications (useful for building output of SimpleEvent)
   *
   * Whenever modifications are provided, the [[mentionToID]] LUT is NOT updated, so as to avoid a conflict with the existing mapping (see the description of mods for the motivation)
   * @param m an Odin Mention
   * @param mods an optional set of [[AssemblyModification]].
   *             This is useful for building the output of a [[SimpleEvent]] (any simple event other than a Binding), which is a set of [[SimpleEvent]] where the key [[assembly.PTM]] comes from the [[SimpleEvent]]
   *             (i.e., the PTM cannot be recovered by simply examining m out of context)
   * @return a tuple of ([[IDPointer]], [[SimpleEntity]])
   */
  protected def createSimpleEntityWithID(
    m: Mention,
    mods: Option[Set[assembly.AssemblyModification]]
  ): (IDPointer, SimpleEntity) = {

    // check for coref
    val cm = m.toCorefMention
    val e = getResolvedForm(cm)

    // mention should be an Entity
    require(cm matches "Entity")

    val modifications = mkAssemblyModifications(e)
    val repr =
      new SimpleEntity(
        // TODO: decide whether or not we should use a richer representation for the grounding ID
        e.nsId,
        // modifications relevant to assembly
        if (mods.isDefined) modifications ++ mods.get else modifications,
        // check if coref was successful (i.e., it found something)
        hasCorefResolution(cm),
        // check if negated
        hasNegation(m),
        this
      )

    // prepare id
    // if mods have been provided, a new id should be created since createSimpleEvent calls this method
    // and the current representation could be an output of a SimpleEvent
    // for a sentence like "Ras is phosphorylated", the Mention for "Ras" should only point to the PTM-less form;
    // however, when createSimpleEvent calls this method to construct an output representation,
    // it gives it the PTMs to associate with this mention
    val id = if (mods.nonEmpty) createID else getOrCreateID(e)
    // Only update table 1 if no additional mods were provided
    if (mods.isEmpty) updateLUTs(id, m, repr) else updateIdToEERepresentationTable(id, repr)

    //println(s"ID for mention '${cm.text}' with label ${cm.label}${if (mods.nonEmpty) s" and mods ${mods.get}" else ""} is $id")
    // id and repr pair
    (id, repr)
  }

  /**
   * Create a [[SimpleEntity]] representation from a Mention
   * and an optional set of optional modifications (useful for building output of SimpleEvent)
   *
   * Whenever modifications are provided, the [[mentionToID]] LUT is NOT updated, so as to avoid a conflict with the existing mapping (see the description of mods for the motivation)
   * @param m an Odin Mention
   * @param mods an optional set of [[AssemblyModification]].
   *             This is useful for building the output of a [[SimpleEvent]] (any simple event other than a Binding), which is a set of [[SimpleEvent]] where the key [[assembly.PTM]] comes from the [[SimpleEvent]]
   *             (i.e., the PTM cannot be recovered by simply examining m out of context)
   * @return a [[SimpleEntity]]
   */
  protected def createSimpleEntity(
    m: Mention,
    mods: Option[Set[assembly.AssemblyModification]]
  ): SimpleEntity = createSimpleEntityWithID(m, mods)._2


  /**
   * Creates a [[Complex]] from a Binding Mention and updates the [[mentionToID]] and [[idToEERepresentation]] LUTs
   * @param m an Odin Mention
   * @return a tuple of ([[IDPointer]], [[Complex]])
   */
  protected def createComplexWithID(m: Mention): (IDPointer, Complex) = {

    // check for coref
    val cm = m.toCorefMention
    val b = getResolvedForm(cm)

    // mention must be a Binding
    //TODO: change name to createComplex
    require(b matches "Binding", "createComplex only handles Binding mentions.")
    //TODO: add require that says arguments only contains key "theme"
    // This way we will know if we're missing stuff
    // TODO: do binding events have sites?
    val mbrs: Set[IDPointer] = b.arguments("theme").map(m => createSimpleEntityWithID(m, None)).map(_._1).toSet
    val repr =
      new Complex(
        mbrs,
        // check if coref was successful (i.e., it found something)
        hasCorefResolution(cm),
        // check if negated
        hasNegation(m),
        this
      )

    // prepare id
    val id = getOrCreateID(m)
    // update LUTs
    updateLUTs(id, m, repr)

    //println(s"ID for binding mention '${cm.text}' is $id")
    (id, repr)
  }

  /**
   * Creates a [[Complex]] from a Binding Mention and updates the [[mentionToID]] and [[idToEERepresentation]] LUTs
   * @param m an Odin Mention
   * @return a [[Complex]]
   */
  protected def createComplexWIthID(m: Mention): Complex = createComplexWithID(m)._2

  /**
   * Creates a [[SimpleEvent]] from a Simple Event Mention (excludes Bindings) and updates the [[mentionToID]] and [[idToEERepresentation]] LUTs
   * @param m an Odin Mention
   * @return a tuple of ([[IDPointer]], [[SimpleEvent]])
   */
  protected def createSimpleEventWithID(m: Mention): (IDPointer, SimpleEvent) = {

    // check for coref
    val cm = m.toCorefMention
    val e = getResolvedForm(cm)

    // mention should be a SimpleEvent, but not a Binding
    require((cm matches "SimpleEvent") && !(cm matches "Binding"), "createSimpleEvent only accepts SimpleEvent mentions that are NOT Bindings.")
    // there should not be a cause among the arguments
    require(!(cm.arguments contains "cause"), "SimpleEvent should not contain a cause!")
    // prepare input (roles -> repr. pointers)

    // filter out sites from input
    val siteLessArgs = e.arguments - "site"
    val input: Map[String, Set[IDPointer]] = siteLessArgs map {
      case (role: String, mns: Seq[Mention]) =>
        (role, mns.map(getOrCreateEERepresentationWithID).map(_._1).toSet)
    }

    // prepare output
    val output: Set[IDPointer] = {
      // handle sites
      val ptms: Set[AssemblyModification] = e match {
        case hasSites if hasSites.arguments contains "site" =>
          // create a PTM for each site
          for (site <- hasSites.arguments("site").toSet[Mention]) yield assembly.PTM(e.label, Some(site.text))
          // create a PTM without a site
        case noSites => Set(assembly.PTM(e.label, None))
      }

      // NOTE: we need to be careful if we use something other than theme
      e.arguments("theme")
        // TODO: should this be one PTM per entity?
        .map(m => createSimpleEntityWithID(m, Some(ptms))).map(_._1)
        .toSet
    }

    // TODO: throw exception if arguments contains "cause"
    val repr =
      new SimpleEvent(
        input,
        output,
        e.label,
        // check if coref was successful (i.e., it found something)
        hasCorefResolution(cm),
        // check if negated
        hasNegation(m),
        this
      )

    // prepare id
    val id = getOrCreateID(m)
    // update LUTs
    updateLUTs(id, m, repr)

    //println(s"ID for ${cm.label} mention of SimpleEvent '${cm.text}' is $id")
    (id, repr)
  }

  /**
   * Creates a [[SimpleEvent]] from a Simple Event Mention (excludes Bindings) and updates the [[mentionToID]] and [[idToEERepresentation]] LUTs
   * @param m an Odin Mention
   * @return a [[SimpleEvent]]
   */
  protected def createSimpleEvent(m: Mention): SimpleEvent = createSimpleEventWithID(m)._2

  /**
   * Creates a [[Regulation]] from a Regulation Mention and updates the [[mentionToID]] and [[idToEERepresentation]] LUTs
   * @param m an Odin Mention
   * @return a tuple of ([[IDPointer]], [[Regulation]])
   */
  protected def createRegulationWithID(m: Mention): (IDPointer, Regulation) = {

    // check for coref
    val cm = m.toCorefMention
    val reg = getResolvedForm(cm)

    // get polarity
    val polarity = getPolarityLabel(reg)

    // mention should be a Regulation
    require(reg matches "Regulation", "createRegulation only handles Regulations")
    // mention's polarity should be either positive or negative
    require(polarity == AssemblyManager.positive || polarity == AssemblyManager.negative, "Polarity of Regulation must be positive or negative")
    // all controlled args must be simple events
    require(reg.arguments("controlled").forall(_ matches "SimpleEvent"), "The 'controlled' of any Regulation must be a SimpleEvent")

    val controllers: Set[IDPointer] = {
      reg.arguments("controller")
        .toSet[Mention]
        .map(c => getOrCreateEERepresentationWithID(c)._1)
    }

    val controlleds: Set[IDPointer] = {
      reg.arguments("controller")
        .toSet[Mention]
        .map(c => getOrCreateEERepresentationWithID(c)._1)
    }

    val repr =
      new Regulation(
        controllers,
        controlleds,
        polarity,
        // check if coref was successful (i.e., it found something)
        hasCorefResolution(cm),
        // check if negated
        hasNegation(m),
        this
      )

    // prepare id
    val id = getOrCreateID(m)
    // update LUTs
    updateLUTs(id, m, repr)

    //println(s"ID for mention '${cm.text}' with label ${cm.label} is $id")
    // id and repr pair
    (id, repr)
  }

  /**
   * Creates a [[Regulation]] from a Regulation Mention and updates the [[mentionToID]] and [[idToEERepresentation]] LUTs
   * @param m an Odin Mention
   * @return a [[Regulation]]
   */
  protected def createRegulation(m: Mention): Regulation = createRegulationWithID(m)._2

  /**
   * Creates a ([[IDPointer]], [[EntityEventRepresentation]]) tuple from a Mention m.
   * Assumes the Mention m is not already present in the [[mentionToID]] LUT
   * Updates to [[mentionToID]] and [[idToEERepresentation]] in the relevant create* call
   * @param m an Odin Mention
   * @return a tuple of ([[IDPointer]], [[EntityEventRepresentation]])
   */
  protected def createEERepresentationWithID(m: Mention): (IDPointer, EntityEventRepresentation) = {

    m.toBioMention match {
      case e if e matches "Entity" => createSimpleEntityWithID(e, None)
      case binding if binding matches "Binding" => createComplexWithID(binding)
      case se if (se matches "SimpleEvent") && ! (se matches "Binding") => createSimpleEventWithID(m)
      case regulation if regulation matches "Regulation" => createRegulationWithID(regulation)
    }
  }

  /**
   * Attempts to retrieve a ([[IDPointer]], [[EntityEventRepresentation]]) tuple given a Mention m.
   * The tuple will be created if the Mention m is not already present in the [[mentionToID]] LUT
   * @param m an Odin Mention
   * @return a tuple of ([[IDPointer]], [[EntityEventRepresentation]])
   */
  def getOrCreateEERepresentationWithID(m: Mention): (IDPointer, EntityEventRepresentation) = {
    val id = getOrCreateID(m)
    val repr = idToEERepresentation.getOrElse(id, createEERepresentation(m))
    (id, repr)
  }

  /**
   * Attempts to retrieve a ([[IDPointer]], [[EntityEventRepresentation]]) tuple given a Mention m.
   * The tuple will be created if the Mention m is not already present in the [[mentionToID]] LUT
   * @param m an Odin Mention
   * @return an [[EntityEventRepresentation]]
   */
  def createEERepresentation(m: Mention): EntityEventRepresentation = createEERepresentationWithID(m)._2

  /**
   * A (mostly) human readable printout of the (key, value) pairs in the [[mentionToID]]] LUT
   */
  def mentionIndexSummary(): Unit = {
    mentionToID.foreach{ pair =>
      val m = pair._1
      val id = pair._2
      println(s"${mentionSummary(m)} => $id")
    }
  }

  //
  // Grouping utilities
  //

  /**
   * Returns groups of equivalent [[EntityEventRepresentation]], ignoring differences due to [[IDPointer]] references.
   *
   * Mentions may point to (essentially) the same [[EntityEventRepresentation]], which would only differ in terms of the [[IDPointer]], which link an [[EntityEventRepresentation]] to a particular Mention
   */
  def groupEEReprs: Seq[Seq[EntityEventRepresentation]] = {
    idToEERepresentation.values
      // ignore IDPointers for grouping purposes
      .groupBy(_.equivalenceHash)
      .mapValues(_.toSeq)
      .values
      .toSeq
  }

  /**
   * Returns head of each group returned by [[groupEEReprs]].
   *
   * @return a Set of [[EntityEventRepresentation]]
   */
  def distinctEEReprs: Set[EntityEventRepresentation] = {
    groupEEReprs.map(_.head)
      .toSet
  }

  /**
   * Returns Set of "distinct" [[EntityEventRepresentation]] with corresponding evidence.
   */
  def distinctEEReprsWithEvidence: Set[(EntityEventRepresentation, Set[Mention])] = {
    distinctEEReprs.map(repr => (repr, getEvidence(repr)))
  }

  // SimpleEntity

  /**
   * Retrieves all SimpleEntities from the manager.
   * Note that these are non-distinct.
   */
  def getSimpleEntities: Set[SimpleEntity] = {
    for {
      e: EntityEventRepresentation <- getEERepresentations
      if e.isInstanceOf[SimpleEntity]
      entity = e.asInstanceOf[SimpleEntity]
    } yield entity
  }

  /**
   * Returns "distinct" Set of SimpleEntity. Ignores multiple instances of the same SimpleEntity.
   * @return a Set of SimpleEntity
   */
  def distinctSimpleEntities: Set[SimpleEntity] = {
    for {
      e: EntityEventRepresentation <- distinctEEReprs
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
      e: EntityEventRepresentation <- getEERepresentations
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
      e: EntityEventRepresentation <- distinctEEReprs
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
      e: EntityEventRepresentation <- getEERepresentations
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
      e: EntityEventRepresentation <- getEERepresentations
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
      e: EntityEventRepresentation <- distinctEEReprs
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
      e: EntityEventRepresentation <- distinctEEReprs
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
      e: EntityEventRepresentation <- getEERepresentations
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
      e: EntityEventRepresentation <- getEERepresentations
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
      e: EntityEventRepresentation <- distinctEEReprs
      if e.isInstanceOf[Regulation]
      reg = e.asInstanceOf[Regulation]
    } yield reg
  }

  /**
   * Returns "distinct" Set of Regulations matching the provided polarity. Ignores differences in IDPointers.
   * @param polarity
   * @return a Set of Regulations
   */
  def distinctRegulations(polarity: String): Set[Regulation] = {
    for {
      e: EntityEventRepresentation <- distinctEEReprs
      if e.isInstanceOf[Regulation]
      reg = e.asInstanceOf[Regulation]
      if reg.polarity == polarity
    } yield reg
  }

  /**
   * Returns "distinct" Set of Regulations and all evidence (Set[Mention]) corresponding to each Regulations.
   * @return Set[(Regulation, Set[Mention])]
   */
  def distinctRegulationsWithEvidence: Set[(Regulation, Set[Mention])] = {
    distinctRegulations
      .map( reg => (reg, getEvidence(reg)))
  }

  /**
   * Returns "distinct" Set of Regulations matching the provided polority and all evidence (Set[Mention]) corresponding to each Regulations.
   * @param polarity a String to match against each [[Regulation.polarity]]
   * @return Set[(Regulation, Set[Mention])]
   */
  def distinctRegulationsWithEvidence(polarity: String): Set[(Regulation, Set[Mention])] = {
    distinctRegulations(polarity)
      .map( reg => (reg, getEvidence(reg)))
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
    val docRepr = s"DOC:${m.document.id.get} (sent. ${m.sentence})"
    s"Mention(label=${m.label}, text='${m.text}', doc=$docRepr)"
  }


  //
  // evidence checks
  //

  /**
   * Checks whether evidence contains a Negation modification
   * @param m an Odin Mention
   * @return true or false
   */
  def hasNegation(m: Mention): Boolean = {
    // get mention's coref resolution
    val cm: CorefMention = m.toCorefMention
    val ante = cm.antecedentOrElse(cm)

    ante match {
      // does the entity have a Negation mod?
      case entity if ante matches "Entity" =>
        entity.modifications exists (_.isInstanceOf[Negation])
      // does the event have a Negation mod OR do any of its arguments have a Negation mod?
      case event if event matches "Event" =>
        (event.modifications exists (_.isInstanceOf[Negation])) || (event.arguments.values.flatten exists hasNegation)
      case _ => false
    }
  }
}

object AssemblyManager {
  val positive = "Positive"
  val negative = "Negative"
  val unknown = "UNKNOWN"
  def apply(): AssemblyManager = new AssemblyManager(Map.empty[Mention, IDPointer], Map.empty[IDPointer, EntityEventRepresentation])
}