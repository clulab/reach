package edu.arizona.sista.assembly

import collection.Map
import collection.immutable
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions.{MentionOps, CorefMention}
// used to differentiate AssemblyModifications from Modifications on mentions
import edu.arizona.sista.reach.mentions
import edu.arizona.sista.assembly


class AssemblyManager(
  m2id: Map[Mention, IDPointer] = Map.empty[Mention, IDPointer],
  id2repr: Map[IDPointer, EntityEventRepresentation] = Map.empty[IDPointer, EntityEventRepresentation]
) {

  var mentionToID: immutable.Map[Mention, IDPointer] = m2id.toMap
  var idToEERepresentation: immutable.Map[IDPointer, EntityEventRepresentation] = id2repr.toMap
  def idToMention: Map[IDPointer, Mention] = mentionToID.map{ case (k, v) => (v, k)}

  def getEERepresentation(m: Mention): EntityEventRepresentation = {
    val id = mentionToID(m)
    idToEERepresentation(id)
  }

  def getEERepresentation(id: IDPointer): EntityEventRepresentation =
    idToEERepresentation(id)

  def getEvidence(repr: EntityEventRepresentation): Set[Mention] = {
    idToEERepresentation.filter {
      // which IDs point to EEReprs that are identical to the one given?
      case (k, v) => v == repr }
      .keys
      // retrieve the mention by id
      .map(id => idToMention(id))
      .toSet
  }

  /**
   * Creates an [[EntityEventRepresentation]] for each valid Mention
   * See [[isValidMention]] for details on validation check
   * @param mentions a sequence of Mention to store in the AssemblyManager LUTs
   */
  // create an EntityEventRepresentation for each mention
  def trackMentions(mentions: Seq[Mention]): Unit = {
    // do not store Sites, Activations, etc. in LUT 1
    mentions.filter(isValidMention)
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

  def getOrCreateID(m: Mention): IDPointer = mentionToID.getOrElse(m, createID)

  def getOrCreateEERepresentation(m: Mention): EntityEventRepresentation = {
    mentionToID.getOrElse(m, None) match {
      // if an ID already exists, retrieve the associated representation
      case id: IDPointer => idToEERepresentation(id)
      case None =>
        // get an ID for the current mention
        val id: IDPointer = getOrCreateID(m)
        // create new representation
        val repr = createEERepresentation(m)
        // update LUTs
        updateLUTs(id, m, repr)

        repr
    }
  }

  def updateLUTs(id: IDPointer, m: Mention, repr: EntityEventRepresentation): Unit = {
    // update LUT #1
    mentionToID  = mentionToID + (m -> id)
    // update LUT #2
    idToEERepresentation = idToEERepresentation + (id -> repr)
  }

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
   * takes a set of optional modifications (useful for building output of SimpleEvent)
   * @param m an Odin mention
   * @param mods an optional set of AssemblyModifications (useful for building output of SimpleEvent)
   * @return a tuple of (IDPointer, SimpleEntity)
   */
  protected def createSimpleEntity(
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
        if (mods.isEmpty) modifications else modifications ++ mods.get,
        // TODO: ask Dane how best to check if this guy is resolved...
        cm.isGeneric,
        this
      )
    // update LUTs
    updateLUTs(id, e, repr)
    // id and repr pair
    (id, repr)
  }

  protected def createBinding(m: Mention): (IDPointer, Complex) = {

    // check for coref
    val cm = m.toCorefMention
    val b = getResolvedForm(cm)

    // mention must be a Binding
    require(b matches "Binding", "createBinding only handles Binding mentions.")

    // TODO: do binding events have sites?
    val mbrs: Set[IDPointer] = b.arguments("theme").map(m => createSimpleEntity(m, None)).map(_._1).toSet
    val repr =
      new Complex(
        mbrs,
        // check if coref was successful (i.e., it found something)
        hasCorefResolution(cm),
        this
      )
    // update LUTs
    updateLUTs(id, b, repr)
    (id, repr)
  }

  protected def createSimpleEvent(m: Mention): (IDPointer, SimpleEvent) = {

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
        (role, mns.map(createIDwithEERepresentation).map(_._1).toSet)
    }
    // prepare output
    val output: Set[IDPointer] = {
      // handle sites
      val ptms: Set[AssemblyModification] = e match {
        case hasSites if hasSites.arguments contains "site" =>
          // create a PTM for each site
          hasSites.arguments("site").toSet.map(site => assembly.PTM(e.label, Some(site.text)))
          // create a PTM without a site
        case noSites => Set(assembly.PTM(e.label, None))
      }

      // NOTE: we need to be careful if we use something other than theme
      e.arguments("theme")
        // TODO: should this be one PTM per entity?
        .map(m => createSimpleEntity(m, Some(ptms))).map(_._1)
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
        this
      )

    // prepare id
    val id = getOrCreateID(m)
    // update LUTs
    // TODO: should we store the coref mention, or the antecedent mention?
    updateLUTs(id, e, repr)
    (id, repr)
  }


  // dedup should cover some of Emek's stuff
  // 1. Approx. sieves (by proximity)
  //   - proximity is 1 or 2 sentences
  //   - entities without mutations
  //   - entities without sites (PTMs)
  //    def createRegulation(m: Mention): (IDPointer, Regulation) = m.toBioMention match {
  //      // val controlled = reg.arguments("controlled").map(createEERepresentation)
          // convert to PTM?????
          // TODO: binarize controllers!
  //      // controller: Entity
  //      // controlled: Event
  //    }

  /**
   * LUT updates happen internally
   * returns an (IDPointer, EntityEventRepresentation) tuple
   * */
  def createIDwithEERepresentation(m: Mention): (IDPointer, EntityEventRepresentation) = {

    m.toBioMention match {
      case e if e matches "Entity" => createSimpleEntity(e, None)
      case binding if binding matches "Binding" => createBinding(binding)
      case se if (se matches "SimpleEvent") && ! (se matches "Binding") => createSimpleEvent(m)
      //case regulation if regulation matches "Regulation" => createRegulation(regulation)
      // TODO: generic reg
      // TODO: reg
    }
  }

  /**
   * LUT updates happen internally
   * returns an EntityEventRepresentation
   * */
  def createEERepresentation(m: Mention): EntityEventRepresentation = createIDwithEERepresentation(m)._2
}
