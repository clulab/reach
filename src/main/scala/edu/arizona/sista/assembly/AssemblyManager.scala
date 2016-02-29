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

  def createID: IDPointer = mentionToID.size + 1

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

    val cm = m.toCorefMention
    require(cm matches "Entity")
    // check for coref
    val ante = cm.antecedent
    val e = if (ante.nonEmpty) ante.get.asInstanceOf[Mention].toCorefMention else cm
    // prepare id
    val id = getOrCreateID(e)
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

    val cm = m.toCorefMention
    require(cm matches "Binding", "createBinding only handles Binding mentions.")
    // check for coref
    val ante = cm.antecedent
    val b = if (ante.nonEmpty) ante.get.asInstanceOf[Mention].toCorefMention else cm
    // prepare id
    val id = getOrCreateID(b)
    // TODO: do binding events have sites?
    val mbrs: Set[IDPointer] = b.arguments("theme").map(m => createSimpleEntity(m, None)).map(_._1).toSet
    val repr =
      new Complex(
        mbrs,
        // TODO: ask Dane how best to check if this guy is resolved...
        cm.isGeneric,
        this
      )
    // update LUTs
    updateLUTs(id, b, repr)
    (id, repr)
  }

  protected def createSimpleEvent(m: Mention): (IDPointer, SimpleEvent) = {

    val cm = m.toCorefMention
    require((cm matches "SimpleEvent") && !(cm matches "Binding"))
    // check for coref
    val ante = cm.antecedent
    val e = if (ante.nonEmpty) ante.get.asInstanceOf[Mention].toCorefMention else cm
    // prepare id
    val id = getOrCreateID(e)
    // prepare input (roles -> repr. pointers)
    // TODO: filter out sites from input
    val input: Map[String, Set[IDPointer]] = e.arguments.map{
      case (role: String, mns: Seq[Mention]) =>
        (role, mns.map(createIDwithEERepresentation).map(_._1).toSet)
    }
    // prepare output
    val output: Set[IDPointer] = {
      // TODO: handle site
      val ptm = assembly.PTM(e.label, None)
      // NOTE: we need to be careful if we use something other than theme
      e.arguments("theme")
        .map(m => createSimpleEntity(m, Some(ptm))).map(_._1)
        .toSet
    }
    // TODO: throw exception if arguments contains "cause"
    val repr =
      new SimpleEvent(
        input,
        output,
        e.label,
        // TODO: ask Dane how best to check if this guy is resolved...
        cm.isGeneric,
        // TODO: confirm that this is a pointer back to the current object
        // ...what happens when .toImmutable is called?
        this
      )
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
