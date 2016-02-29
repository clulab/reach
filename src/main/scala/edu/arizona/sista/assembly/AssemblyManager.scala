package edu.arizona.sista.assembly

import collection.Map
import collection.mutable
import collection.immutable
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions.{MentionOps, BioMention}
// used to differentiate AssemblyModifications from Modifications on mentions
import edu.arizona.sista.reach.mentions
import edu.arizona.sista.assembly

abstract class AssemblyManager {

  val mentionToID: Map[Mention, IDPointer]
  val idToEERepresentation: Map[IDPointer, EntityEventRepresentation]
  // reverse dict of ID to Mention
  val idToMention: Map[IDPointer, Mention] = mentionToID.map{ case (k, v) => (v, k)}

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
}

// immutable assembly manager
class ImmutableAssemblyManager(
  val mentionToID: immutable.Map[Mention, IDPointer],
  val idToEERepresentation: immutable.Map[IDPointer, EntityEventRepresentation]
) extends AssemblyManager {

  def toMutable: MutableAssemblyManager = {
    // convert immutable LUTs to a mutable form
    val mngr =
      new MutableAssemblyManager(
        mutable.Map(mentionToID.toSeq: _*),
        mutable.Map(idToEERepresentation.toSeq: _*)
      )
    // TODO: shouldn't EERepresentations now point to the new MutableAssemblyManager?
    mngr
  }
}

class MutableAssemblyManager(
    val mentionToID: mutable.Map[Mention, IDPointer] = mutable.Map.empty[Mention, IDPointer],
    val idToEERepresentation: mutable.Map[IDPointer, EntityEventRepresentation] = mutable.Map.empty[IDPointer, EntityEventRepresentation]
) extends AssemblyManager {

  def toImmutable: ImmutableAssemblyManager =
    // convert mutable LUTs to an immutable form
    // TODO: shouldn't EERepresentations now point to this new ImmutableAssemblyManager?
    new ImmutableAssemblyManager(
      immutable.Map(mentionToID.toSeq: _*),
      immutable.Map(idToEERepresentation.toSeq: _*)
    )

  def createID: IDPointer = mentionToID.size + 1

  def getOrCreateID(m: Mention): IDPointer = mentionToID.getOrElse(m, createID)

  def getOrCreateEERepresentation(m: Mention): EntityEventRepresentation = {
    mentionToID.getOrElse(m, None) match {
      // if an ID already exists, retrieve the associated representation
      case id: IDPointer => idToEERepresentation(id)
      case None =>
        // get an ID for the current mention
        val id: IDPointer = getOrCreateID(m)
        // update LUT 1
        mentionToID(m) = id
        // create new representation
        val repr = createEERepresentation(m)
        // update LUT 2
        idToEERepresentation(id) = repr
        repr
    }
  }

  def updateLUTs(id: IDPointer, m: Mention, repr: EntityEventRepresentation): Unit = {
    // update LUT #1
    mentionToID(m) = id
    // update LUT #2
    idToEERepresentation(id) = repr
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

  protected def createSimpleEntity(m: Mention, ptm: Option[assembly.PTM]): (IDPointer, SimpleEntity) = m.toBioMention match {
    // TODO: refactor to avoid two cases?
    // TODO: add require() for labels?
    case ge if ge matches "Generic_entity" =>
      val ante = ge.toCorefMention.antecedent
      val e = if (ante.nonEmpty) ante.get.asInstanceOf[BioMention] else ge
      val id = getOrCreateID(e)
      val mods = mkAssemblyModifications(e)
      val repr =
        new SimpleEntity(
          // TODO: decide whether or not we should use a richer representation for the grounding ID
          e.nsId,
          // modifications relevant to assembly
          if (ptm.isEmpty) mods else mods ++ Set(ptm.get),
          true,
          // TODO: confirm that this is a pointer back to the current object
          // ...what happens when .toImmutable is called?
          this
        )
      // update LUTs
      // TODO: should we store the corefmention, or the antecedent mention?
      updateLUTs(id, ge, repr)
      // id and repr pair
      (id, repr)

    case e if (e matches "Entity") && !(e matches "Generic_entity") =>
      val id = getOrCreateID(e)
      val mods = mkAssemblyModifications(e)
      val repr =
        new SimpleEntity(
          // TODO: decide whether or not we should use a richer representation for the grounding ID
          e.nsId,
          // modifications relevant to assembly
          if (ptm.isEmpty) mods else mods ++ Set(ptm.get),
          false,
          // TODO: confirm that this is a pointer back to the current object
          // ...what happens when .toImmutable is called?
          this
        )
      // update LUTs
      updateLUTs(id, e, repr)
      // id and repr pair
      (id, repr)
  }

  protected def createBinding(m: Mention): (IDPointer, Complex) = m.toBioMention match {
    // TODO: refactor to avoid two cases?
    // TODO: add require() for labels?
    case gb if (gb matches "Binding") && (gb matches "Generic_event") =>
      val ante = gb.toCorefMention.antecedent
      val b = if (ante.nonEmpty) ante.get.asInstanceOf[BioMention] else gb
      val id = getOrCreateID(b)
      val mbrs: Set[IDPointer] = b.arguments("theme").map(m => createSimpleEntity(m, None)).map(_._1).toSet
      val repr =
        new Complex(
          mbrs,
          true,
          // TODO: confirm that this is a pointer back to the current object
          // ...what happens when .toImmutable is called?
          this
        )
      // update LUTs
      // TODO: should we store the coref mention, or the antecedent mention?
      updateLUTs(id, gb, repr)
      (id, repr)

    case b if (b matches "Binding") && !(b matches "Generic_event") =>
      val id = getOrCreateID(b)
      val mbrs: Set[IDPointer] = b.arguments("theme").map(m => createSimpleEntity(m, None)).map(_._1).toSet
      val repr =
        new Complex(
          mbrs,
          false,
          // TODO: confirm that this is a pointer back to the current object
          // ...what happens when .toImmutable is called?
          this
        )
      // update LUTs
      updateLUTs(id, b, repr)
      (id, repr)
  }

  protected def createSimpleEvent(m: Mention): (IDPointer, SimpleEvent) = m.toBioMention match {
    // TODO: refactor to avoid two cases?
    // TODO: add require() for labels?
    case ge if (ge matches "SimpleEvent") && !(ge matches "Binding") && (ge matches "Generic_event") =>
      val ante = ge.toCorefMention.antecedent
      val e = if (ante.nonEmpty) ante.get.asInstanceOf[BioMention] else ge
      val id = getOrCreateID(e)
      val input: Map[String, Set[IDPointer]] = e.arguments.map{
        case (role: String, mns: Seq[Mention]) =>
          (role, mns.map(createIDwithEERepresentation).map(_._1).toSet)
      }
      val output: Set[IDPointer] = {
        // TODO: handle site
        val ptm = assembly.PTM(e.label, None)
        e.arguments("theme")
          .map(m => createSimpleEntity(m, Some(ptm))).map(_._1)
          .toSet
      }
      val repr =
        new SimpleEvent(
          input,
          output,
          e.label,
          true,
          // TODO: confirm that this is a pointer back to the current object
          // ...what happens when .toImmutable is called?
          this
        )
      // update LUTs
      // TODO: should we store the corefmention, or the antecedent mention?
      updateLUTs(id, ge, repr)
      (id, repr)

    case e if (e matches "SimpleEvent") && !(e matches "Binding") && !(e matches "Generic_event") =>
      val id = getOrCreateID(e)
      val input: Map[String, Set[IDPointer]] = e.arguments.map{
        case (role: String, mns: Seq[Mention]) =>
          (role, mns.map(createIDwithEERepresentation).map(_._1).toSet)
      }
      val output: Set[IDPointer] = {
        // TODO: handle site
        val ptm = assembly.PTM(e.label, None)
        e.arguments("theme")
          // for each theme, create a simple entity with the appropriate PTM
          // and get its ID
          .map(m => createSimpleEntity(m, Some(ptm))).map(_._1)
          .toSet
      }
      val repr =
        new SimpleEvent(
          input,
          output,
          e.label,
          true,
          // TODO: confirm that this is a pointer back to the current object
          // ...what happens when .toImmutable is called?
          this
        )
      // update LUTs
      // TODO: should we store the coref mention, or the antecedent mention?
      updateLUTs(id, e, repr)
      (id, repr)
  }

  //    def createRegulation(m: Mention): (IDPointer, Regulation) = m.toBioMention match {
  //      // val controlled = reg.arguments("controlled").map(createEERepresentation)
  //      // controller: Option[EntityEventRepresentation]
  //      // controlled: Set[SimpleEvent]
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

object AssemblyManager {

}
