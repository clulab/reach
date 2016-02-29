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
  // reverse dict of id to mention
  val idToMention: Map[IDPointer, Mention] = mentionToID.map{ case (k, v) => (v, k)}

  def getEERepresentation(m: Mention): EntityEventRepresentation = {
    val id = mentionToID(m)
    idToEERepresentation(id)
  }

  def getEERepresentation(id: IDPointer): EntityEventRepresentation =
    idToEERepresentation(id)

  def getEvidence(repr: EntityEventRepresentation): Set[Mention] = {
    idToEERepresentation.filter {
      // which ids point to EEReprs that are identical to the one given?
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
    // TODO: convert EERepresentations to point to the new MutableAssemblyManager?
    mngr
  }
}

class MutableAssemblyManager(
    val mentionToID: mutable.Map[Mention, IDPointer] = mutable.Map.empty[Mention, IDPointer],
    val idToEERepresentation: mutable.Map[IDPointer, EntityEventRepresentation] = mutable.Map.empty[IDPointer, EntityEventRepresentation]
) extends AssemblyManager {

  def toImmutable: ImmutableAssemblyManager =
    // convert mutable LUTs to an immutable form
    // TODO: should EEReprs point to this new ImmutableAssemblyManager?
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

  // TODO: this one is a lot of work
  // must update LUT 1 each time
  def createEERepresentation(m: Mention): EntityEventRepresentation = {

    def mkAssemblyModifications(m: Mention): Set[AssemblyModification] = {
      m.toBioMention.modifications flatMap {
        // TODO: is site part of label?
        case mut: mentions.Mutant => Set(MutantEntity(mut.label))
        // TODO: should site be handled differently?
        case ptm: mentions.PTM => Set(assembly.PTM(ptm.toString, None))
        case _ => Nil
      }
    }

    m.toBioMention match {
      // a generic entity
      case ge if ge matches "Generic_entity" =>
        val ante = ge.toCorefMention.antecedent
        val e = if (ante.nonEmpty) ante.get.asInstanceOf[BioMention] else ge
        val id = getOrCreateID(e)
        val repr =
          new SimpleEntity(
            // TODO: decide whether or not we should use a richer representation for the grounding ID
            e.nsId,
            // modifications relevant to assembly
            Set(EntityLabel(e.label)) ++ mkAssemblyModifications(e),
            true,
            // TODO: confirm that this is a pointer back to the current object
            // ...what happens when .toImmutable is called?
            this
          )
        // update LUTs
        updateLUTs(id, ge, repr)
        repr
      // because of fall-through, this cannot be a Generic_entity
      case e if (e matches "Entity") && !(e matches "Generic_entity") =>
        val id = getOrCreateID(e)
        val repr =
          new SimpleEntity(
            // TODO: decide whether or not we should use a richer representation for the grounding ID
            e.nsId,
            // modifications relevant to assembly
            Set(EntityLabel(e.label)) ++ mkAssemblyModifications(e),
            false,
            // TODO: confirm that this is a pointer back to the current object
            // ...what happens when .toImmutable is called?
            this
          )
        // update LUTs
        updateLUTs(id, e, repr)
        repr
      // TODO: generic simple event
      // TODO: simple event

    }
  }
}

object AssemblyManager {

}
