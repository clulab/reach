package edu.arizona.sista.reach.grounding

import edu.arizona.sista.reach.grounding.ReachIMKBMentionLookups._

/**
  * Object implementing logic to enumerate context related KB entries.
  *   Written by Tom Hicks. 2/19/2016.
  *   Last Modified: Initial creation.
  */
object ReachContextKBLister {
  /** A sequence of the context related KB instances, whose values are to be listed. */
  val ContextKBs = Seq( (ContextCellLine, "CellLine"), (ContextCellType, "CellType"), (ContextOrgan, "Organ"), (ContextSpecies, "Species") )

  /** Return a sequence of grounding information objects from the context related KBs. */
  def listContextKBs: Seq[ContextGrounding] = {
    ContextKBs.flatMap{ case (kb, ctxType) => kb.entries map (kbe => ContextGrounding(ctxType, kbe.text, kbe.key, kbe.namespace, kbe.id, kbe.nsId, kbe.species))}
  }

  /** Case class to hold grounding information about context related KB entries. */
  case class ContextGrounding(ctxType:String, text:String, key:String, namespace:String, id:String,
                              nsId:String, species:String)

}
