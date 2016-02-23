package edu.arizona.sista.reach.grounding

import edu.arizona.sista.reach.grounding.ReachIMKBMentionLookups._

/**
  * Object implementing logic to enumerate context related KB entries.
  *   Written by Tom Hicks. 2/19/2016.
  *   Last Modified: Add cell component KBs as context KBs.
  */
object ReachContextKBLister {
  /** A sequence of the context related KB instances, whose values are to be listed. */
  val ContextKBs = Seq( ContextCellLine, ContextCellType, ContextOrgan, ContextSpecies,
                        StaticCellLocation, StaticCellLocation2, ModelGendCellLocation )

  /** Return a sequence of grounding information objects from the context related KBs. */
  def listContextKBs: Seq[ContextGrounding] = {
    ContextKBs.map(kb => kb.entries).flatten.map(kbe =>
      ContextGrounding(kbe.text, kbe.key, kbe.namespace, kbe.id, kbe.nsId, kbe.species))
  }

  /** Case class to hold grounding information about context related KB entries. */
  case class ContextGrounding(text:String, key:String, namespace:String, id:String,
                              nsId:String, species:String)

}
