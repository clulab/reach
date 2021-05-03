package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachIMKBMentionLookups._

/**
  * Object implementing logic to enumerate context related KB entries.
  *   Written by Tom Hicks. 2/19/2016.
  *   Last Modified: Update for hiding of KB entry class.
  */
object ReachContextKBLister {
  /** A sequence of the context related KB instances, whose values are to be listed. */

  val contextTypes = Seq("CellLine", "CellType", "Species", "TissueType", "Organ", "Cellular_component")

  /** Return a sequence of grounding information objects from the context related KBs. */
  def listContextKBs: Seq[ContextGrounding] =
      for{
        ctxType <- contextTypes
        if configuredKBML contains ctxType
        kb <- configuredKBML(ctxType)
        grounding <- kb.resolutions match {
            case Some(resolutions) =>
              resolutions map (
                kbr => ContextGrounding(ctxType, kbr.text, kbr.namespace, kbr.id, kbr.nsId, kbr.species))
            case None => Nil
          }
      } yield grounding




  /** Case class to hold grounding information about context related KB entries. */
  case class ContextGrounding(ctxType:String, text:String, namespace:String, id:String,
                              nsId:String, species:String)

}
