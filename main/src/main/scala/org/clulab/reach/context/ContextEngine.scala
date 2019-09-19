package org.clulab.reach.context

import com.typesafe.scalalogging.LazyLogging
import org.clulab.reach.mentions._
import org.clulab.reach.grounding.ReachContextKBLister


trait ContextEngine {

  /** initializes any data structure that needs to be initialized */
  def infer(mentions: Seq[BioMention]): Unit

  /** updates those data structures with any new info */
  def update(mentions: Seq[BioMention]): Unit

  /** assigns context to mentions given current state of the engine */
  def assign(mentions: Seq[BioMention]): Seq[BioMention]

}

object ContextEngine extends LazyLogging {
  // Seq of the labels we care about in context
  val contextMatching = Seq("Species", "Organ", "CellLine", "CellType", "Cellular_component", "TissueType", "ContextPossessive", "ContextLocation", "ContextDirection")

  def isContextMention(mention:BioMention) = (ContextEngine.contextMatching map (mention.labels.contains(_))).foldLeft(false)(_||_)

  def getContextKey(mention:BioMention):(String, String) ={
    val id = if(mention.isGrounded) mention.grounding match{
      case Some(grounding) => grounding.nsId
      case None => "UNGROUNDED"
    } else "UNGROUNDED"

    val labels = mention.labels filter (contextMatching.contains(_))
    println(s"Printing label of grounding ID from policy4: ${id}")

    (labels.head, id)
  }

  // Vocabularies

  // Rebuild the latent vocabulary out of the new grounding component
  // Build a map of Cxt Key -> Text description

  // Sort the context entries and removes redundant entries
  val sortedContextEntries:Seq[ReachContextKBLister.ContextGrounding] = ReachContextKBLister.listContextKBs.sortWith{
    (a, b) =>
      if(a.ctxType != b.ctxType){
        a.ctxType < b.ctxType
      }
      else{
        val s = s"${a.namespace}:${a.id}"
        val t = s"${b.namespace}:${b.id}"

        s < t
      }
  }.groupBy{ // Group by namespace and id
    e => (e.namespace, e.id)
  }.values.map(_(0)).toSeq // Select the first element of each group
  
}
