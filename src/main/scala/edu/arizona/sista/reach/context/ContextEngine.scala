package edu.arizona.sista.reach.context


import java.io._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.reach.context.rulebased._
import edu.arizona.sista.reach.utils.FileReader
import edu.arizona.sista.reach.grounding.ReachKBUtils
import edu.arizona.sista.reach.grounding.ReachContextKBLister

trait ContextEngine {

  /** initializes any data structure that needs to be initialized */
  def infer(
      entries: Seq[FriesEntry],
      documents: Seq[Document],
      mentionsPerEntry: Seq[Seq[BioMention]]
  ): Unit

  /** updates those data structures with any new info */
  def update(mentions: Seq[BioMention]): Unit

  /** assigns context to mentions given current state of the engine */
  def assign(mentions: Seq[BioMention]): Seq[BioMention]

}

object ContextEngine {
  // Seq of the labels we care about in context
  val contextMatching = Seq("Species", "Organ", "CellLine", "CellType", "Cellular_component", "ContextPossessive", "ContextLocation", "ContextDirection")

  def getContextKey(mention:BioMention):(String, String) ={
    val id = if(mention.isGrounded) mention.grounding match{
      case Some(grounding) => grounding.nsId
      case None => "UNGROUNDED"
    } else "UNGROUNDED"

    val labels = mention.labels filter (contextMatching.contains(_))

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


  val latentVocabulary:Map[(String, String), String] = sortedContextEntries.map{
    entry => ((entry.ctxType, s"${entry.namespace}:${entry.id}") -> entry.text)
  }.toMap

  // Same here but for the observed features
  val featureVocabulary:Map[(String, String), String] = latentVocabulary // Now add any new stuff that may show up as a feature

  def getDescription(mention:BioMention, voc:Map[(String, String), String]):String = {
    val key = getContextKey(mention)
    if(key._2.startsWith("uaz:")){
      println(s"Warning: ${mention.text}")
    }
    getDescription(key, voc)
  }

  def getDescription(key:(String, String), voc:Map[(String, String), String]):String = voc.lift(key) match {
    case Some(desc) => desc
    case None =>
      println(s"WARNING: key $key not found in the context vocabulary")
      "MISSING"
  }

  def getIndex(mention:BioMention, voc:Map[(String, String), String]):Int = {
    val key = getContextKey(mention)
    if(key._2.startsWith("uaz:")){
      println(s"Warning: ${mention.text}")
    }
    getIndex(key, voc)
  }

  // index 0 is "Missing", the rest of the entries get shifted 1 position
  def getIndex(key:(String, String), voc:Map[(String, String), String]):Int = voc.keys.toList.indexOf(key) match{
    case -1 =>
      println(s"WARNING: key $key not found in the context vocabulary")
      0
    case ix:Int => ix + 1
  }

  def getKey(ix:Int, voc:Map[(String, String), String]):(String, String) = if(ix>0) voc.keys.toList(ix - 1) else ("MISSING", "MISSING")
}
