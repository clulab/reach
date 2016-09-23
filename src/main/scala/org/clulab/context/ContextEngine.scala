package org.clulab.context

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


object ContextEngine {
  // Seq of the labels we care about in context
  //val contextMatching = Seq("Species", "Organ", "CellLine", "CellType", "Cellular_component", "TissueType")

  //def isContextMention(mention:BioMention) = (ContextEngine.contextMatching map (mention.labels.contains(_))).foldLeft(false)(_||_)

  def getContextKey(mention:BioMention):(ContextClass.Value, String) ={
    val id = if(mention.isGrounded) mention.grounding match{
      case Some(grounding) => grounding.nsId
      case None => "UNGROUNDED"
    } else "UNGROUNDED"

    //val labels = mention.labels filter (contextMatching.contains(_))
    val ctxClass = ContextClass.getContextClass(mention)

    (ctxClass, id)
  }

  // Vocabularies

  // Rebuild the latent vocabulary out of the new grounding component
  // Build a map of Cxt Key -> Text description

  // Sort the context entries and removes redundant entries
  val sortedContextEntries:Seq[ReachContextKBLister.ContextGrounding] = ReachContextKBLister.listContextKBs.groupBy{ // Group by namespace and id
    e => (e.namespace, e.id)
  }.values.map(_(0)).toSeq.sortWith{
    (a, b) =>
      val s = s"${a.namespace}:${a.id}"
      val t = s"${b.namespace}:${b.id}"
      s < t
  } // Select the first element of each group


  val latentVocabulary:Map[(ContextClass.Value, String), (Int, String)] = sortedContextEntries.zipWithIndex.map{
    case (entry, ix) => (ContextClass.withName(entry.ctxType), s"${entry.namespace}:${entry.id}") -> (ix, entry.text)
  }.toMap

  val reversedLatentVocabulary:Map[Int, (ContextClass.Value, String)] = {for((key, value) <- latentVocabulary) yield {
    (value._1 -> key)
  }}.toMap

  // Same here but for the observed features
  val featureVocabulary:Map[(ContextClass.Value, String), (Int, String)] = latentVocabulary // Now add any new stuff that may show up as a feature

  val reversedFeatureVocabulary:Map[Int, (ContextClass.Value, String)] = {for((key, value) <- featureVocabulary) yield {
    (value._1 -> key)
  }}.toMap

  def getDescription(mention:BioMention, voc:Map[(ContextClass.Value, String), (Int, String)]):String = {
    val key = getContextKey(mention)
    if(key._2.startsWith("uaz:")){
      println(s"Warning: ${mention.text}")
    }
    getDescription(key, voc)
  }

  def getDescription(key:(ContextClass.Value, String), voc:Map[(ContextClass.Value, String), (Int, String)]):String = voc.lift(key) match {
    case Some((ix, desc)) => desc
    case None =>
      println(s"WARNING: key $key not found in the context vocabulary")
      "MISSING"
  }

  def getIndex(mention:BioMention, voc:Map[(ContextClass.Value, String), (Int, String)]):Int = {
    val key = getContextKey(mention)
    if(key._2.startsWith("uaz:")){
      println(s"Warning: ${mention.text}")
    }
    getIndex(key, voc)
  }

  // index 0 is "Missing", the rest of the entries get shifted 1 position
  def getIndex(key:(ContextClass.Value, String), voc:Map[(ContextClass.Value, String), (Int, String)]):Int = {
    val (ix, description) = voc(key)
    return ix
  }

  def getKey(ix:Int, voc:Map[Int, (ContextClass.Value, String)]):(ContextClass.Value, String) = {
    voc(ix)
  }//if(ix>0) voc.keys.toList(ix - 1) else ("MISSING", "MISSING")
}
