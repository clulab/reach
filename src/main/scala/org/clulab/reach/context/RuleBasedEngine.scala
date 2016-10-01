package org.clulab.reach.context

import collection.immutable
import org.clulab.reach.mentions._


abstract class RuleBasedContextEngine extends ContextEngine {

  // Fields
  // To be overriden in the implementations. Returns a sequence of (Type, Val) features
  // Feature order should be kept consisting for all return values
  var orderedContextMentions:Map[Int, Seq[BioTextBoundMention]] = _
  // This is to keep the default species if necessary
  var defaultSpeciesContext:Option[String] = None

  // Name of the entry features
  /** initializes any data structure that needs to be initialized */
  def infer(mentions: Seq[BioMention]){
    // Keep only the context mentions
    val contextMentions = mentions filter ContextEngine.isContextMention map (_.asInstanceOf[BioTextBoundMention])
    // Map them to (sentence #, mention instance)
    val entries = contextMentions groupBy (m => m.sentence)
    // Create an ordered Map, for efficiency
    orderedContextMentions = immutable.TreeMap(entries.toArray:_*)

    // Compute the default species
    val speciesMentions = contextMentions filter (_.labels.head == "Species")
    val counts = speciesMentions.groupBy(_.nsId).mapValues(_.size).toSeq.map(t => (t._2 -> t._1)).toMap
    if(!counts.isEmpty){
      val key = counts.keys.max
      defaultSpeciesContext = Some(counts(key))
    }
  }

  /** updates those data structures with any new info */
  def update(mentions: Seq[BioMention]): Unit

  /** assigns context to mentions given current state of the engine */
  def assign(mentions: Seq[BioMention]): Seq[BioMention]
}
