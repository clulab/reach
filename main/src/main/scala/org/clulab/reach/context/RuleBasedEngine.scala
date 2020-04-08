package org.clulab.reach.context

import collection.immutable
import org.clulab.reach.mentions._


abstract class RuleBasedContextEngine extends ContextEngine {

  // Fields
  // To be overridden in the implementations. Returns a sequence of (Type, Val) features
  // Feature order should be kept consistent for all return values
  var orderedContextMentions:Map[Int, Seq[BioTextBoundMention]] = _
  // This is to keep the default species if necessary
  var defaultContexts:Option[Map[String, String]] = None

  // Name of the entry features
  /** initializes any data structure that needs to be initialized */
  def infer(mentions: Seq[BioMention]){
    // Keep only the context mentions
    val contextMentions = mentions filter ContextEngine.isContextMention map (_.asInstanceOf[BioTextBoundMention])
    // Map them to (sentence #, mention instance)
    val entries = contextMentions groupBy (m => m.sentence)
    // Create an ordered Map, for efficiency
    orderedContextMentions = immutable.TreeMap(entries.toArray:_*)

    // Compute default context classes
    // First count the context types
    val contextCounts:Map[(String, String), Int] = contextMentions map ContextEngine.getContextKey groupBy identity mapValues (_.size)
    // Then group them by class
    val defaultContexts:Map[String, String] = contextCounts.toSeq.groupBy(_._1._1)
      // Sort them in decreasing order by frequency
      .mapValues(_.map(t => (t._1._2, t._2)))
      // And pick the id with of the type with highest frequency
      .mapValues(l => l.maxBy(_._2)._1)

    // Assign it to the class field
    this.defaultContexts = Some(defaultContexts)
  }

  /** updates those data structures with any new info */
  def update(mentions: Seq[BioMention]): Unit

  /** assigns context to mentions given current state of the engine */
  def assign(mentions: Seq[BioMention]): Seq[BioMention]
}
