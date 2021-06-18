package org.clulab.reach.context

import org.clulab.odin.Mention
import org.clulab.reach.mentions._
import org.clulab.struct.Counter

import scala.collection.immutable.Range


// Policy Two
class BoundedPaddingContext(
 bound:Int = 3 // Default bound to extend the policy
) extends RuleBasedContextEngine{

  /** updates those data structures with any new info */
  def update(mentions: Seq[BioMention]){}

  /** assigns context to mentions given current state of the engine */
  def assign(mentions: Seq[BioMention]) = {

    assert(this.orderedContextMentions != null, "ContextEngine: infer must be called before assign")

    // Assign context to each mention
    for(m <- mentions){
      var (contextMap, contextMetaData) = queryContext(m)

      // If the context map doesn't have species and there's a default
      val species = "Species" // Put this somewhere else
      if (!contextMap.contains(species) && defaultContexts.exists(_.contains(species)))
        contextMap += (species -> Array(defaultContexts.get(species)))

      // Assign the context map to the mention
      m.contextOpt = if(contextMap.nonEmpty) Some(contextMap) else None
      // Assign the context metadata map to the mention
      m.contextMetaDataOpt = if(contextMetaData.nonEmpty) Some(contextMetaData) else None
    }

    mentions
  }

  // This queries the context mentions and builds the context map
  def queryContext(m:Mention):(ContextMap, ContextMetaData) = {
    val range = getRange(m.sentence)
    // Get the mentions we need
    val contextMentions = range.flatMap(orderedContextMentions.getOrElse(_, Nil))
    // Extract the keys just once
    val contextKeys = contextMentions.map(ContextEngine.getContextKey)
    // Make the dictionary
    val contextMap: ContextMap = contextKeys groupBy (_._1) mapValues (t => t.map(_._2).distinct)
    // Build the dictionary with the context metadata
    val distances = contextMentions.zip(contextKeys).map { case (mention, key) =>
      val distance = Math.abs(mention.sentence - m.sentence)
      (key, distance)
    }
    val contextMetaData = distances.groupBy(_._1).mapValues(d => new Counter(d map (_._2)))

    (contextMap, contextMetaData)
  }

  // This is to be overriden by the subclasses!
  def getRange(ix:Int): Range = Range.inclusive(ix-bound, ix)

}


// Policy 1
class PaddingContext extends BoundedPaddingContext(Int.MaxValue)


// Policy 3
class FillingContext(bound:Int = 3) extends BoundedPaddingContext(bound){

  override def queryContext(m:Mention):(ContextMap, ContextMetaData) = defaultContexts match {
    case Some(defaults) =>
      // Make a mutable map to add the default contexts
      val (contextMap, contextMetaData) = super.queryContext(m)
      val contextMapAdditions = defaults
          .filterKeys(!contextMap.contains(_))
          .mapValues(Seq(_))

      (contextMap ++ contextMapAdditions, contextMetaData)

    // If there aren't any defaults
    case None => super.queryContext(m)
  }
}

// Policy 4
class BidirectionalPaddingContext(
    bound:Int = 3 // Default bound to extend the policy
) extends BoundedPaddingContext{
    override def getRange(ix:Int): Range = Range.inclusive(ix-bound, ix+bound)
}
