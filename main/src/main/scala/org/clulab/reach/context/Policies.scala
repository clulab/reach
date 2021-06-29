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
  def assign(mentions: Seq[BioMention]): Seq[BioMention] = {
    import BoundedPaddingContext.species

    assert(this.orderedContextMentions != null, "ContextEngine: infer must be called before assign")

    // Assign context to each mention
    for(m <- mentions){
      val (contextMap, contextMetaData) = queryContext(m)
      // If the context map doesn't have species and there's a default
      val contextMapWithDefaultSpecies =
          if (!contextMap.contains(species) && defaultContexts.exists(_.contains(species)))
            contextMap + (species -> Seq(defaultContexts.get(species)))
          else
            contextMap

      // Assign the context map to the mention
      m.setContext(contextMapWithDefaultSpecies)
      // Assign the context metadata map to the mention
      m.setContextMetaData(contextMetaData)
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
    // Make the dictionary, whereby map(identity) is critical for producing a serializable result
    val contextMap = contextKeys.groupBy(_._1).mapValues(value => value.map(_._2).distinct).map(identity)
    // Build the dictionary with the context metadata
    val distances = contextMentions.zip(contextKeys).map { case (mention, key) =>
      val distance = Math.abs(mention.sentence - m.sentence)
      (key, distance)
    }
    val contextMetaData = distances.groupBy(_._1).mapValues(value => new Counter(value.map(_._2))).map(identity)

    (contextMap, contextMetaData)
  }

  // This is to be overriden by the subclasses!
  def getRange(ix:Int): Range = Range.inclusive(ix-bound, ix)

}

object BoundedPaddingContext {
  val species = "Species"
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
