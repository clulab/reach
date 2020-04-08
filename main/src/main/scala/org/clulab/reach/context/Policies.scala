package org.clulab.reach.context

import org.clulab.reach.context.feature_utils.ContextFeatureUtils
import org.clulab.reach.mentions._

import collection.mutable


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
      val interval = getInterval(m.sentence)
      var contextMap = queryContext(interval)

      // If the context map doesn't have species and there's a default
      if(!contextMap.keySet.contains("Species")
          && defaultContexts.isDefined){

        val defaults = defaultContexts.get
        if(defaults.keySet.contains("Species")){
            contextMap += ("Species" -> Array(defaults("Species")))
        }

      }

      // Assign the context map to the mention
      m.context = if(contextMap != Map.empty) Some(contextMap) else None
    }
    //ContextFeatureUtils.writeRowsToFile(mentions)
    mentions
  }

  // This queries the context mentions and builds the context map
  def queryContext(interval:(Int, Int)):Map[String, Seq[String]] = {
    // Get the mentions we need
    val contextMentions = new mutable.ListBuffer[BioTextBoundMention]
    for(i <- interval._1 to interval._2){
      val mentions:Seq[BioTextBoundMention] = orderedContextMentions.lift(i) match{
        case Some(mentions) => mentions
        case None => Nil
      }
      contextMentions ++= mentions
    }

    // Make the dictionary
    val context:Map[String, Seq[String]] = (Nil ++ contextMentions) map ContextEngine.getContextKey groupBy (_._1) mapValues (t => t.map(_._2).take(1)) map identity

    context
  }

  // This is to be overriden by the subclasses!
  def getInterval(ix:Int):(Int, Int) = (ix-bound, ix)

}


// Policy 1
class PaddingContext extends BoundedPaddingContext(Int.MaxValue)


// Policy 3
class FillingContext(bound:Int = 3) extends BoundedPaddingContext(bound){

  override def queryContext(interval:(Int, Int)):Map[String, Seq[String]] = defaultContexts match {
    case Some(defaults) =>
      // Make a mutable map to add the default contexts
      val context:mutable.Map[String, Seq[String]] = mutable.Map.empty ++ super.queryContext(interval)

      val existingKeys = context.keySet
      for(ctxClass <- defaults.keys){
        if(!existingKeys.contains(ctxClass))
          context += (ctxClass -> Seq(defaults(ctxClass)))
      }

      // Convert to an immutable map and return
      Map.empty ++ context

    // If there aren't any defaults
    case None => super.queryContext(interval)
  }
}

// Policy 4
class BidirectionalPaddingContext(
    bound:Int = 3 // Default bound to extend the policy
) extends BoundedPaddingContext{
    override def getInterval(ix:Int):(Int, Int) = (ix-bound, ix+bound)
}
