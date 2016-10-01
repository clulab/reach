package org.clulab.reach.context

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

    // Assign context to each mention
    for(m <- mentions){
      val interval = getInterval(m.sentence)
      var contextMap = queryContext(interval)

      // If the context map doesn't have species and there's a default
      if(!contextMap.keySet.contains("Species")
          && defaultSpeciesContext.isDefined){
        contextMap += ("Species" -> Array(defaultSpeciesContext.get))
      }

      // Assign the context map to the mention
      m.context = Some(contextMap)
    }

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
    val context:Map[String, Seq[String]] = contextMentions map ContextEngine.getContextKey groupBy (_._1) mapValues (t => t.map(_._2).take(1))

    context
  }

  // This is to be overriden by the subclasses!
  def getInterval(ix:Int):(Int, Int) = (ix-bound, ix)

}


// Policy 1
class PaddingContext extends BoundedPaddingContext(Int.MaxValue){

}

// ENRIQUE: Disabled until further notice
// Policy 3
// class FillingContext(bound:Int = 3) extends BoundedPaddingContext(bound){
//
//     // Override the infer context to fill the empty slots
//     protected override def inferContext = {
//       // Get the most common mentioned context of each type
//       val defaultContexts = this.mentions.flatten.map(ContextEngine.getContextKey(_))  // Get the context keys of the mentions
//         .filter(x => this.contextTypes.contains(x._1)).groupBy(_._1) // Keep only those we care about and group them by type
//         .mapValues(bucket => bucket.map(ContextEngine.getIndex(_, ContextEngine.featureVocabulary))) // Get their numeric value from the vocabulary
//         .mapValues(bucket => bucket.groupBy(identity).mapValues(_.size)) // Count the occurences
//         .mapValues(bucket => Seq(bucket.maxBy(_._2)._1)) // Select the most common element
//
//       // Let the super class do its job
//       val paddedContext = super.inferContext
//
//       // Now for each line assign a default context if necessary
//       paddedContext map {
//         step =>
//           // Existing contexts for this line
//           val context = step.map(ContextEngine.getKey(_, ContextEngine.latentVocabulary)).groupBy(_._1)
//           this.contextTypes flatMap {
//             ctype =>
//               context.lift(ctype) match {
//                 case Some(x) =>
//                   x map (ContextEngine.getIndex(_, ContextEngine.latentVocabulary))
//                 case None =>
//                   defaultContexts.lift(ctype).getOrElse(Seq())
//               }
//           }
//       }
//     }
// }

// Policy 4
class BidirectionalPaddingContext(
    bound:Int = 3 // Default bound to extend the policy
) extends BoundedPaddingContext{
    override def getInterval(ix:Int):(Int, Int) = (ix-bound, ix+bound)
}
