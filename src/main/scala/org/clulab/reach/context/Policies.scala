package org.clulab.reach.context

import scala.annotation.tailrec


// Policy Two
class BoundedPaddingContext(
 bound:Int = 3 // Default bound to extend the policy
) extends RuleBasedContextEngine{

  protected def contextTypes = Seq("Species", "Organ", "CellType", "CellLine", "Cellular_component", "TissueType")

  // TODO: Do something smart to resolve ties
  protected def untie(entities:Seq[(String, String)]) = entities.head

  protected final def padContext(prevStep:Seq[Int], remainingSteps:List[Seq[Int]], repetitions:Seq[Int], bound:Int):List[Seq[Int]] = {
    @tailrec
    def iter(prevStep:Seq[Int], remainingSteps:List[Seq[Int]], repetitions:Seq[Int], bound:Int, acc:List[Seq[Int]]):List[Seq[Int]] = {


      remainingSteps match {

        case head::tail =>
          // Group the prev step inferred row and the current by context type, then recurse
          val prevContext = prevStep map (ContextEngine.getKey(_, ContextEngine.reversedLatentVocabulary)) groupBy (_._1)
          val currentContext = head map (ContextEngine.getKey(_, ContextEngine.reversedLatentVocabulary)) groupBy (_._1)

          // Apply the heuristic
          // Inferred context of type "x"
          val newRepetitions = new Array[Int](repetitions.size)

          val currentStep = contextTypes.flatMap{ // Do this for each type of context. Flat Map as there could be more than one context of a type (maybe)
            contextType =>
              val stepIx = this.contextTypes.indexOf(contextType)

              if(repetitions(stepIx) < bound){
                (prevContext.lift(contextType), currentContext.lift(contextType)) match {
                  // No prev, Current
                  case (None, Some(curr)) =>
                    newRepetitions(stepIx) = 1
                    Seq(untie(curr))
                  // Prev, No current
                  case (Some(prev), None) =>
                    newRepetitions(stepIx) = repetitions(stepIx)+1
                    Seq(prev.head)
                  // Prev, Current
                  case (Some(prev), Some(curr)) =>
                    newRepetitions(stepIx) = 1
                    Seq(untie(curr))
                  // No prev, No current
                  case (None, None) =>
                    newRepetitions(stepIx) = 1
                    Nil
                }
              }
              else{
                newRepetitions(stepIx) = 1
                currentContext.lift(contextType) match {
                  case Some(curr) =>
                    Seq(untie(curr))
                  case None =>
                    Seq()
                }
              }

          } map (ContextEngine.getIndex(_, ContextEngine.latentVocabulary))

          // Recurse
          iter(currentStep, tail, newRepetitions, bound, currentStep::acc)


        case Nil => acc
      }
    }

    iter(prevStep, remainingSteps, repetitions, bound, Nil)
  }
  // Apply the policy
  protected override def inferContext = padContext(Seq(), latentSparseMatrix, Seq.fill(this.contextTypes.size)(1), bound)

}


// Policy 1
class PaddingContext extends BoundedPaddingContext(Int.MaxValue){

}


// Policy 3
class FillingContext(bound:Int = 3) extends BoundedPaddingContext(bound){

    // Override the infer context to fill the empty slots
    protected override def inferContext = {
      // Get the most common mentioned context of each type
      val defaultContexts = this.mentions.flatten.map(ContextEngine.getContextKey(_))  // Get the context keys of the mentions
        .filter(x => this.contextTypes.contains(x._1)).groupBy(_._1) // Keep only those we care about and group them by type
        .mapValues(bucket => bucket.map(ContextEngine.getIndex(_, ContextEngine.featureVocabulary))) // Get their numeric value from the vocabulary
        .mapValues(bucket => bucket.groupBy(identity).mapValues(_.size)) // Count the occurences
        .mapValues(bucket => Seq(bucket.maxBy(_._2)._1)) // Select the most common element

      // Let the super class do its job
      val paddedContext = super.inferContext

      // Now for each line assign a default context if necessary
      paddedContext map {
        step =>
          // Existing contexts for this line
          val context = step.map(ContextEngine.getKey(_, ContextEngine.reversedLatentVocabulary)).groupBy(_._1)
          this.contextTypes flatMap {
            ctype =>
              context.lift(ctype) match {
                case Some(x) =>
                  x map (ContextEngine.getIndex(_, ContextEngine.latentVocabulary))
                case None =>
                  defaultContexts.lift(ctype).getOrElse(Seq())
              }
          }
      }
    }
}

// Policy 4
class BidirectionalPaddingContext(
    bound:Int = 3 // Default bound to extend the policy
) extends BoundedPaddingContext{
    protected override def inferContext = {
        // Do the same as before
        val firstPass = super.inferContext
        // Reverse the sequences and use the same algorithm
        val reversedContext = firstPass map { _.reverse }
        val paddedContext = padContext(Seq(), reversedContext,
         Seq.fill(this.contextTypes.size)(1), bound)
        // Don't forget to reverse again
        paddedContext map { _.reverse }
    }
}
