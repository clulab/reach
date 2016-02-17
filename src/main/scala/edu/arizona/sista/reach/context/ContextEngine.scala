package edu.arizona.sista.reach.context

import java.io._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.nxml.FriesEntry

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

  // // Writes the two matrix files to disk
  // def outputContext(context:ContextEngine, path:String) = {
  //
  //   val outObserved = path + ".obs"
  //   val outLatent = path + ".lat"
  //
  //   val observedMatrix = context.featureMatrix
  //   val latentMatrix = context.latentStateMatrix
  //
  //   // First output the latent states sequence
  //   val outStreamLatent:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outLatent)))
  //
  //   for(step <- latentMatrix){
  //     val line = step map (if(_) "1" else "0") mkString(" ")
  //     outStreamLatent.println(line)
  //   }
  //   outStreamLatent.close()
  //
  //   // Now the observed values
  //   val outStreamObserved:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outObserved)))
  //   for(step <- observedMatrix){
  //     val line = step map ( x => f"$x%1.0f") mkString (" ")
  //     outStreamObserved.println(line)
  //   }
  //   outStreamObserved.close()
  // }
  //
  // def outputVocabularies(context:ContextEngine, path:String) = {
  //   val outObserved = path + ".obsvoc"
  //   val outLatent = path + ".latvoc"
  //   // First output the latent states voc
  //   val outStreamLatent:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outLatent)))
  //
  //   context.latentVocabulary foreach {
  //     outStreamLatent.println(_)
  //   }
  //
  //   outStreamLatent.close()
  //
  //   // Now the observed voc
  //   val outStreamObserved:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outObserved)))
  //
  //   context.observationVocavulary foreach {
  //     outStreamObserved.println(_)
  //   }
  //
  //   outStreamObserved.close()
  // }
}
