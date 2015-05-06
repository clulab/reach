package edu.arizona.sista.bionlp.reach.postprocessing

import edu.arizona.sista.odin.domains.bigmechanism.summer2015.DarpaFlow
import edu.arizona.sista.odin._

/**
 * PostProcessor flattens the representation of Simple EventMention by unpacking the arguments of any RelationMentions
 * Created by gus
 */
class PostProcessor extends DarpaFlow {

  /**
   * For flattening simple EventMentions that contain a RelationMention by unpackign the RM's argument map
   * @param args
   * @return
   */
  def unpackEventArgs(args: Seq[(String, Mention)]): Seq[(String, Mention)] = args flatMap {

      // TextBounds are perfect just the way they are
      case (label:String, tb:TextBoundMention) => Seq((label, tb))

      // Unpack the args of a RelationMention
      case (label:String, rel:RelationMention) => {
        val unpackedArgs:Seq[(String, Mention)] = argsToTuples(rel.arguments)
        unpackEventArgs(unpackedArgs)
      }

      // Simple EventMentions shouldn't have EventMentions as args.
      case (label:String, em:EventMention) => {
        val unpackedArgs:Seq[(String, Mention)] = argsToTuples(em.arguments)
        unpackEventArgs(unpackedArgs)
      }
  }

  /**
   * Turn a Mention's argument map into a Seq of (argName, Mention) tuples
   * @param argMap a Mention's argument Map
   * @return a Seq of (argName, Mention) tuples
   */
  def argsToTuples(argMap: Map[String, Seq[Mention]]): Seq[(String, Mention)] = {
    // I don't think flatMap can work here...
    argMap.map{ case (argName, mentions) => mentions.map(m => (argName, m))}
      .flatten
      .toSeq
  }

  def mkArgMap(argTuples: Seq[(String, Mention)]): Map[String, Seq[Mention]] = {
    argTuples
      .groupBy(_._1)
      .mapValues(_.map(_._2))
  }

  def apply (mentions: Seq[Mention], state: State): Seq[Mention] = mentions map {
    //TODO: Find the longest tbm for this span
    case tm: TextBoundMention => tm
    case em: EventMention => {
      // Get Seq (argName, Mention) tuples
      val argRepresentations = argsToTuples(em.arguments)

      // Unpack any relation mentions and create a new argument map
      val simplifiedArgs = unpackEventArgs(argRepresentations)
      // Create the new argument map
      val simplifiedArgMap = mkArgMap(simplifiedArgs)
      // Here is our EventMention with unpacked relation mentions
      new EventMention(em.labels,
            em.trigger,
            simplifiedArgMap,
            em.sentence,
            em.document,
            em.keep,
            em.foundBy,
            em.xref)
    }
    case rel: RelationMention => rel
  }
}
