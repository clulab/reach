package edu.arizona.sista.reach.context


import java.io._
import edu.arizona.sista.reach._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.reach.context.rulebased._
import edu.arizona.sista.reach.utils.FileReader
import edu.arizona.sista.reach.grounding.ReachKBUtils

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
    val id = if(mention.isGrounded) mention.xref match{
      case Some(xref) => xref.id
      case None => "UNGROUNDED"
    } else "UNGROUNDED"

    val labels = mention.labels filter (contextMatching.contains(_))

    (labels.head, id)
  }

  // Vocabularies
  // Get relevant files
  val kbFiles = Seq(("Cell_Lines.tsv.gz", "CellLine"), ("Cell_Type.tsv.gz", "CellType"), ("Organ.tsv.gz", "Organ"), ("Species.tsv.gz", "Species"), ("tissue-type.tsv.gz", "CellType"),
    ("uniprot-subcellular-locations.tsv.gz", "Cellular_component"), ("GO-subcellular-locations.tsv.gz", "Cellular_component"), ("biopax-cellular_component.tsv.gz", "Cellular_component"),
    ("manual-cellular_component.tsv.gz", "Cellular_component")) map {
      case (path, ctxType) => (ctxType, ReachKBUtils.makePathInKBDir(path))
    }

  // Build a map of Cxt Key -> Text description
  val latentVocabulary:Map[(String, String), String] = (kbFiles flatMap {
    case (ctxType, file) =>
      ReachKBUtils.sourceFromResource(file).getLines map (_.split("\t").toList) map {
        tokens =>
          val key = tokens.last
          val value = tokens.dropRight(1).mkString(" ")
          (ctxType, key) -> value
    }
  }).toMap

  // Same here but for the observed features
  val featureVocabulary:Map[(String, String), String] = latentVocabulary // Now add any new stuff that may show up as a feature

  def getDescription(mention:BioMention, voc:Map[(String, String), String]):String = getDescription(getContextKey(mention), voc)

  def getDescription(key:(String, String), voc:Map[(String, String), String]):String = voc.lift(key) match {
    case Some(desc) => desc
    case None =>
      println(s"WARNING: key $key not found in the context vocabulary")
      "MISSING"
  }

  def getIndex(mention:BioMention, voc:Map[(String, String), String]):Int = getIndex(getContextKey(mention), voc)

  // index 0 is "Missing", the rest of the entries get shifted 1 position
  def getIndex(key:(String, String), voc:Map[(String, String), String]):Int = voc.keys.toList.indexOf(key) match{
    case -1 =>
      println(s"WARNING: key $key not found in the context vocabulary")
      0
    case ix:Int => ix + 1
  }

  def getKey(ix:Int, voc:Map[(String, String), String]):(String, String) = if(ix>0) voc.keys.toList(ix - 1) else ("MISSING", "MISSING")
}
