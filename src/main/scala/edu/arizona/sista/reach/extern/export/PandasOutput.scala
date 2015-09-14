package edu.arizona.sista.odin.extern.export.context

import scala.collection.mutable
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.processors._
import edu.arizona.sista.reach.nxml.FriesEntry

/**
  * Class to output a field-separated text file to parse as a Pandas' Data Frame
  *   Written by Enrique Noriega. 27/7/2015.
  *
  */

class PandasOutput() {
  def toCSV(paperID:String,
            allMentions:Seq[BioMention],
            paperPassages:Map[BioMention, FriesEntry]):(Seq[String],
               Seq[String], Seq[String], Seq[String]) = {

    // Get fetch documents
    val docs = (allMentions map (m => (m.document, paperPassages(m))) sortBy { t:(Document, FriesEntry) =>
      t._1.id match {
        // Take the last element of the Id and make it a number
        case Some(id) => id.split("_").last.toInt
        case None => -1
      }
    }).distinct

    var entities = new mutable.ListBuffer[String]()
    var relations = new mutable.ListBuffer[String]()
    val lines = new mutable.ListBuffer[String]()
    val events = new mutable.ListBuffer[String]()

    entities += "Paper ID\tLine Num\tGrounded ID\tText\tType"
    relations += "Paper ID\tLine Num\tMaster ID\tDependent ID\tText\tType"
    lines += "Paper ID\tIs title?\tSection ID\tPassage ID\tLine Num\tText"
    events += "Paper ID\tLine Num\tType\tTrigger"

    var absoluteIx = 0 // The absolute index of the sentences

    for (((doc, entry), passageIx) <- docs.zipWithIndex){
      // Extract the ordered sentences of the passage
      val sentences:Seq[Sentence] = doc.sentences

      // keep the mentions relevant only to this passage
      val mentions = allMentions filter (_.document == doc)

      for ((sentence, ix) <- sentences.zipWithIndex){

        // Store the sentence's text
        // paperID - is title - section ID - passageID - line Num - Text
        lines += s"$paperID\t${entry.isTitle}\t${entry.sectionId}\t$passageIx\t$absoluteIx\t${sentence.getSentenceText}"

        // Get the mentions in the current sentence
        val m = mentions filter (_.sentence == ix)

        m foreach { mention =>
          // Generate the lines for entities
          mention match {
            case tb:BioTextBoundMention =>
              var sb = new mutable.StringBuilder()
              // paperID - sentence ix - ID - text - type
              sb ++= s"$paperID\t$absoluteIx\t"
              // Get the grounded id
              tb.xref match {
                case Some(xref) => sb ++= xref.printString
                case None => sb ++= "N/A"
              }
              sb ++= s"\t${tb.text}\t"

              if (tb.labels contains "Organ"){
                sb ++= "Organ"
                entities += sb.toString
              }
              else if (tb.labels contains "Species"){
                sb ++= "Species"
                entities += sb.toString
              }
              else if (tb.labels contains "CellType"){
                sb ++= "CellType"
                entities += sb.toString
              }
              else if (tb.labels contains "CellLine"){
                sb ++= "CellLine"
                entities += sb.toString
              }
              else if (tb.labels contains "Gene_or_gene_product"){
                sb ++= "Gene_or_gene_product"
                entities += sb.toString
              }
            // Generate the lines for context relations
            case rel:BioRelationMention =>
              var sb = new mutable.StringBuilder()

              val interesting = Seq("ContextPossesive", "ContextLocation", "ContextDirection")

              if(interesting.exists(rel.labels.contains(_))){

                // paperID - sentence ix - master ID - dependent ID - text - type
                sb ++= s"$paperID\t$absoluteIx\t"

                // Resolve the master participant
                rel.arguments("master").head.asInstanceOf[BioTextBoundMention].xref match {
                  case Some(xref) =>
                    sb ++= xref.printString
                    sb ++= "\t"
                  case None => sb ++= "N/A"
                }

                // Resolve the dependent participant
                rel.arguments("dependent").head.asInstanceOf[BioTextBoundMention].xref match {
                  case Some(xref) =>
                    sb ++= xref.printString
                  case None => sb ++= "N/A"
                }

                sb ++= s"\t${rel.text}\t"

                // Get the type of context
                if(rel.labels contains "ContextPossesive"){
                  sb ++= "ContextPossesive"
                  relations += sb.toString
                }
                else if(rel.labels contains "ContextLocation"){
                  sb ++= "ContextLocation"
                  relations += sb.toString
                }
                else if(rel.labels contains "ContextDirection"){
                  sb ++= "ContextDirection"
                  relations += sb.toString
                }
              }

            case ev:BioEventMention =>
                var sb = new mutable.StringBuilder()

                // paperID - sentence ix - labels - trigger txt - argument IDs (this last field may be comma-separated)
                sb ++= s"$paperID\t$absoluteIx\t"

                 // These are the labels to be removed
                val removeList = Seq("SimpleEvent", "Event", "PossibleController", "ActivationEvent")

                // Add labels
                val labels = ev.labels filter (!removeList.contains(_))
                sb ++= s"${labels.mkString(",")}\t"

                // Add trigger text
                sb ++= s"${ev.trigger.text}\t"

                // Add arguments of the event
                val args = ev.arguments flatMap {
                  case (k, v) => v map {
                    case tb:BioTextBoundMention => tb.xref match {
                      case Some(xref) => s"$k,${xref.printString}"
                      case None => s"$k,N/A"
                    }
                  }
                }

                sb ++= args.mkString("|")
                events += sb.toString

            case _ => Unit

          }
        }
        absoluteIx += 1
      }
    }

    (entities.toList.distinct, events.toList.distinct, relations.toList.distinct, lines.toList)
  }
}
