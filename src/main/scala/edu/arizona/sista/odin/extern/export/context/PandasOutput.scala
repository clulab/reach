package edu.arizona.sista.odin.extern.export.context

import scala.collection.mutable
import edu.arizona.sista.odin._
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.processors._
import edu.arizona.sista.bionlp.FriesEntry

/**
  * Class to output a field-separated text file to parse as a Pandas' Data Frame
  *   Written by Enrique Noriega. 27/7/2015.
  *
  */

class PandasOutput() {
  def toCSV(paperID:String,
            allMentions:Seq[Mention],
            paperPassages:Seq[FriesEntry]):(Seq[String],
               Seq[String], Seq[String]) = {


    // Get fetch documents
    val docs = (allMentions map (_.document) sortBy { x:Document =>
      x.id match {
        // Take the last element of the Id and make it a number
        case Some(id) => id.split("_").last.toInt
        case None => -1
      }
    }).distinct

    var entities = new mutable.ListBuffer[String]()
    var relations = new mutable.ListBuffer[String]()
    val lines = new mutable.ListBuffer[String]()

    var absoluteIx = 0 // The absolute index of the sentences

    for (doc <- docs){
      // Extract the ordered sentences of the passage
      val sentences:Seq[Sentence] = doc.sentences

      // keep the mentions relevant only to this passage
      val mentions = allMentions filter (_.document == doc)

      for (s <- sentences.zipWithIndex){
        val ix = s._2
        val sentence = s._1

        // Store the sentence's text
        lines += s"$paperID\t$absoluteIx\t${sentence.getSentenceText}"

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
            case ev:BioRelationMention =>
              var sb = new mutable.StringBuilder()

              // paperID - sentence ix - master ID - dependent ID - text - type
              sb ++= s"$paperID\t$absoluteIx\t"

              // Resolve the master participant
              ev.arguments("master").head.asInstanceOf[BioTextBoundMention].xref match {
                case Some(xref) =>
                  sb ++= xref.printString
                  sb ++= "\t"
                case None => sb ++= "N/A"
              }

              // Resolve the dependent participant
              ev.arguments("dependent").head.asInstanceOf[BioTextBoundMention].xref match {
                case Some(xref) =>
                  sb ++= xref.printString
                case None => sb ++= "N/A"
              }

              sb ++= s"\t${ev.text}\t"

              // Get the type of context
              if(ev.labels contains "ContextPossesive"){
                sb ++= "ContextPossesive"
                relations += sb.toString
              }
              else if(ev.labels contains "ContextLocation"){
                sb ++= "ContextLocation"
                relations += sb.toString
              }
              else if(ev.labels contains "ContextDirection"){
                sb ++= "ContextDirection"
                relations += sb.toString
              }
            case _ => Unit
          }
        }
        absoluteIx += 1
      }
    }

    (entities.toList.distinct, relations.toList.distinct, lines.toList)
  }
}
