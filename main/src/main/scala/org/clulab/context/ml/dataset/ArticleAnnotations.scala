package org.clulab.context.ml.dataset

import java.io._
import io.Source
import ai.lum.common.Interval
import ai.lum.nxmlreader.standoff.Tree
import org.clulab.reach.mentions.BioTextBoundMention
import org.clulab.context.ml.PreAnnotatedDoc
import org.clulab.context.ContextClass
import org.clulab.utils.Serializer

// object ContextLabel extends Enumeration{
//   val Species, CellLine, CellType, Organ, CellularLocation, UNDETERMINED = Value
// }

case class ContextType(val contextType:ContextClass.Value, val id:String)

object ContextType{

  def parse(mention:BioTextBoundMention):ContextType = this(ContextClass.getContextClass(mention), mention.nsId)

  def parse(annotationId:String) = {
    val tokens = annotationId.split(":", 2)

    val (namespace, gid) = (tokens(0), tokens(1))

    namespace match {
      case "taxonomy" => this(ContextClass.Species, annotationId)
      case "cellosaurus" => this(ContextClass.CellLine, annotationId)
      case "cellontology" => this(ContextClass.CellType, annotationId)
      case "uberon" => this(ContextClass.Organ, annotationId)
      case "tissuelist" => this(ContextClass.TissueType, annotationId)
      case "go" => this(ContextClass.Cellular_component, annotationId)
      case "uaz" =>
        val y = gid.split("-")
        if(y.size < 2){
            if(y(0).toLowerCase.contains("uberon"))
              this(ContextClass.Organ, annotationId)
            else if(y(0).toLowerCase.contains("cl:"))
              this(ContextClass.CellLine, annotationId)
            else
              println(s"DEBUG: Unrecognized context id $annotationId - ContextType.parse")
              this(ContextClass.Undetermined, annotationId)
        }
        else{
            val x = y(1)
            x.toLowerCase match {
             case "org" => this(ContextClass.Organ, annotationId)
             case "cline" => this(ContextClass.CellLine, annotationId)
             case "ct" => this(ContextClass.CellType, annotationId)
             case i =>
                println(s"DEBUG: Unrecognized context id $annotationId - ContextType.parse")
                this(ContextClass.Undetermined, annotationId)
            }
        }
      case _ =>
        println(s"DEBUG: Unrecognized context id $annotationId - ContextType.parse")
        this(ContextClass.Undetermined, annotationId)
    }
  }
}


case class EventAnnotation(val sentenceId:Int,
   val interval:Interval,
   val annotatedContexts:Option[Seq[ContextType]] = None){
     override def equals(o: Any) = o match {
       case other:EventAnnotation =>
        if(this.sentenceId == other.sentenceId &&
          this.interval == other.interval)
          true
        else
          false
       case _ => false
     }
   }

case class ContextAnnotation(val sentenceId: Int,
   val interval:Interval,
   val contextType:ContextType){
     override def equals(o: Any) = o match {
       case other:EventAnnotation =>
        if(this.sentenceId == other.sentenceId &&
          this.interval == other.interval)
          true
        else
          false
       case _ => false
     }
   }

case class ArticleAnnotations(val name:String,
   val sentences:Map[Int, String],
   val eventAnnotations:Seq[EventAnnotation],
   val contextAnnotations:Seq[ContextAnnotation],
   val standoff:Option[Tree] = None,
   val preprocessed:Option[PreAnnotatedDoc] = None)

object ArticleAnnotations{
  def readPaperAnnotations(directory:String):ArticleAnnotations = {
    // Read the tsv annotations from a paper
    val rawSentences = Source.fromFile(new File(directory, "sentences.tsv")).getLines
    val sentences:Map[Int, String] = rawSentences.map{
      s =>
        val tokens = s.split("\t")
        if(tokens.size < 2){
          (tokens(0).toInt, "")
        }
        else
          (tokens(0).toInt, tokens(1))
    }.toMap

    val rawEvents = Source.fromFile(new File(directory, "events.tsv")).getLines
    val events = rawEvents.map{
      s =>
        val tokens = s.split("\t")
        val sentenceId = tokens(0).toInt

        val bounds = tokens(1).split("-").map(_.toInt)
        val (start, end) = (bounds(0), bounds(1))
        val interval = if(start == end) Interval.singleton(start) else Interval.closed(start, end)
        val contexts:Seq[ContextType] =
          if(tokens.size == 3) tokens(2).split(",").map(ContextType.parse(_))
          else{
            // TODO: Uncomment this and fix it in the files
            //println(s"DEBUG: Event without context in $directory")
            Seq()
          }

        EventAnnotation(sentenceId, interval, Some(contexts))
    }.toSeq

    val rawContext = Source.fromFile(new File(directory, "context.tsv")).getLines
    val context = rawContext.map{
      s =>
        val tokens = s.split("\t")

        val sentenceId = tokens(0).toInt

        val bounds = tokens(1).split("-").map(_.toInt)
        val (start, end) = (bounds(0), bounds(1))
        val interval = if(start == end) Interval.singleton(start) else Interval.closed(start, end)
        val context = ContextType.parse(tokens(2))

        ContextAnnotation(sentenceId, interval, context)
    }.toSeq

    val soffFile = new File(directory, "standoff.json")
    val standoff = None //if(soffFile.exists) Some(Tree.readJson(soffFile.getPath)) else None TODO: Uncomment this when the pullrequest is accepted

    val preprocessed:Option[PreAnnotatedDoc] = {
      val ppFile = new File(directory, "preprocessed.ser")
      if(ppFile.exists){
        // val ois = new ObjectInputStream(new FileInputStream(ppFile)) {
        //   override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
        //     try { Class.forName(desc.getName, false, getClass.getClassLoader) }
        //     catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
        //   }
        // }

        Some(Serializer.load[PreAnnotatedDoc](ppFile.getAbsolutePath))
      }
      else{
        None
      }
    }

    ArticleAnnotations(directory, sentences, events, context, standoff, preprocessed)
  }
}
