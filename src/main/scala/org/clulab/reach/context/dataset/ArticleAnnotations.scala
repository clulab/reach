package org.clulab.reach.context.dataset

import java.io._
import io.Source
import ai.lum.common.Interval
import ai.lum.nxmlreader.standoff.Tree
import org.clulab.reach.context.ml.PreAnnotatedDoc

object ContextLabel extends Enumeration{
  val Species, CellLine, CellType, Organ, CellularLocation, UNDETERMINED = Value
}

case class ContextType(val contextType:ContextLabel.Value, val id:String)

object ContextType{
  def parse(annotationId:String) = {
    val tokens = annotationId.split(":", 2)

    val (namespace, gid) = (tokens(0), tokens(1))

    namespace match {
      case "taxonomy" => this(ContextLabel.Species, annotationId)
      case "cellosaurus" => this(ContextLabel.CellLine, annotationId)
      case "cellontology" => this(ContextLabel.CellType, annotationId)
      case "uberon" => this(ContextLabel.Organ, annotationId)
      case "tissuelist" => this(ContextLabel.Organ, annotationId)
      case "go" => this(ContextLabel.CellularLocation, annotationId)
      case "uaz" =>
        // TODO: Fix this to consider uaz:UBERON:0000479 and uaz:CL:0000786
        val y = gid.split("-")
        if(y.size < 2){
            println(s"DEBUG: $annotationId")
            this(ContextLabel.UNDETERMINED, annotationId)
        }
        else{
            val x = y(1)
            x.toLowerCase match {
             case "org" => this(ContextLabel.Organ, annotationId)
             case "cline" => this(ContextLabel.CellLine, annotationId)
             case "ct" => this(ContextLabel.CellType, annotationId)
            }
        }



      case _ => this(ContextLabel.UNDETERMINED, annotationId)
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
            println(s"DEBUG: Event without context in $directory")
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
        val ois = new ObjectInputStream(new FileInputStream(ppFile)) {
          override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
            try { Class.forName(desc.getName, false, getClass.getClassLoader) }
            catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
          }
        }
        Some(ois.readObject.asInstanceOf[PreAnnotatedDoc])
      }
      else{
        None
      }
    }

    ArticleAnnotations(directory, sentences, events, context, standoff, preprocessed)
  }
}
