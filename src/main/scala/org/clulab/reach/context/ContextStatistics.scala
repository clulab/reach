/** Counts statistics of the context assigned to mentions of a paper **/

package org.clulab.reach.context

import java.io._
import collection.mutable.StringBuilder
import org.clulab.reach.mentions._

class ContextStatistics(val reachMentions:Seq[BioMention]){

  val contextMentions = reachMentions.filter{
    case m:BioTextBoundMention => ContextEngine.isContextMention(m)
    case _ => false
  }

  val eventMentions = reachMentions.filter{
    case e:BioEventMention => true
    case _ => false
  }

  // List of tuples (CtxClass, CtxType)
  lazy val associatedContexts:Seq[(String, String)] = eventMentions.flatMap{
    e =>
      e.context match {
        case Some(context) => context.toList.flatMap{ case(cls, elems) => elems.map(e => (cls, e))}
        case None => Nil
      }
  }

  // Context mention statistics
  lazy val mentionsPerCtxClass:Map[String, Int] = contextMentions.groupBy(_.label).mapValues(_.size)

  lazy val mentionsPerCtxType:Map[String, Int] = contextMentions.groupBy(_.nsId).mapValues(_.size)

  lazy val uniqueCtxTypes:Seq[String] = mentionsPerCtxType.keys.toSeq

  // Context association statistics
  lazy val associationPerCtxClass:Map[String, Int] = associatedContexts.groupBy(_._1).mapValues(_.size)

  lazy val associationPerCtxType:Map[String, Int] = associatedContexts.groupBy(_._2).mapValues(_.size)

  lazy val uniqueAssociatedCtxTypes:Seq[String] = associationPerCtxType.keys.toSeq

  def save(f:File){
    val pw = new PrintWriter(f)

    pw.println("mentionsPerCtxClass")
    for((k, v) <- mentionsPerCtxClass){
      pw.println(s"$k\t$v")
    }
    pw.println

    pw.println("mentionsPerCtxType")
    for((k, v) <- mentionsPerCtxType){
      pw.println(s"$k\t$v")
    }
    pw.println

    pw.println("uniqueCtxTypes")
    for(t <- uniqueCtxTypes){
      pw.println(s"$t")
    }
    pw.println

    pw.println("associationPerCtxClass")
    for((k, v) <- associationPerCtxClass){
      pw.println(s"$k\t$v")
    }
    pw.println

    pw.println("associationPerCtxType")
    for((k, v) <- associationPerCtxType){
      pw.println(s"$k\t$v")
    }
    pw.println

    pw.println("uniqueAssociatedCtxTypes")
    for(t <- uniqueAssociatedCtxTypes){
      pw.println(s"$t")
    }
    pw.println

    pw.println("#ofEvents")
    pw.println(s"${eventMentions.size}")

    pw.close
  }
}
