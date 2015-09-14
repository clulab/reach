package edu.arizona.sista.reach.extern.export

import java.io._

import scala.collection.mutable.MutableList

import scala.util.hashing.MurmurHash3._

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._

/**
  * Defines methods used to manipulate, cache, and output Mentions.
  *   Written by Tom Hicks. 4/3/2015.
  *   Last Modified: Change package to parent export.
  */
class MentionManager {
  // Constants:

  // mention numbering sequence counter
  private val mSeqNum = new IncrementingCounter()

  // cache for mentions, keyed by a hash key computed from the mention
  protected val roots = scala.collection.mutable.Map[Int, MentionCacheValue]()

  // define an implicit ordering for mentions by sentence/startOffset/endOffset
  protected implicit val MentionOrdering = Ordering.by { mention:Mention =>
    (mention.sentence, mention.startOffset, mention.endOffset)
  }


  //
  // Public API:
  //

  def isEventSite(mention:BioMention):Boolean =
    mention.modifications.exists(mod => mod.isInstanceOf[EventSite])

  def isHypothesized(mention:BioMention):Boolean =
    mention.modifications.exists(mod => mod.isInstanceOf[Hypothesis])

  def isMutated(mention:BioMention):Boolean =
    mention.modifications.exists(mod => mod.isInstanceOf[Mutant])

  def isNegated(mention:BioMention):Boolean =
    mention.modifications.exists(mod => mod.isInstanceOf[Negation])


  def mergedEvents (): Seq[Mention] = {
    roots.values.toSeq.sortBy(_.seqNum).map(_.mention)
  }

  def mergeEventMentions (mentions:Seq[Mention]) = {
    // use only mentions labeled as Events for the roots of the forest trees:
    mentions.filter(_.matches("Event")).foreach { mention =>
      val hash = computeHash(mention)
      roots.getOrElseUpdate(hash, new MentionCacheValue(mSeqNum.next, hash, mention))
    }
  }

  /** Generates a representation of the given mention as a list of strings. */
  def mentionToStrings (mention:Mention): List[String] = {
    return mentionToStrings(mention, 0)
  }


  /** Return the named argument from the arguments of the given mention. */
  def namedArgument (mention:Mention, argName:String): Option[Seq[Mention]] = {
    mention.arguments.get(argName)
  }
  def causeArgs       (mention:Mention): Option[Seq[Mention]] = namedArgument(mention, "cause")
  def controlledArgs  (mention:Mention): Option[Seq[Mention]] = namedArgument(mention, "controlled")
  def controllerArgs  (mention:Mention): Option[Seq[Mention]] = namedArgument(mention, "controller")
  def destinationArgs (mention:Mention): Option[Seq[Mention]] = namedArgument(mention, "destination")
  def goalArgs        (mention:Mention): Option[Seq[Mention]] = namedArgument(mention, "goal")
  def proteinArgs     (mention:Mention): Option[Seq[Mention]] = namedArgument(mention, "protein")
  def themeArgs       (mention:Mention): Option[Seq[Mention]] = namedArgument(mention, "theme")
  def siteArgs        (mention:Mention): Option[Seq[Mention]] = namedArgument(mention, "site")
  def sourceArgs      (mention:Mention): Option[Seq[Mention]] = namedArgument(mention, "source")


  /** Output a string representation of the mentions selected by the given label string
    * to the given output stream.
    * NB: This method closes the given output stream when done!
    */
  def outputSelectedMentions (mentionType:String,
                              mentions:Seq[Mention],
                              outFile:File): Unit = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
    mentions.filter(_.matches(mentionType)).foreach { mention =>
      mentionToStrings(mention).foreach { str => out.println(str) }
    }
    out.flush()
    out.close()
  }

  /** Return the preferred label string for display. */
  def preferredLabel (mention:Mention): String = {
    return if (mention.isInstanceOf[Display]) mention.asInstanceOf[Display].displayLabel
           else mention.label
  }

  /** Sort the given mentions and return a sequence of string representations for them. */
  def sortMentionsToStrings (mentions:Seq[Mention]): Seq[String] ={
    // return mentions.sorted.flatMap(mentionToStrings) // sorts via implicit ordering defined above
    return mentions.sortBy(m => (m.sentence,m.startOffset,m.endOffset)).flatMap(mentionToStrings)
  }


  //
  // Private Methods
  //

  private def computeHash (mention:Mention): Int = {
    // val hash = computeHash(mention, symmetricSeed)
    // return finalize(hash)
    computeHash(mention, symmetricSeed)
  }

  private def computeHash (mention:Mention, hash:Int): Int = {
    mention match {
      case mention: TextBoundMention =>
        mix(hash, stringHash("TEXT" + mention.label + mention.text))
      case mention: EventMention =>
        val h1 = mix(hash, stringHash("EVENT" + mention.label))
        mix(h1, unorderedHash(mention.arguments.filterNot(ignoreArg).map(computeHash(_,0))))
      case mention: RelationMention =>
        val h1 = mix(hash, stringHash("EVENT" + mention.label))
        mix(h1, unorderedHash(mention.arguments.filterNot(ignoreArg).map(computeHash(_,0))))
      case _ => 0
    }
  }

  private def computeHash (entry:Tuple2[String,Seq[Mention]], hash:Int): Int = {
    mix(mix(hash, stringHash(entry._1)),             // add argument name (key) to hash
        orderedHash(entry._2.map(computeHash(_,0)))) // recursively add mentions of this argument
  }

  /** Filter to decide which mention arguments to ignore. */
  private def ignoreArg (entry:Tuple2[String, Seq[Mention]]): Boolean = {
    (entry._1 == "trigger")                 // ignore the trigger argument
  }


  /** Return a list of strings representing the given mention at the given indentation level. */
  private def mentionToStrings (mention:Mention, level:Integer): List[String] = {
    val mStrings:MutableList[String] = MutableList[String]()
    val indent = ("  " * level)
    val label = preferredLabel(mention)
    mention match {
      case mention: TextBoundMention =>
        mStrings += s"${indent}TextBoundMention: S${mention.sentence}/${mention.startOffset}/${mention.endOffset}: ${label}"
        mStrings += s"${indent}text: ${mention.text}"
        if (mention.toBioMention.isGrounded)
          mStrings += s"${indent}xref: ${mention.toBioMention.xref.get}"

      case mention: EventMention =>
        mStrings += s"${indent}EventMention: S${mention.sentence}/${mention.startOffset}/${mention.endOffset}: ${label}"
        mStrings += s"${indent}text: ${mention.text}"
        mStrings += s"${indent}trigger:"
        mStrings ++= mentionToStrings(mention.trigger, level+1)
        mention.arguments foreach {
          case (k,vs) => {
            mStrings += s"${indent}${k} (${vs.length}):"
            for (v <- vs) {
              mStrings ++= mentionToStrings(v, level+1)
            }
          }
        }

      case mention: RelationMention =>
        mStrings += s"${indent}RelationMention: S${mention.sentence}/${mention.startOffset}/${mention.endOffset}: ${label}"
        mStrings += s"${indent}text: ${mention.text}"
        mention.arguments foreach {
          case (k,vs) => {
            mStrings += s"${indent}${k} (${vs.length}):"
            for (v <- vs) {
              mStrings ++= mentionToStrings(v, level+1)
            }
          }
        }
      case _ => ()
    }

    // postprocessing common to all participants of match above:
    if (mention.isInstanceOf[BioEventMention])
      mStrings ++= modificationsToStrings(mention.toBioMention, level)
    if (level == 0) mStrings += ("=" * 80)

    // all done at this level: return accumulated list of output strings
    return mStrings.toList
  }


  /** Return a list of strings representing the modifications to the given mention at
    * the given indentation level. */
  private def modificationsToStrings (biomention:BioMention, level:Integer): List[String] = {
    val mStrings:MutableList[String] = MutableList[String]()
    val headIndent = ("  " * level)
    val indent = ("  " * (level+1))
    if (biomention.isModified) {
      mStrings += s"${headIndent}modifications (${biomention.modifications.size}):"
      biomention.modifications.foreach {
        case EventSite(site) =>
          mStrings += s"${indent}event-site: ${site.text}"
        case Hypothesis(evidence) =>
          mStrings += s"${indent}hypothesis: ${evidence.text}"
        case Mutant(evidence) =>
          mStrings += s"${indent}mutant: ${evidence.text}"
        case Negation(evidence) =>
          mStrings += s"${indent}negation: ${evidence.text}"
        case PTM(modLabel, evidence, site) =>
          val evText = if (evidence.isDefined) evidence.get.text else ""
          mStrings += s"${indent}PTM: ${evText}"
          if (site.isDefined)
            mStrings ++= mentionToStrings(site.get, level+1)
        case _ => ()
      }
    }
    return mStrings.toList
  }

}


class MentionCacheValue (val seqNum:Int, val hashValue:Int, val mention:Mention)

class IncrementingCounter {
  protected var cntr:Int = 0
  def current(): Int = { cntr }
  def next(): Int = {
    cntr += 1
    return cntr
  }
}
