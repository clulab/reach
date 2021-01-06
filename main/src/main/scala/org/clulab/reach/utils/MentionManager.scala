package org.clulab.reach.utils

import java.io._
import scala.collection.mutable.MutableList
import scala.util.hashing.MurmurHash3._
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.reach.context._
import org.clulab.reach.mentions._


/**
  * Defines methods used to manipulate, cache, and output Mentions.
  *   Written by Tom Hicks. 4/3/2015.
  *   Last Modified: Rename is event mention method for clarity.
  */
class MentionManager {

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
  def preferredLabel (mention:Mention): String = mention match {
    case d: Display => d.displayLabel
    case other => other.label
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
    val mType = mention.getClass.toString.split("""\.""").last
    val label = preferredLabel(mention)
    mStrings += s"${indent}${mType}: S${mention.sentence}/${mention.startOffset}/${mention.endOffset}: ${label}"
    mStrings += s"${indent}text: ${mention.text}"
    mStrings += s"${indent}labels: ${mention.labels}"
    mStrings += s"${indent}foundBy: ${mention.foundBy}"

    mention match {
      case tbm: TextBoundMention =>
        val bioMen = mention.toBioMention
        if (bioMen.isGrounded) {
          if (bioMen.hasCandidates) {
            val subdent = ("  " * (level+1))
            mStrings += s"${indent}candidates:"
            bioMen.candidates.get.foreach { c => mStrings += s"${subdent}${c}" }
          }
          mStrings += s"${indent}grounding: ${bioMen.grounding.get}"
        }

      case evm: EventMention =>
        if (evm.isInstanceOf[BioEventMention])
          mStrings += s"${indent}is-direct: ${evm.asInstanceOf[BioEventMention].isDirect}"
        mStrings += s"${indent}trigger:"
        mStrings ++= mentionToStrings(evm.trigger, level+1)
        evm.arguments foreach {
          case (k,vs) => {
            mStrings += s"${indent}${k} (${vs.length}):"
            for (v <- vs) {
              mStrings ++= mentionToStrings(v, level+1)
            }
          }
        }

      case rm: RelationMention =>
        rm.arguments foreach {
          case (k,vs) => {
            mStrings += s"${indent}${k} (${vs.length}):"
            for (v <- vs) {
              mStrings ++= mentionToStrings(v, level+1)
            }
          }
        }

      case _ =>
        mStrings += s"${indent}UNKNOWN MENTION TYPE"
    }

    // postprocessing common to participants of match above:

    if (mention.isInstanceOf[BioMention]) {
      val bioMention = mention.toBioMention
      mStrings ++= modificationsToStrings(bioMention, level)
      if (bioMention.context.isDefined)
        mStrings ++= contextToStrings(bioMention.context.get, level)
    }

    mention.toCorefMention.antecedent foreach { ante =>
      mStrings += s"${indent}antecedent:"
      mStrings ++= mentionToStrings(ante.asInstanceOf[CorefMention], level+1)
    }

    if (level == 0) mStrings += ("=" * 80)

    // all done at this level: return accumulated list of output strings
    return mStrings.toList
  }


  /** Return a list of strings representing the context properties from the
      given context map. */
  private def contextToStrings (ctxMap:ContextMap, level:Integer): List[String] = {
    val mStrings:MutableList[String] = MutableList[String]()
    val headIndent = ("  " * level)
    val indent = ("  " * (level+1))
    if (ctxMap.nonEmpty) {
      mStrings += s"${headIndent}context:"
      ctxMap foreach { ctxEntry =>
        mStrings += s"${indent}${ctxEntry._1}: ${ctxEntry._2}"
      }
    }
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
        case Mutant(evidence, foundBy) =>
          mStrings += s"${indent}mutant: ${evidence.text}"
        case Negation(evidence) =>
          mStrings += s"${indent}negation: ${evidence.text}"
        case ptm:PTM =>
          mStrings ++= ptmToStrings(ptm, level+1)

        case KDtrigger(evidence) =>
          mStrings += s"${indent}KD-trigger: ${evidence.text}"
        case KOtrigger(evidence) =>
          mStrings += s"${indent}KO-trigger: ${evidence.text}"
        case DNtrigger(evidence) =>
          mStrings += s"${indent}DN-trigger: ${evidence.text}"
        case OEtrigger(evidence) =>
          mStrings += s"${indent}OE-trigger: ${evidence.text}"
        case CHEMtrigger(evidence) =>
          mStrings += s"${indent}CHEM-trigger: ${evidence.text}"
        case UnassignedTrigger(evidence) =>
          mStrings += s"${indent}Unassigned-trigger: ${evidence.text}"

        case _ => ()
      }
    }
    return mStrings.toList
  }

  /** Return a list of strings representing the PTM case class (part of the modifications),
    * indented at the given indentation level. */
  private def ptmToStrings (ptm:PTM, level:Integer): List[String] = {
    val mStrings:MutableList[String] = MutableList[String]()
    val headIndent = ("  " * level)
    val indent = ("  " * (level+1))
    mStrings += s"${headIndent}PTM: ${ptm.label}"
    if (ptm.evidence.isDefined)
      mStrings += s"${indent}evidence: ${ptm.evidence.get.text}"
    mStrings += s"${indent}negated: ${ptm.negated}"
    if (ptm.site.isDefined)
      mStrings ++= mentionToStrings(ptm.site.get, level+2)
    return mStrings.toList
  }

}


/** Companion object defining constant vals and read-only functions. */
object MentionManager {

  def hasFeatures(mention:BioMention):Boolean = {
    mention.modifications.foreach(feat => {
      if(feat.isInstanceOf[Mutant] || feat.isInstanceOf[PTM])
        return true
    })
    return false
  }


  def isEventOrRelationMention (mention:Mention): Boolean = {
    mention.isInstanceOf[EventMention] || mention.isInstanceOf[RelationMention]
  }

  def isRelationMention (mention:Mention): Boolean = mention.isInstanceOf[RelationMention]

  def isTextBoundMention (mention:Mention): Boolean = mention.isInstanceOf[TextBoundMention]

  def isEventSite (mention:BioMention): Boolean =
    mention.modifications.exists(mod => mod.isInstanceOf[EventSite])


  def isHypothesized (mention:BioMention): Boolean =
    mention.modifications.exists(isHypothesis)

  def isHypothesis (mod:Modification) = mod.isInstanceOf[Hypothesis]


  def isMutated (mention:BioMention): Boolean =
    mention.modifications.exists(isMutation)

  def isMutation (mod:Modification) = mod.isInstanceOf[Mutant]


  def isNegated (mention:BioMention): Boolean = {
    mention.modifications.exists(isNegation)
  }

  def isNegation (mod:Modification) = mod.isInstanceOf[Negation]

  def sentenceEndCharacterOffset (doc:Document, sentOffset:Int): Int = {
    doc.sentences(sentOffset).endOffsets.last
  }

  def sentenceStartCharacterOffset (doc:Document, sentOffset:Int): Int = {
    doc.sentences(sentOffset).startOffsets.head
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
