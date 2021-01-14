package org.clulab.reach.export.indexcards

import java.io.File
import java.util.Date
import java.util.regex.Pattern

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.Mention
import org.clulab.reach.ReachConstants._
import org.clulab.reach.{FriesEntry, display}
import org.clulab.reach.export.JsonOutputter._
import org.clulab.reach.export.{JsonOutputter, OutputDegrader}
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions._
import org.clulab.reach.mentions.serialization.json.mentionToJSON
import org.clulab.reach.utils.MentionManager
import IndexCardOutput._

/**
  * Defines classes and methods used to build and output the index card format.
  *   Written by: Mihai Surdeanu. 8/27/2015.
  *   Last Modified: Update for renamed method.
  */
class IndexCardOutput extends JsonOutputter with LazyLogging {

  /**
   * Returns the given mentions in the index-card JSON format, as one big string.
   * All index cards are concatenated into a single JSON string.
   */
  override def toJSON (paperId:String,
                       allMentions:Seq[Mention],
                       paperPassages:Seq[FriesEntry],
                       startTime:Date,
                       endTime:Date,
                       outFilePrefix:String): String = {
    val otherMetaData = extractOtherMetaData(paperPassages)
    val cards = mkCards(paperId, allMentions, startTime, endTime, otherMetaData)
    val uniModel:PropMap = new PropMap
    uniModel("cards") = cards
    writeJsonToString(uniModel)
  }

  /**
   * Writes the given mentions to output files in IndexCard JSON format.
   * Separate output files are written for sentences, entities, and events.
   * Each output file is prefixed with the given prefix string.
   */
  override def writeJSON (paperId:String,
                          allMentions:Seq[Mention],
                          paperPassages:Seq[FriesEntry],
                          startTime:Date,
                          endTime:Date,
                          outFilePrefix:String
  ): Unit = {
    // we create a separate directory for each paper, and store each index card as a separate file
    val dir = new File(outFilePrefix)
    if(! dir.exists()) {
      if (! dir.mkdirs()) {
        throw new RuntimeException(s"ERROR: failed to create output directory $outFilePrefix!")
      }
    } else {
      // delete all files in this directory
      val files = dir.listFiles()
      for(file <- files) file.delete()
    }

    val otherMetaData = extractOtherMetaData(paperPassages)

    // index cards are generated here
    val cards = mkCards(paperId, allMentions, startTime, endTime, otherMetaData)

    // save one index card per file
    var count = 1
    for(card <- cards) {
      val outFile = new File(outFilePrefix + File.separator + mkIndexCardFileName(paperId, count) + ".json")
      writeJsonToFile(card, outFile)
      count += 1
    }
  }

  /** Return a new filename for each card. */
  def mkIndexCardFileName(paperId:String, count:Int):String =
    s"$paperId-$ORGANIZATION-$RUN_ID-$count"


  /**
    * Creates annotations from all events read from this paper.
    * This needs to be done in 2 passes:
    *   1) save recursive events
    *   2) save simple events that are not part of recursive events
    */
  def mkCards(paperId:String,
              allMentions:Seq[Mention],
              startTime:Date,
              endTime:Date,
              otherMetaData:Map[String, String]): Iterable[PropMap] = {
    val cards = new ListBuffer[PropMap]

    // dereference all coreference mentions:
    val derefedMentions = allMentions.map(m => m.antecedentOrElse(m.toCorefMention))

    // keeps just events:
    val eventMentions = derefedMentions.filter(MentionManager.isEventOrRelationMention)
    // flatten mentions, deduplicate, etc.
    val flattenedMentions = OutputDegrader.prepareForOutput(eventMentions).map(_.toCorefMention)
    // keeps track of simple events that participate in regulations
    val simpleEventsInRegs = new mutable.HashSet[Mention]()

    // first, print all regulation events
    for (mention <- flattenedMentions) {
      if (REGULATION_EVENTS.contains(mention.label)) {
        val card = mkRegulationIndexCard(mention, simpleEventsInRegs)
        card.foreach(c => {
          addMeta(c, mention, paperId, startTime, endTime, otherMetaData)
          cards += c
        })
      }
    }

    // now, print everything else that wasn't printed already
    for (mention <- flattenedMentions) {
      if (! REGULATION_EVENTS.contains(mention.label) &&
          ! simpleEventsInRegs.contains(mention))
      {
        val card = mkIndexCard(mention)
        card.foreach(c => {
          addMeta(c, mention, paperId, startTime, endTime, otherMetaData)
          cards += c
        })
      }
    }

    cards.toList
  }


  /** Main annotation dispatcher method. */
  def mkIndexCard(mention: CorefMention): Option[PropMap] = {
    val eventType = mkEventType(mention)
    val exOpt = eventType match {
      case "protein-modification" => mkModificationIndexCard(mention)
      case "complex-assembly" => mkBindingIndexCard(mention)
      case "translocation" => mkTranslocationIndexCard(mention)
      case "activation" => mkActivationIndexCard(mention)
      case "regulation" => throw new RuntimeException("ERROR: regulation events must be saved before!")
      // FIXME: not sure if this is how these events should be handled
      case "transcription" => mkSimpleEventIndexCard(mention, "transcribes")
      // FIXME: this isn't a real solution
      case "amount" => mkSimpleEventIndexCard(mention, mention.label)
      // FIXME: what would this conversion look like?
      case "conversion" => mkSimpleEventIndexCard(mention, mention.label)
      case _ =>
        val json = mentionToJSON(mention, pretty = true)
        val message = s"""Event type "$eventType" is not supported for indexcard output:\n$json"""
        // throw new RuntimeException(message)
        logger.warn(message)
        None
    }
    exOpt.map { ex =>
      mkHedging(ex, mention)
      mkContext(ex, mention)
      new PropMap() += ("extracted_information" -> ex)
    }
  }

  /** Add the properties of the given context map to the given property map. */
  def mkContext (f:PropMap, mention:CorefMention): Unit = {
    if (mention.context.exists(_.nonEmpty))
      f("context") = mention.context.get
  }

  def mkArgument(arg:CorefMention):Any = {
    val derefArg = arg.antecedentOrElse(arg)
    val argType = mkArgType(derefArg)
    argType match {
      case "entity" => mkSingleArgument(derefArg)
      case "complex" => mkComplexArgument(derefArg)
      case _ => {
        throw new RuntimeException(s"ERROR: argument type '$argType' not supported!")
      }
    }
  }

  def mkSingleArgument(arg:CorefMention):PropMap = {
    val f = new PropMap
    f("entity_text") = arg.text
    f("entity_type") = arg.displayLabel.toLowerCase

    // participants should always be grounded here! (even if only to an UAZID)
    // assert(arg.isGrounded, s"Argument '${arg.text}' should be grounded at this point!")
    if (!arg.isGrounded) {
      logger.error(s"Failed to ground argument ${arg.text}!")
    }
    if (arg.isGrounded) f("identifier") = mkIdentifier(arg.grounding.get)

    if (MentionManager.hasFeatures(arg)) f("features") = mkFeatures(arg)
    // TODO: we do not compare against the model; assume everything exists, so the validation works
    f("in_model") = true
    f
  }

  def mkFeatures(arg:CorefMention):FrameList = {
    val fl = new FrameList
    arg.modifications.foreach {
      case ptm: PTM =>
        fl += mkPTMFeature(ptm)
      case mut: Mutant =>
        //println(s"argument and mutation: ${arg.text} ${mut.evidence.text}")
        fl += mkMutantFeature(mut)
      case _ => // there may be other features that are not relevant for the index card output
    }
    fl
  }

  def mkPTMFeature(ptm:PTM):PropMap = {
    val f = new PropMap
    f("feature_type") = "modification"
    f("modification_type") = ptm.label.toLowerCase
    if(ptm.site.isDefined)
      f("position") = ptm.site.get.text
    f
  }

  def mkMutantFeature(m:Mutant):PropMap = {
    val f = new PropMap
    f("feature_type") = "mutation"
    if(m.evidence.text.length > 0) {
      val ev = m.evidence.text
      f("evidence") = ev

      if(ev.toLowerCase.startsWith("mutant") ||
         ev.toLowerCase.startsWith("mutation")) {
        addUnspecifiedMutation(f)
      } else {
        val lm = LETTER_MUTATION.matcher(ev)
        if(lm.matches()) {
          addMutation(f, ev, 0)
        } else {
          val ldm = LETTER_DIGIT_MUTATION.matcher(ev)
          if(ldm.find()) {
            addMutation(f, ldm.group(1), ldm.group(2).toInt)
          } else {
            // TODO: this should never happen
            addUnspecifiedMutation(f)
          }
        }
      }
    } else {
      addUnspecifiedMutation(f)
    }
    f
  }

  def addMutation(f:PropMap, base:String, site:Int): Unit = {
    f("to_base") = base
    f("site") = site
  }
  def addUnspecifiedMutation(f:PropMap): Unit = {
    addMutation(f, "A", 0)
  }


  def mkComplexArgument(complex:CorefMention):FrameList = {
    val fl = new FrameList
    val participants = complex.arguments.get("theme")
    if (participants.isEmpty)
      throw new RuntimeException("ERROR: cannot have a complex with 0 participants!")
    participants.get.foreach(part => {
      fl += mkSingleArgument(part.antecedentOrElse(part.toCorefMention))
    })
    fl
  }

  def mkIdentifier(grounding:KBResolution):String = {
    grounding.namespace + ":" + grounding.id
  }

  def mkEventModification(mention:CorefMention):PropMap = {
    val f = new PropMap
    f("modification_type") = mention.displayLabel.toLowerCase
    val position = mention.siteArgs
    if (position.isDefined)
      f("position") = position.get.head.text
    f
  }

  /** Creates a card for a simple, modification event */
  def mkModificationIndexCard(mention:CorefMention,
                              positiveModification:Boolean = true):Option[PropMap] = {
    val f = new PropMap

    // a modification event will have exactly one theme
    f("participant_b") = mkArgument(mention.themeArgs.get.head.toCorefMention)

    if (positiveModification)
      f("interaction_type") = "adds_modification"
    else
      f("interaction_type") = "inhibits_modification"

    if (mention.isInstanceOf[BioEventMention])
      f("is_direct") = mention.asInstanceOf[BioEventMention].isDirect

    val mods = new FrameList
    mods += mkEventModification(mention)
    f("modifications") = mods
    Some(f)
  }

  def mkHedging(f:PropMap, mention:CorefMention) {
    f("negative_information") = MentionManager.isNegated(mention)
    f("hypothesis_information") = MentionManager.isHypothesized(mention)
  }

  /** Creates a card for a simple events */
  def mkSimpleEventIndexCard(mention: CorefMention, label: String): Option[PropMap] = {
    val f = new PropMap
    f("interaction_type") = label
    f("participant_b") = mkArgument(mention.themeArgs.get.head.toCorefMention)

    Some(f)
  }

  /** Creates a card for a regulation event */
  def mkRegulationIndexCard(mention:CorefMention,
                            simpleEventsInRegs:mutable.HashSet[Mention]):Option[PropMap] = {

    val controlleds = mention.controlledArgs
    if (controlleds.isEmpty)
      throw new RuntimeException("ERROR: a regulation event must have a controlled argument!")
    val controlled = controlleds.get.head.toCorefMention

    // INDEX CARD LIMITATION:
    // We only know how to output regulations when controlled is a modification!
    val eventType = mkEventType(controlled)
    if (eventType != "protein-modification") return None

    // do not output this event again later, when we output single modifications
    simpleEventsInRegs += controlled

    // populate participant_b and the interaction type from the controlled event
    val posMod = mention.label match {
      case "Positive_regulation" => true
      case "Negative_regulation" => false
      case _ => throw new RuntimeException(s"ERROR: unknown regulation event ${mention.label}!")
    }
    val ex = mkModificationIndexCard(controlled, positiveModification = posMod)

    // add participant_a from the controller
    val controllers = mention.controllerArgs
    if (controllers.isEmpty)
      throw new RuntimeException("ERROR: an activation event must have a controller argument!")
    val controller = controllers.get.head.toCorefMention
    ex.get("participant_a") = mkArgument(controller)

    // add hedging and context
    mkHedging(ex.get, mention)
    mkContext(ex.get, mention)

    // all this becomes the extracted_information block
    val f = new PropMap
    f("extracted_information") = ex.get
    Some(f)
  }


  /** Creates a card for an activation event */
  def mkActivationIndexCard(mention:CorefMention):Option[PropMap] = {
    val f = new PropMap

    // a modification event must have exactly one controller and one controlled
    val controllers = mention.controllerArgs
    if (controllers.isEmpty)
      throw new RuntimeException("ERROR: an activation event must have a controller argument!")
    f("participant_a") = mkArgument(controllers.get.head.toCorefMention)

    val controlleds = mention.controlledArgs
    if (controlleds.isEmpty)
      throw new RuntimeException("ERROR: an activation event must have a controlled argument!")
    f("participant_b") = mkArgument(controlleds.get.head.toCorefMention)

    if (mention.label == "Positive_activation")
      f("interaction_type") = "increases_activity"
    else
      f("interaction_type") = "decreases_activity"
    Some(f)
  }


  /** Creates a card for a complex-assembly event */
  def mkBindingIndexCard(mention:CorefMention):Option[PropMap] = {
    val f = new PropMap
    f("interaction_type") = "binds"

    val participants = mention.themeArgs.get

    // a Binding events must have at least 2 arguments
    f("participant_a") = mkArgument(participants.head.toCorefMention)

    val participantsTail = participants.tail
    if (participantsTail.size == 1) {
      f("participant_b") = mkArgument(participantsTail.head.toCorefMention)
    }
    else if (participantsTail.size > 1) {
      // store them all as a single complex.
      // INDEX CARD LIMITATION: This is ugly, but there is no other way with the current format
      val fl = new FrameList
      participantsTail.foreach(part => {
        fl += mkSingleArgument(part.toCorefMention)
      })
      f("participant_b") = fl
    }
    else {
      throw new RuntimeException("ERROR: A complex assembly event must have at least 2 participants!")
    }

    // add binding sites if present
    val sites = mention.siteArgs
    if (sites.isDefined) {
      val fl = new StringList
      for (site <- sites.get) {
        fl += site.text
      }
      f("binding_site") = fl
    }

    Some(f)
  }


  /** Creates a card for a translocation event */
  def mkTranslocationIndexCard(mention:CorefMention):Option[PropMap] = {
    val f = new PropMap
    f("interaction_type") = "translocates"
    f("participant_b") = mkArgument(mention.themeArgs.get.head.toCorefMention)

    val sources = mention.sourceArgs
    if (sources.isDefined)
      addLocation(f, "from", sources.get.head.toCorefMention)

    val destinations = mention.destinationArgs
    if (destinations.isDefined)
      addLocation(f, "to", destinations.get.head.toCorefMention)

    Some(f)
  }


  def addLocation(f:PropMap, prefix:String, loc:CorefMention): Unit = {
    f(prefix + "_location_id") = mkIdentifier(loc.grounding.get)
    f(prefix + "_location_text") = loc.text
  }

  def addMeta(f:PropMap,
              mention:CorefMention,
              paperId:String,
              startTime:Date,
              endTime:Date,
              otherMetaData:Map[String, String]): Unit = {
    f("submitter") = COMPONENT
    f("score") = 0
    f("pmc_id") = if(paperId.startsWith("PMC")) paperId.substring(3) else paperId
    f("reader_type") = "machine"
    f("reading_started") = startTime
    f("reading_complete") = endTime
    otherMetaData.foreach { case(k, v) => f(k) = v } // add other meta data key/value pairs
    if (mention.isInstanceOf[BioEventMention])
      f("trigger") = mention.asInstanceOf[BioEventMention].trigger.text
    val ev = new StringList
    ev += mention.text
    f("evidence") = ev
    // TODO: we do not compare against the model; assume everything is new
    f("verbose_text") = display.cleanVerbose(mention.sentenceObj.getSentenceText)
    f("model_relation") = "extension"
  }

}


object IndexCardOutput {
  val LETTER_DIGIT_MUTATION = Pattern.compile("^([ACDEFGHIKLMNPQRSTVWYacdefghiklmnpqrstvwy]+)(\\d+)")
  val LETTER_MUTATION = Pattern.compile("^([ACDEFGHIKLMNPQRSTVWYacdefghiklmnpqrstvwy]+)$")
}
