package edu.arizona.sista.odin.extern.export.fries

import java.io._
import java.util.Date
import edu.arizona.sista.utils.DateUtils

import scala.collection.mutable.Map
import scala.collection.mutable.MutableList
import scala.collection.mutable.Set

import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

import edu.arizona.sista.processors._
import edu.arizona.sista.odin._

/**
  * Defines classes and methods used to build and output FRIES models.
  *   Written by Tom Hicks. 4/30/2015.
  *   Last Modified: Cleanups.
  */
class FriesOutput {
  type Memoized = scala.collection.mutable.HashSet[Mention]
  type PropMap  = scala.collection.mutable.HashMap[String, Any]
  type CardList = scala.collection.mutable.MutableList[PropMap]  // has O(c) append

  // Constants:
  val AllEvents = Set("Acetylation", "Binding", "Farnesylation",
                      "Glycosylation", "Hydrolysis", "Hydroxylation",
                      "Methylation", "Negative_regulation", "Phosphorylation",
                      "Positive_regulation", "Ribosylation", "Sumoylation",
                      "Transport", "Ubiquitination")
  val AssumedProteins = Set("Gene_or_gene_product", "Protein", "Protein_with_site")
  val Now = DateUtils.formatUTC(new Date())
  val RootEvents = Set("Negative_regulation", "Positive_regulation")

  // used by json output serialization:
  implicit val formats = org.json4s.DefaultFormats

  // create mention manager and cache
  protected val mentionMgr = new MentionManager()


  //
  // Public API:
  //

  /** Output a JSON object representing the FRIES output for the given mentions. */
  def toJSON (allMentions:Seq[Mention], outFile:File) = {
    val fries:PropMap = new PropMap
    val mentions = allMentions.filter(_.isInstanceOf[EventMention])
    val rootChildren = memoizeRootChildren(mentions)
    // showMemoization(rootChildren)
    val cards = new CardList
    mentions.foreach { mention =>
      if (!rootChildren.contains(mention)) {
        val card = doMention(mention)
        cards += card
      }
    }
    fries("cards") = cards
    writeJsonToFile(fries, outFile)
  }


  //
  // Private Methods
  //

  /** Return a new index card (map) initialized with the (repeated) document information. */
  private def beginNewCard (mention:Mention): PropMap = {
    val doc:Document = mention.document
    val card = new PropMap
    card("pmc_id") = doc.id.getOrElse("DOC-ID-MISSING")
    card("reading_started") = Now
    card("reading_ended") = Now
    card("submitter") = "UAZ"
    card("reader_type") = "machine"
    val extracted = new PropMap
    extracted("negative_information") = false
    card("extracted_information") = extracted
    card("evidence") = mention.text
    return card
  }

  /** Dispatch on and process the given mention, returning its information in a properties map. */
  private def doMention (mention:Mention): PropMap = {
    mention.label match {                   // dispatch on mention type
      case "Acetylation" => null
      case "Binding" => doBinding(mention, true)
      case "Degradation" => null
      case "Exchange" => null
      case "Expression" => null
      case "Farnesylation" => null
      case "Glycosylation" => null
      case "Hydrolysis" => null
      case "Hydroxylation" => null
      case "Methylation" => null
      case "Negative_regulation" => null
      case "Phosphorylation" => doPhosphorylation(mention, true)
      case "Positive_regulation" => doPositiveRegulation(mention, true)
      case "Ribosylation" => null
      case "Sumoylation" => null
      case "Translation" => null
      case "Transcription" => null
      case "Transport" => null
      case "Ubiquitination" => null
      case _ => null
    }
  }

  /** Return properties map for the given binding mention. */
  private def doBinding (mention:Mention, root:Boolean=false): PropMap = {
    val card = if (root) beginNewCard(mention) else new PropMap
    val themeArgs = mentionMgr.themeArgs(mention)
    if (themeArgs.isDefined) {
      val extracted:PropMap = card("extracted_information").asInstanceOf[PropMap]
      extracted("interaction_type") = "binds"
      val themes = themeArgs.get.map(doTextBoundMention(_))
      extracted("participant_b") = themes.size match {
        case 0 => null
        case 1 => themes.head
        case _ => themes
      }
      extracted("participant_b_site") = null // TODO: extract site when sites unbound

      val cause:Option[PropMap] = getCause(mention)
      extracted("participant_a") = cause.orNull
      if (cause.isDefined)
        extracted("participant_a_site") = null // TODO: extract site when sites unbound
      else
        extracted("participant_a_site") = null
    }
    return card
  }


  /** Return properties map for the given phosphorylation mention. */
  private def doPhosphorylation (mention:Mention, root:Boolean=false): PropMap = {
    val card = if (root) beginNewCard(mention) else new PropMap
    val themeArgs = mentionMgr.themeArgs(mention)
    if (themeArgs.isDefined) {
      val extracted:PropMap = card("extracted_information").asInstanceOf[PropMap]
      extracted("interaction_type") = "adds_modification"
      val themes = themeArgs.get.map(doTextBoundMention(_))
      extracted("participant_b") = themes.size match {
        case 0 => null
        case 1 => themes.head
        case _ => themes
      }

      val cause = getCause(mention)
      extracted("participant_a") = cause.orNull

      val aMod = new PropMap
      aMod("modification_type") = "phosphorylation"
      aMod("position") = getSite(mention).orNull
      val modList = new CardList
      modList += aMod
      extracted("modifications") = modList
    }
    return card
  }

  /** Return properties map for the given positive regulation mention. */
  private def doPositiveRegulation (mention:Mention, root:Boolean=false): PropMap = {
    val card = if (root) beginNewCard(mention) else new PropMap
    val controllerArgs = mentionMgr.controllerArgs(mention)
    val controlledArgs = mentionMgr.controlledArgs(mention)
    if (controllerArgs.isDefined && controlledArgs.isDefined) {
      val extracted:PropMap = card("extracted_information").asInstanceOf[PropMap]
      extracted("interaction_type") = "increases_activity"
      val controllerProps = doTextBoundMention(controllerArgs.get.head)
      val controlled = controlledArgs.get.head
      val themeArgs = mentionMgr.themeArgs(controlled)
      val controlledProps = themeArgs.map(osm => doTextBoundMention(osm.head)).orElse(null)
      controllerProps("features") = getFeatures(controlled)
      extracted("participant_a") = controllerProps
      extracted("participant_b") = controlledProps
    }
    return card
  }


  /** Return a properties map for the given single text bound mention. */
  private def doTextBoundMention (mention:Mention, context:String="protein"): PropMap = {
    val part = new PropMap
    part("entity_type") =
      if (AssumedProteins.contains(mention.label)) context
      else if (mention.label == "Simple_chemical") "chemical"
      else "BAD_TEXT_MENTION_LABEL"
    part("entity_text") = mention.text
    val ns = mention.xref.map(_.namespace).getOrElse("")
    val id = mention.xref.map(_.id).getOrElse("")
    part("identifier") = s"${ns}:${id}"
    return part
  }


  /** Process optional cause argument on the given mention, returning a properties map. */
  private def getCause (mention:Mention): Option[PropMap] = {
    val causeArgs = mentionMgr.causeArgs(mention)
    return causeArgs.map(osm => doTextBoundMention(osm.head))
  }


  /** Return a list of property maps of features extracted from the given mention.
    *  NB: currently only returning a list of 1 feature map.
    */
  private def getFeatures (mention:Mention): CardList = {
    val featureList = new CardList
    val props = new PropMap
    mention.label match {
      case "Binding" => {
        props("feature_type") = "binding"
        props("bound_to") = getText(mentionMgr.themeArgs(mention))
      }
      // case "Mutation" => "mutant",          // TODO: handle mutation features?
      case _ => {
        props("feature_type") = "modification"
        props("modification_type") = mention.label.toLowerCase
      }
    }
    featureList += props
    return featureList
  }


  /** Process optional site argument on the given mention, returning a site string option. */
  private def getSite (mention:Mention): Option[String] = {
    return getText(mentionMgr.siteArgs(mention))
  }

  /** Process the given mention argument, returning a text string option for the first arg. */
  private def getText (args:Option[Seq[Mention]]): Option[String] = {
    return if (args.isDefined) Some(args.get.head.text) else None
  }

  /** Remember all mentions reachable from the forest of root event mentions. */

  private def memoizeRootChildren (mentions:Seq[Mention]): Memoized = {
    val memoized = new Memoized
    mentions.filter(m => RootEvents.contains(m.label)).foreach{ mention =>
      for (mArg <- mention.arguments.values.flatten)
        memoizeRootChildren(mArg, memoized)
    }
    return memoized
  }

  /** Remember all descendant mentions reachable from the given mention. */
  private def memoizeRootChildren (child:Mention, memoized:Memoized): Memoized = {
    memoized += child                              // memoize this child
    for (mArg <- child.arguments.values.flatten)   // do all descendants
      memoizeRootChildren(mArg, memoized)
    return memoized
  }

  /** Print a textual representation of the memoized root mentions. REMOVE LATER? */
  private def showMemoization (rootChildren:Memoized) = {
    val sortedMentions = rootChildren.toSeq.sortBy(m => (m.sentence, m.start))
    sortedMentions.foreach { sm => mentionMgr.mentionToStrings(sm).foreach{println} }
  }


  /** Convert the entire output data structure to JSON and write it to the given file. */
  private def writeJsonToFile (fries:PropMap, outFile:File) = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
    Serialization.writePretty(fries, out)
    out.println()                           // add final newline which serialization omits
    out.flush()
    out.close()
  }
}


/** Enumeration of values that can appear in the EntityType field. */
object EntityType extends Enumeration {
  type EntityType = Value
  val Protein, Chemical, Gene = Value
}

// Binding
// Hydrolysis
// Phosphorylation
// Positive_regulation
// Transport
