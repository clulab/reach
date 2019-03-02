package org.clulab.reach

import java.io.File

import scala.util.control.NonFatal
import scala.collection.mutable.StringBuilder
import org.clulab.odin._
import org.clulab.processors.{Document, Sentence}
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.mentions._
import ai.lum.common.FileUtils._
import ai.lum.common.ConfigUtils._

import com.typesafe.config.ConfigFactory

import scala.collection.mutable

object BioNlp2013 {

  def main(args: Array[String]): Unit = {

    val config = ConfigFactory.load()
    // initializing all our stuff
    val bionlpSystem = new BioNlp2013System
    
    val conf    = ConfigFactory.load()
    val dataDir = config[File]("bionlpEvaluation.data")
    val outDir  = config[File]("bionlpEvaluation.output")

    for (txtFile <- dataDir.listFilesByWildcard("*.txt").toSeq.par) {
      try {
        println(txtFile)
        val a1 = bionlpSystem.readCorrespondingA1(txtFile)
        val doc = bionlpSystem.readBioNlpAnnotations(txtFile, a1)
        val results = bionlpSystem.extractFrom(doc)
        val a2 = bionlpSystem.dumpA2Annotations(results, a1)
        // dump standoff
        val basename = txtFile.getBaseName()
        val a2File = new File(outDir, basename + ".a2")
        a2File.writeString(a2)
      } catch {
        case NonFatal(e) => ()
      }
    }

  }

}

// represents a trigger as a span of text.  Used to keep track of seen triggers.
case class TextualSpan(docId: Option[String], sentIdx: Int, start: Int, end: Int) extends Serializable

// a brat textbound mention
case class BratTBM(id: String, label: String, start: Int, end: Int, text: String) extends Serializable {
  val standoff: String = s"$id\t$label $start $end\t$text\n"
}

class BioNlp2013System {

  // These are the only SimpleEvents allowed in the task
  val targetLabels = Set(
    "Binding",
    "AutoPhosphorylation",
    "Phosphorylation",
    "Ubiquitination",
    "Acetylation",
    "Deacetylation",
    //"Translocation",
    "Transcription",
    "Localization",
    "Gene_expression"
  )

  // Change labels for print-out to work with BioNLP labels
  def labelTranslator(in: String): String = in match {
    case "Translocation" => "Localization"
    case "AutoPhosphorylation" => "Phosphorylation"
    case other => other
  }

  // initialize processor
  val processor = new BioNLPProcessor(withRuleNER = false, maxSentenceLength = 160)

  /** Avoid using the NER */
  def annotate(text: String): Document = {
    val doc = processor.mkDocument(text, keepText = true)
    processor.tagPartsOfSpeech(doc)
    processor.lemmatize(doc)
    //processor.recognizeNamedEntities(doc)
    processor.parse(doc)
    //processor.chunking(doc)
    //processor.resolveCoreference(doc)
    //processor.discourse(doc)
    doc.clear()
    doc
  }

  annotate("something")

  // initialize reach
  val reachSystem = new ReachSystem

  def extractFrom(doc: Document): Seq[BioMention] = {
    reachSystem.extractFrom(doc)
  }

  def mkDoc(text: String, docId: String): Document = {
    val doc = annotate(text)
    adjustOffsets(doc, text)
    doc.id = Some(docId)
    doc
  }

  // returns annotated document corresponding to bionlp2013 file
  // with entities provided in the corpus
  def readBioNlpAnnotations(txtFile: File, tbms: Vector[BratTBM]): Document = {
    val docId = txtFile.getBaseName() // the docId is the filename minus path and extension
    val text = txtFile.readString()
    val doc = mkDoc(text, docId)
    replaceNer(doc, tbms)
    // FIXME the following prints are for debugging only and should be removed
    // tbms.foreach(println)
    // for (s <- doc.sentences) {
    //   for (i <- s.words.indices) {
    //     print(s"(${s.words(i)}, ${s.entities.get(i)}, ${s.startOffsets(i)}, ${s.endOffsets(i)})")
    //   }
    //   println()
    // }
    // println("---")
    // val a2File = new File("""\.txt$""".r.replaceAllIn(txtFile.getCanonicalName(), ".a2"))
    doc
  }

  def readCorrespondingA1(txtFile: File): Vector[BratTBM] = {
    val a1File = new File("""\.txt$""".r.replaceAllIn(txtFile.getCanonicalPath(), ".a1"))
    readA1Annotations(a1File)
  }

  def readA1Annotations(a1File: File): Vector[BratTBM] = {
    val pattern = """(T\d+)\t(\w+) (\d+) (\d+)\t(.+)""".r
    val text = a1File.readString()
    pattern
      .findAllMatchIn(text)
      .map(m => BratTBM(m.group(1), m.group(2), m.group(3).toInt, m.group(4).toInt, m.group(5)))
      .toVector
  }

  // replaces ner in-place
  def replaceNer(doc: Document, tbms: Vector[BratTBM]): Unit = {
    for (sent <- doc.sentences) {
      sent.entities = mkTags(sent, tbms)
    }
  }

  /** Takes a token produced by processors and retrieves a list of reverse mappings
    * corresponding to known character replacements applied during tokenization. <br>
    * Returns a List ordered by of matching precedence.
    **/
  def tokenCandidates(term: String): List[String] = term match {
    // parentheses and brackets
    case "-LRB-" => List("(", "-LRB-")
    case "-LSB-" => List("[", "-LSB-")
    case "-LCB-" => List("{", "-LCB-")

    case "-RRB-" => List(")", "-RRB-")
    case "-RSB-" => List("]", "-RSB-")
    case "-RCB-" => List("}", "-RCB-")

    // slashes
    case "and"   => List("-", "/", "and", ",")

    // quotes
    case "''"    => List("\"", "''")
    case "``"    => List("\"", "``")

    // hyphens in complexes
    case ","     => List("-", ",")

    // periods after an abbreviation
    case "."     => List("", ".")

    case w       => List(w)
  }

  /**
    * Change Document word offsets to align with pre-tokenized offsets so that we can identify the right NER entities
    */
  def adjustOffsets(doc: Document, text: String) = {
    // where to start each word search from
    var from = 0
    for {
      (s, i) <- doc.sentences.zipWithIndex
      (w, j) <- s.words.zipWithIndex
    } {
      // try all candidates
      val candidates = tokenCandidates(w).flatMap { c =>
        // find the index of this word in the pre-tokenized text
        val idx = text.indexOf(c, from)
        // don't consider candidate if index is -1
        if (idx == -1) None else Some((c, idx))
      }
      // the alignment didn't work because the word wasn't found in the original text
      if (candidates.isEmpty) {
        println("ERROR")
        println(text.slice(s.startOffsets.head, s.endOffsets.last))
        println(s.words.mkString(" "))
        println(w)
      }
      // get the closest one
      val (tok, start) = if (candidates.isEmpty) (w, -1) else candidates.minBy(_._2)
      // compute end offset
      val end = start + tok.length
      // overwrite offsets
      doc.sentences(i).startOffsets(j) = start
      doc.sentences(i).endOffsets(j) = end
      // new beginning
      from = end
    }
  }

  /**
    * From entities (text-bound mentions) in the a1 file, populate entities in the sentence to base TextBoundMentions on
    */
  def mkTags(sent: Sentence, tbms: Vector[BratTBM]): Option[Array[String]] = tbms match {
    // no TBMS => no entities
    case Vector() => Some(Array.fill(sent.words.length)("O"))
    case nonemptyTbms =>
      val tags = Array.fill(sent.words.length)("O")
      // only consider mentions in the current sentence
      for (m <- nonemptyTbms if m.start >= sent.startOffsets.head && m.end <= sent.endOffsets.last) {
        var first = true
        for (i <- tags.indices) {
          // a1 TBM must be within the bounds of this token in the Document
          if (m.start >= sent.startOffsets(i) && m.end <= sent.endOffsets(i)) {
            val tag = if (first) {
              first = false
              // s"B-${m.label}"
              "B-Gene_or_gene_product"
            } else {
              // s"I-${m.label}"
              "I-Gene_or_gene_product"
            }
            tags(i) = tag
          }
        }
      }
      Some(tags)
  }

  /**
    * Translate Reach event mentions and new entity mentions (triggers and sites) into BRAT output for normalization
    * and evaluation
    */
  def dumpA2Annotations(mentions: Seq[BioMention], entities: Seq[BratTBM]): String = {

    // keep track of triggers dumped to standoff in order to reuse ID
    var triggerMap: Map[TextualSpan, BratTBM] = Map.empty[TextualSpan, BratTBM]

    // start standoff
    val entityStandoff = new StringBuilder
    val eventStandoff = new StringBuilder
    var currTBMID = entities.size
    var currEMID = 0
    // retrieve all textbound mentions
    val tbms = mentions.flatMap {
      case m: BioTextBoundMention => Some(m)
      case _ => None
    }
    // map textbound mentions to brat id
    val tbmToId = tbms.flatMap { m =>
      val start = m.startOffset
      val end = m.endOffset
      entities.find(e => e.start <= start && e.end >= end).map(m -> _.id)
    }.toMap

    // simple events that are the controlled of a BioRelationMention regulation
    val regThemes = mentions
      .filter(_.isInstanceOf[BioRelationMention])
      .flatMap(m => m.arguments.getOrElse("controlled", Nil))

    // keep track of SimpleEvent IDs for regulations later
    val semToId = mutable.Map[Mention, String]()

    // look up TBMs with PTM modifications -- these are events in BioNLP terms
    val modifieds = mentions
      .filter(_.isInstanceOf[BioTextBoundMention])
      .filter(m => tbmToId.contains(m.asInstanceOf[BioTextBoundMention]))
      .filter(_.modifications.exists(m => m.isInstanceOf[PTM] && targetLabels.contains(m.label)))
    for {
      modified <- modifieds
      mod <- modified.modifications
      // ensure it's a PTM we care about
      if mod.isInstanceOf[PTM] && targetLabels.contains(mod.label) && mod.asInstanceOf[PTM].evidence.nonEmpty
    } {
      val ptmMod = mod.asInstanceOf[PTM]

      // make a trigger from the modification's evidence
      currTBMID += 1
      currEMID += 1
      semToId(modified) = s"E$currEMID"
      val lbl = ptmMod.label
      // If the same trigger is used for multiple events (ex. event split because of a coordination),
      // we need to reuse its ID.
      //
      // represent the trigger
      val trigger = TextualSpan(
        modified.document.id,
        modified.sentence,
        ptmMod.evidence.get.startOffset,
        ptmMod.evidence.get.endOffset
      )
      val repr: BratTBM = if (! triggerMap.contains(trigger)) {
        val triggerTBM = BratTBM(
          id = s"T$currTBMID",
          label = lbl,
          start = ptmMod.evidence.get.startOffset,
          end = ptmMod.evidence.get.endOffset,
          text = ptmMod.evidence.get.text
        )
        // update map
        triggerMap = triggerMap + (trigger -> triggerTBM)

        // append new trigger standoff
        entityStandoff ++= triggerTBM.standoff

        triggerTBM
      } else {
        triggerMap(trigger)
      }
      eventStandoff ++= s"E$currEMID\t$lbl:${repr.id} "

      // the theme is just the original TBM (that has the modification) which already exists in the tbmToId
      val id = tbmToId(modified.asInstanceOf[BioTextBoundMention])
      eventStandoff ++= s"Theme:$id "

//      // if there's a site, print it as well
//      if (ptmMod.site.nonEmpty) {
//        val site = ptmMod.site.get
//        val ts = TextualSpan(
//          docId = site.document.id,
//          sentIdx = site.sentence,
//          start = site.startOffset,
//          end = site.endOffset
//        )
//        // check if Site in triggerMap
//        val knownSite = if (! triggerMap.contains(ts)) {
//          currTBMID += 1
//          val bratTBM = BratTBM(
//            id = s"T$currTBMID",
//            label = "Entity",
//            start = site.startOffset,
//            end = site.endOffset,
//            text = site.text
//          )
//          // update triggerMap
//          triggerMap = triggerMap + (ts -> bratTBM)
//          entityStandoff ++= bratTBM.standoff
//          bratTBM
//        } else {
//          triggerMap(ts)
//        }
//        // write site standoff
//        eventStandoff ++= s"Site:${knownSite.id} "
//      }

      eventStandoff ++= "\n"
    }

    /**
      * This SimpleEvent has at least one theme that was found in the a1-derived TBM list and isn't generic
      */
    def hasValidArgs(mention: BioMention): Boolean = {
      mention.arguments.contains("theme") &&
        mention.arguments("theme")
          .filter(a => tbmToId.contains(a.asInstanceOf[BioTextBoundMention]) &&
            !a.toCorefMention.isGeneric)
          .nonEmpty
    }

    /**
      * If this is a binding, it's got to have at least two valid themes
      */
    def validBinding(mention: BioMention): Boolean = mention match {
      case binding if mention matches "Binding" =>
        binding.arguments("theme")
          .filter(a => tbmToId.contains(a.asInstanceOf[BioTextBoundMention]) &&
            !a.toCorefMention.isGeneric)
          .size > 1
      case other => true
    }

    // report simple events that aren't the controlled of a BioRelationMention regulation
    // filters ensure that theme exists in .a1 file and that the SimpleEvent at least has a theme
    val sems = mentions.filter(_ matches "SimpleEvent")
      .filter(m => targetLabels contains m.label) // ignore irrelevant events
      .filterNot(regThemes.contains) // handle simple-event-with-cause separately
      .filter(hasValidArgs)
      .filter(validBinding)
      .map(_.asInstanceOf[BioEventMention])
    for (se <- sems) {
      currTBMID += 1
      currEMID += 1
      semToId(se) = s"E$currEMID"
      val lbl = se.label
      // If the same trigger is used for multiple events (ex. event split because of a coordination),
      // we need to reuse its ID.
      //
      // represent the trigger
      val trigger = TextualSpan(se.document.id, se.sentence, se.trigger.startOffset, se.trigger.endOffset)
      val repr: BratTBM = if (! triggerMap.contains(trigger)) {
        val triggerTBM = BratTBM(
          id = s"T$currTBMID",
          label = lbl,
          start = se.trigger.startOffset,
          end = se.trigger.endOffset,
          text = se.trigger.text
        )
        // update map
        triggerMap = triggerMap + (trigger -> triggerTBM)

        // append new trigger standoff
        entityStandoff ++= triggerTBM.standoff

        triggerTBM
      } else {
        triggerMap(trigger)
      }
      eventStandoff ++= s"E$currEMID\t$lbl:${repr.id} "

      var themeSeen = false
      for {
        (name, args) <- se.arguments
        arg <- args
      } {
        arg match {
          case m: BioTextBoundMention if tbmToId contains m =>
            val id = tbmToId(m)
            val label = if (name == "theme" && themeSeen) "Theme2" else name.capitalize
            eventStandoff ++= s"$label:$id "
            if (name == "theme") themeSeen = true
//          case site: BioTextBoundMention if name == "site" =>
//            val ts = TextualSpan(
//              docId = site.document.id,
//              sentIdx = site.sentence,
//              start = site.startOffset,
//              end = site.endOffset
//            )
//            // check if Site in triggerMap
//            val knownSite = if (! triggerMap.contains(ts)) {
//              currTBMID += 1
//              val bratTBM = BratTBM(
//                id = s"T$currTBMID",
//                label = "Entity",
//                start = site.startOffset,
//                end = site.endOffset,
//                text = site.text
//              )
//              // update triggerMap
//              triggerMap = triggerMap + (ts -> bratTBM)
//              entityStandoff ++= bratTBM.standoff
//              bratTBM
//            } else {
//              triggerMap(ts)
//            }
//            // write site standoff
//            eventStandoff ++= s"Site:${knownSite.id} "
          case _ => ()
        }
      }
      eventStandoff ++= "\n"
    }

    /** Check whether or not a mention's args are known or TextBound */
    def seenIt(mns: Seq[Mention]): Boolean = mns.forall{
      case btm: TextBoundMention =>
        tbmToId.contains(btm.asInstanceOf[BioTextBoundMention])
      case other =>
        semToId.contains(other)
    }

    // report regulation events
    val rems = mentions
      .filter(_ matches "Regulation")
      .filter(m => m.arguments.contains("controlled") && m.arguments.contains("controller"))
      .filter{ reg =>
        // we check the controlled simple event for themes that won't print
        val regulated = reg.arguments("controlled")
        val regulator = reg.arguments("controller")
        // ensure that at least one theme is not generic and exists in the map
        val regulatedOkay = regulated.exists(r => targetLabels.contains(r.label) &&
          hasValidArgs(r.asInstanceOf[BioMention]) &&
          validBinding(r.asInstanceOf[BioMention]))
        val regulatorOkay = seenIt(regulator)
        regulatedOkay && regulatorOkay
      }

    for (ce <- rems) ce match {
      // e.g. A phosphorylates B
      case ce: BioRelationMention =>
        val promoteds = ce.arguments("controlled")
          .filter(m => hasValidArgs(m.asInstanceOf[BioMention]))
          .filter(m => validBinding(m.asInstanceOf[BioMention]))
        for (p <- promoteds) {
          currEMID += 1
          val promoted = p.asInstanceOf[BioEventMention]
          val promotedLabel = labelTranslator(promoted.label)
          // If the same trigger is used for multiple events (ex. event split because of a coordination),
          // we need to reuse its ID.
          //
          // represent the trigger
          val trigger = TextualSpan(promoted.document.id, promoted.sentence, promoted.startOffset, promoted.trigger.endOffset)
          val repr: BratTBM = if (!triggerMap.contains(trigger)) {
            currTBMID += 1
            val triggerTBM = BratTBM(
              id = s"T$currTBMID",
              label = promotedLabel,
              start = promoted.trigger.startOffset,
              end = promoted.trigger.endOffset,
              text = promoted.trigger.text
            )
            // update map
            triggerMap = triggerMap + (trigger -> triggerTBM)

            // append new trigger standoff
            entityStandoff ++= triggerTBM.standoff

            triggerTBM
          } else {
            triggerMap(trigger)
          }
          eventStandoff ++= s"E$currEMID\t$promotedLabel:${repr.id} "

          for {
            (name, args) <- ce.arguments
            arg <- args
          } {
            arg match {
              case tbm: BioTextBoundMention if tbmToId.contains(tbm) && !promoted.label.startsWith("Auto") =>
                val id = tbmToId(tbm)
                eventStandoff ++= s"Cause:$id "
              case evm: BioEventMention =>
                val subArgs = evm.arguments
                  .map { case (n, a) =>
                    n -> a.filter(_.isInstanceOf[BioTextBoundMention])
                  }
                  .filter { case (n, a) => a.nonEmpty }
                for {
                  (subName, subArgs) <- subArgs
                  subArg <- subArgs
                } {
                  subArg match {
                    case tbm: BioTextBoundMention if tbmToId contains tbm =>
                      val id = tbmToId(tbm)
                      eventStandoff ++= s"${subName.capitalize}:$id "
//                    case site: BioTextBoundMention if subName == "site" =>
//                      val ts = TextualSpan(
//                        docId = site.document.id,
//                        sentIdx = site.sentence,
//                        start = site.startOffset,
//                        end = site.endOffset
//                      )
//                      // check if Site in triggerMap
//                      val knownSite = if (! triggerMap.contains(ts)) {
//                        currTBMID += 1
//                        val bratTBM = BratTBM(
//                          id = s"T$currTBMID",
//                          label = "Entity",
//                          start = site.startOffset,
//                          end = site.endOffset,
//                          text = site.text
//                        )
//                        // update triggerMap
//                        triggerMap = triggerMap + (ts -> bratTBM)
//                        entityStandoff ++= bratTBM.standoff
//                        bratTBM
//                      } else {
//                        triggerMap(ts)
//                      }
//                      // write site standoff
//                      eventStandoff ++= s"Site:${knownSite.id} "
                    case _ => ()
                  }
                }

              case _ => ()
            }
          }
          eventStandoff ++= "\n"
        }

      // e.g. A upregulates the phosphorylation of B by C
      case ce: BioEventMention =>
        currEMID += 1

        // If the same trigger is used for multiple events (ex. event split because of a coordination),
        // we need to reuse its ID.
        //
        // represent the trigger
        val trigger = TextualSpan(ce.document.id, ce.sentence, ce.startOffset, ce.trigger.endOffset)
        val repr: BratTBM = if (! triggerMap.contains(trigger)) {
          currTBMID += 1
          val triggerTBM = BratTBM(
            id = s"T$currTBMID",
            label = ce.label,
            start = ce.trigger.startOffset,
            end = ce.trigger.endOffset,
            text = ce.trigger.text
          )
          // update map
          triggerMap = triggerMap + (trigger -> triggerTBM)

          // append new trigger standoff
          entityStandoff ++= triggerTBM.standoff

          triggerTBM
        } else {
          triggerMap(trigger)
        }
        eventStandoff ++= s"E$currEMID\t${ce.label}:${repr.id} "

        for {
          (name, args) <- ce.arguments
          arg <- args
          if !(name == "controller" && ce.label == "Transcription")
        } {
          val label = name match {
            case "controlled" => "Theme"
            case "controller" => "Cause"
            case _ =>
              println(s"$name is not an expected regulation label")
              ""
          }
          arg match {
            case m: BioTextBoundMention if tbmToId contains m =>
              val id = tbmToId(m)
              eventStandoff ++= s"$label:$id "
            case em if (em matches "Event") && (semToId contains em) =>
              val id = semToId(em)
              eventStandoff ++= s"$label:$id "
//            case site: BioTextBoundMention if name == "site" =>
//              val ts = TextualSpan(
//                docId = site.document.id,
//                sentIdx = site.sentence,
//                start = site.startOffset,
//                end = site.endOffset
//              )
//              // check if Site in triggerMap
//              val knownSite = if (! triggerMap.contains(ts)) {
//                currTBMID += 1
//                val bratTBM = BratTBM(
//                  id = s"T$currTBMID",
//                  label = "Entity",
//                  start = site.startOffset,
//                  end = site.endOffset,
//                  text = site.text
//                )
//                // update triggerMap
//                triggerMap = triggerMap + (ts -> bratTBM)
//                entityStandoff ++= bratTBM.standoff
//                bratTBM
//              } else {
//                triggerMap(ts)
//              }
//              // write site standoff
//              eventStandoff ++= s"Site:${knownSite.id} "

            case _ => ()
          }
        }
        eventStandoff ++= "\n"
    }

    entityStandoff.mkString + eventStandoff.mkString
  }

}
