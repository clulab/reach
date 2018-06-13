package org.clulab.reach

import java.io.File
import scala.util.control.NonFatal
import scala.collection.mutable.StringBuilder
import org.clulab.odin._
import org.clulab.processors.{ Document, Sentence }
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.mentions._
import org.clulab.reach.grounding._
import org.clulab.reach.darpa.{ DarpaActions, MentionFilter, NegationHandler }
import ai.lum.common.FileUtils._
import RuleReader.readResource

object BioNlp2013 {

  def main(args: Array[String]): Unit = {

    // initializing all our stuff
    val bionlpSystem = new BioNlp2013System

    val dataDir = new File("/home/marco/data/reach/BioNLP-ST-2013_GE_devel_data_rev3")
    val outDir = new File("/home/marco/data/reach/output")

    for (txtFile <- dataDir.listFilesByWildcard("*.txt")) {
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

// a brat textbound mention
case class BratTBM(id: String, label: String, start: Int, end: Int, text: String)

class BioNlp2013System {

  // initialize processor
  val processor = new BioNLPProcessor(withRuleNER = false)

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
    // handle quotes
    case "''"    => List("\"", "''")
    case "``"    => List("\"", "``")
    case w       => List(w)
  }

  def adjustOffsets(doc: Document, text: String) = {
    var from = 0
    for {
      (s, i) <- doc.sentences.zipWithIndex
      (w, j) <- s.words.zipWithIndex
    } {
      // try all candidates
      val candidates = tokenCandidates(w).flatMap { c =>
        val idx = text.indexOf(c, from)
        // don't consider candidate if index is -1
        if (idx == -1) None else Some((c, idx))
      }
      if (candidates.isEmpty) {
        println("ERROR")
        println(text.slice(s.startOffsets.head, s.endOffsets.last))
        println(s.words.mkString(" "))
        println(w)
      }
      // get the closest one
      val (tok, start) = if (candidates.isEmpty) (w, -1) else candidates.minBy(_._2)
      // compute end offset
      val end = start + tok.size
      // overwrite offsets
      doc.sentences(i).startOffsets(j) = start
      doc.sentences(i).endOffsets(j) = end
      // new beginning
      from = end
    }
  }

  def mkTags(sent: Sentence, tbms: Vector[BratTBM]): Option[Array[String]] = tbms match {
    case Vector() => Some(Array.fill(sent.words.size)("O"))
    case tbms =>
      val tags = Array.fill(sent.words.size)("O")
      // only consider mentions in the current sentence
      for (m <- tbms if m.start >= sent.startOffsets.head && m.end <= sent.endOffsets.last) {
        var first = true
        for (i <- tags.indices) {
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

  def dumpA2Annotations(mentions: Seq[BioMention], entities: Seq[BratTBM]): String = {
    // start standoff
    val standoff = new StringBuilder
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
    // retrieve all event mentions
    val ems = mentions.flatMap {
      case m: BioEventMention => Some(m)
      case _ => None
    }
    // only report simple events
    for (em <- ems) em match {
      case se if se matches "SimpleEvent" =>
        currTBMID += 1
        currEMID += 1
        val lbl = se.label.replaceFirst("Auto", "").capitalize
        standoff ++= s"T${currTBMID}\t$lbl ${se.trigger.startOffset} ${se.trigger.endOffset}\t${se.trigger.text}\n"
        standoff ++= s"E${currEMID}\t$lbl:T${currTBMID} "
        for {
          (name, args) <- se.arguments
          arg <- args
        } {
          arg match {
            case m: BioTextBoundMention if tbmToId contains m =>
              val id = tbmToId(m)
              standoff ++= s"${name.capitalize}:${id} "
            case _ => ()
          }
        }
        standoff ++= "\n"

      case ce if ce matches "Regulation" =>
        currTBMID += 1
        currEMID += 1
        val promoted = ce.arguments("controlled").head.asInstanceOf[BioEventMention]
        val promotedLabel = promoted.label.replaceFirst("Auto", "").capitalize
        standoff ++= s"T${currTBMID}\t$promotedLabel ${promoted.trigger.startOffset} ${promoted.trigger.endOffset}\t${promoted.trigger.text}\n"
        standoff ++= s"E${currEMID}\t$promotedLabel:T${currTBMID} "
        for {
          (name, args) <- ce.arguments
          arg <- args
        } {
          arg match {
            case tbm: BioTextBoundMention if tbmToId contains tbm =>
              val id = tbmToId(tbm)
              standoff ++= s"Cause:$id "
            case evm: BioEventMention =>
              val subArgs = evm.arguments
                .map{ case (n, a) =>
                  n -> a.filter(_.isInstanceOf[BioTextBoundMention])
                }
                .filter{ case (n, a) => a.nonEmpty }
              for {
                (subName, subArgs) <- subArgs
                subArg <- subArgs
              } {
                subArg match {
                  case tbm: BioTextBoundMention if tbmToId contains tbm =>
                    val id = tbmToId(tbm)
                    standoff ++= s"${subName.capitalize}:$id "
                  case _ => ()
                }
              }
            case _ => ()
          }
        }
        standoff ++= "\n"

      case em => ()
    }
    standoff.mkString
  }

}
