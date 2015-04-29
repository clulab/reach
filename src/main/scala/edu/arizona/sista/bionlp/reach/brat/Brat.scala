package edu.arizona.sista.bionlp.reach.brat

import java.io.{File, InputStream}
import scala.collection.mutable.HashMap
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.{Document, Sentence}
import edu.arizona.sista.odin._

object Brat {
  def readStandOff(input: String): Seq[Annotation] =
    input.lines.toSeq flatMap parseAnnotation

  def readStandOff(input: InputStream): Seq[Annotation] =
    io.Source.fromInputStream(input).getLines.toSeq flatMap parseAnnotation

  def readStandOff(input: File): Seq[Annotation] =
    io.Source.fromFile(input).getLines.toSeq flatMap parseAnnotation

  def parseAnnotation(line: String): Option[Annotation] = {
    val chunks = line.trim.split("\t")
    val elems = chunks(1).split(" ")

    def arguments(elems: Seq[String]): Map[String, Seq[String]] =
      elems map (_.split(":")) groupBy (_(0)) mapValues (_.map(_(1)))

    chunks.head match {
      // text bound annotation
      case id if id.startsWith("T") =>
        val Array(label, offsets) = chunks(1).split(" ", 2)
        val spans = offsets.split(";") map (_.split(" ").map(_.toInt)) map (t => Interval(t(0), t(1)))
        Some(TextBound(id, label, spans, chunks(2)))

      // relation
      case id if id.startsWith("R") =>
        val label = elems.head
        val args = arguments(elems.tail)
        Some(Relation(id, label, args))

      // event
      case id if id.startsWith("E") =>
        val Array(label, trigger) = elems.head.split(":")
        val args = arguments(elems.tail)
        Some(Event(id, label, trigger, args))

      // equivalence
      case id if id.startsWith("*") =>
        Some(Equivalence(id, elems.head, elems.tail))

      // attribute
      case id if id.startsWith("A") || id.startsWith("M") =>
        if (elems.size == 2) {
          Some(BinaryAttribute(id, elems(0), elems(1)))
        } else if (elems.size == 3) {
          Some(MultiValueAttribute(id, elems(0), elems(1), elems(2)))
        } else {
          sys.error("unrecognized attribute type")
        }

      case id if id.startsWith("N") =>
        val Array(resource, entry) = elems(2).split(":")
        Some(Normalization(id, elems(0), elems(1), resource, entry, chunks(2)))

      // ignore everything else
      case _ => None
    }
  }

  def alignLabels(document: Document, annotations: Seq[Annotation]): Seq[Seq[String]] = {
    val textBound = annotations flatMap (_ match {
      case annotation: TextBound => Some(annotation)
      case _ => None
    })
    document.sentences map (alignSentenceLabels(_, textBound))
  }

  def alignTokenLabel(sentence: Sentence, token: Interval, annotations: Seq[TextBound]): String = {
    var label = "O"
    for (a <- annotations; span <- a.spans) {
      if (token overlaps span) {
        if (token.start <= span.start) {
          label = s"B-${a.label}"
        } else {
          label = s"I-${a.label}"
        }
      }
    }
    label
  }

  def alignSentenceLabels(sentence: Sentence, annotations: Seq[TextBound]): Seq[String] = {
    sentence.startOffsets zip sentence.endOffsets map {
      case (start, end) => alignTokenLabel(sentence, Interval(start, end), annotations)
    }
  }

  def dumpStandoff(mentions: Seq[Mention], doc: Document): String =
    dumpStandoff(mentions, doc, Nil)


  def dumpStandoff(mentions: Seq[Mention], doc: Document, annotations: Seq[Annotation]): String = {

    val idTracker = IdTracker(annotations)

    val mentionRepresentations = mentions.flatMap(m => m match {
      case event:EventMention => Seq(dumpStandoff(event.trigger, doc, idTracker), dumpStandoff(event, doc, idTracker))
      case m => Seq(dumpStandoff(m, doc, idTracker))
    })
      .distinct // just to be safe...
      .groupBy(_.head.toString) // first character

    val ruleNames = mentions.distinct.map(m => displayRuleName(m, doc, idTracker))

    // returns the num. value of the entity name
    def getNum(text:String):Int = if (text.isEmpty) -1 else text.split("\\s+").head.tail.toInt

    // sort mention representations
    (mentionRepresentations.getOrElse("T", Seq.empty).sortBy(getNum) ++
     mentionRepresentations.getOrElse("R", Seq.empty).sortBy(getNum) ++
     mentionRepresentations.getOrElse("E", Seq.empty).sortBy(getNum) ++
     ruleNames)
       .mkString("\n")
  }

  def displayRuleName(m: Mention, doc: Document, tracker: IdTracker): String = {
    //example:
    //#10     FoundByRule E4       Rulename1
    s"${tracker.getUniqueId(m, doc)}\tFoundByRule ${getId(m, doc, tracker)}\t${m.foundBy}"
  }

  def getId(m: Mention, doc: Document, tracker: IdTracker): String = m match {
    case t: TextBoundMention => tracker.getId(t, doc)
    case e: EventMention => tracker.getId(e, doc)
    case r: RelationMention => tracker.getId(r, doc)
  }

  def dumpStandoff(mention: Mention, doc: Document, tracker: IdTracker): String = {
    val sentence = doc.sentences(mention.sentence)

    mention match {
      case m: TextBoundMention =>
        val offsets = s"${sentence.startOffsets(m.start)} ${sentence.endOffsets(m.end - 1)}"
        val str = sentence.words.slice(m.start, m.end).mkString(" ")
        s"${getId(m, doc, tracker)}\t${m.label} $offsets\t$str"

      case m: EventMention =>
        val trigger = getId(m.trigger, doc, tracker)
        val arguments = m.arguments.flatMap{ case (name, vals) => vals map (v => s"$name:${getId(v, doc, tracker)}") }.mkString(" ")
        s"${getId(m, doc, tracker)}\t${m.label}:$trigger $arguments"

      case m: RelationMention =>
        val arguments = m.arguments.flatMap{ case (name, vals) => vals map (v => s"$name:${getId(v, doc, tracker)}") }.mkString(" ")
        s"${getId(m, doc, tracker)}\t${m.label} $arguments"
    }
  }

  def syntaxStandoff(doc: Document): String = {
    val idTracker = IdTracker()
    val tags = (doc.sentences.zipWithIndex flatMap {
      case (s, i) => s.tags.get.zipWithIndex map {
        case (tag, j) => (i, j) -> new TextBoundMention(tag, Interval(j), i, doc, true, "syntax")
      }
    }).toMap

    val tbIds = tags map { case (k, v) => k -> idTracker.getId(v, doc)}

    var id = 0
    val rels = doc.sentences.zipWithIndex flatMap {
      case (s, i) =>
        val outgoing = s.dependencies.get.outgoingEdges
        0 until outgoing.size flatMap {
          j => outgoing(j) map {
            case (k, dep) =>
              id += 1
              s"R$id\t$dep governor:${tbIds((i, j))} dependent:${tbIds((i, k))}"
          }
        }
    }

    (tags.values.map(m => dumpStandoff(m, doc, idTracker)) ++ rels).mkString("\n")
  }
}


class IdTracker(val textBoundLUT: HashMap[String, Interval]) {
  val eventLUT = new HashMap[EventMention, String]
  val relationLUT = new HashMap[RelationMention, String]
  val uniqueLUT = new HashMap[Mention, String]

  def getId(mention: TextBoundMention, doc: Document): String = {
    val span = charInterval(mention, doc)
    val ids = textBoundLUT.keys filter (id => textBoundLUT(id) overlaps span)
    if (ids.size == 1) ids.head
    else if (ids.size > 1) ids.head // arbitrarily returning the first one
    else {
      val id = s"T${textBoundLUT.size + 1}"
      val kv = (id -> span)
      textBoundLUT += kv
      id
    }
  }

  def getId(mention: EventMention, doc: Document): String =
    eventLUT.getOrElseUpdate(mention, s"E${eventLUT.size + 1}")

  def getId(mention: RelationMention, doc: Document): String =
    relationLUT.getOrElseUpdate(mention, s"R${relationLUT.size + 1}")

  def getUniqueId(mention: Mention, doc: Document): String =
    uniqueLUT.getOrElseUpdate(mention, s"#${uniqueLUT.size + 1}")

  def charInterval(mention: Mention, document: Document): Interval =
    charInterval(mention, document.sentences(mention.sentence))

  def charInterval(mention: Mention, sentence: Sentence): Interval = {
    val charStart = sentence.startOffsets(mention.start)
    val charEnd = sentence.endOffsets(mention.end - 1)
    Interval(charStart, charEnd)
  }
}

object IdTracker {
  def apply(): IdTracker = apply(Nil)

  def apply(annotations: Seq[Annotation]): IdTracker = {
    val lut = annotations flatMap {
      _ match {
        case a: TextBound => Some(a.id -> a.totalSpan)
        case _ => None
      }
    }
    new IdTracker(HashMap(lut: _*))
  }
}
