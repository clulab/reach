package org.clulab.reach.mentions.serialization.json

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin
import org.clulab.odin._
import org.clulab.odin.serialization.json.{JSONSerializer => OdinJSONSerializer}
import org.clulab.processors.Document
import org.clulab.reach.grounding.KBResolution
import org.clulab.reach.mentions.{Anaphoric, EventSite, Hypothesis, Modification, Mutant, Negation, PTM}
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioRelationMention, BioTextBoundMention}
import org.clulab.reach.mentions.{CHEMtrigger, DNtrigger, KDtrigger, KOtrigger, OEtrigger, UnassignedTrigger}
import org.clulab.reach.mentions.{CorefEventMention, CorefMention, CorefRelationMention, CorefTextBoundMention}
import org.clulab.reach.mentions.{MentionOps => OdinMentionOps}
import org.clulab.serialization.json.{DocOps, stringify}
import org.clulab.struct.{DirectedGraph, Edge, Interval}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.{prettyJson, renderJValue}

import java.io.File


/** JSON serialization utilities */
object JSONSerializer extends LazyLogging {
  implicit val formats = org.json4s.DefaultFormats

  def prettify(json: JValue): String = prettyJson(renderJValue(json))

  def json(jsonAST: JValue, pretty: Boolean = false): String = stringify(jsonAST, pretty)

  private def mentionsToDocsJMap(mentions: Seq[Mention]): Map[String, JValue] = {
    docsToDocsJMap(mentions.map(m => m.document))
  }

  /** Creates a Map of a Document.equivalenceHash (as String) -> Document.jsonAST <br>
    */
  def docsToDocsJMap(docs: Seq[Document]): Map[String, JValue] = {
    // create a set of Documents
    // in order to avoid calling jsonAST for duplicate docs
    docs.distinct.map(doc => doc.equivalenceHash.toString -> doc.jsonAST).toMap
  }

  /** Creates a Map of a Document.equivalenceHash (as String) -> Document <br>
    * Used for deserialization of mention JSON
    */
  def docsToDocumentMap(docs: Seq[Document]): Map[String, Document] = OdinJSONSerializer.mkDocumentMap(docsToDocsJMap(docs))

  def jsonAST(mention: Mention): JValue = jsonAST(Seq(mention))

  def jsonAST(mentions: Seq[Mention]): JValue = {

    val mentionList: List[JValue] = mentions.zipWithIndex.map { case (mention, index) =>
      println(s"$index ${mention.text}")
      MentionOps(mention).jsonAST
    }.toList
    val docMap: Map[String, JValue] = mentionsToDocsJMap(mentions)
    ("documents" -> docMap) ~ ("mentions" -> mentionList)
  }

  def jsonAST(f: File): JValue = parse(scala.io.Source.fromFile(f).getLines.mkString)

  /** Produce a sequence of mentions from a json file */
  def toBioMentions(file: File): Seq[BioMention] = toBioMentions(jsonAST(file))

  /** Produce a Seq[CorefMention] from json */
  def toBioMentions(json: JValue): Seq[BioMention] = {

    require(json \ "documents" != JNothing, "\"documents\" key missing from json")
    require(json \ "mentions" != JNothing, "\"mentions\" key missing from json")

    // build the documents once
    val docMap = OdinJSONSerializer.mkDocumentMap((json \ "documents").asInstanceOf[JObject])
    val mmjson = (json \ "mentions").asInstanceOf[JArray]

    mmjson.arr.map(mjson => toBioMention(mjson, docMap))
  }

  /** Build mention from json of mention and corresponding json map of documents <br>
    * Since a single Document can be quite large and may be shared by multiple mentions,
    * only a reference to the document json is contained within each mention.
    * A map from doc reference to document json is used to avoid redundancies and reduce file size during serialization.
    * */
  def toBioMention(mjson: JValue, docMap: Map[String, Document]): BioMention = {
    val tokInterval = getTokenInterval(mjson)
    // elements shared by all Mention types
    val labels = getLabels(mjson)
    val sentence = getSentence(mjson)
    val docHash = getDocHash(mjson)
    val document = docMap(docHash)
    val keep = getKeep(mjson)
    val foundBy = getFoundBy(mjson)

    // build BioMention
    // NOTE: while it would be cleaner to create a Mention and THEN add the needed bio and coref attributes,
    // it would not be easy to transform the arguments & trigger post-hoc using the json...
    val m = mjson \ "type" match {
      case JString(BioEventMentionOps.string) =>
        new BioEventMention(
          labels,
          // trigger must be (Bio)TextBoundMention
          toBioMention(mjson \ "trigger", docMap).toBioMention.asInstanceOf[BioTextBoundMention],
          mkArgumentsFromJsonAST(mjson \ "arguments", docMap),
          paths = toPaths(mjson, docMap),
          sentence,
          document,
          keep,
          foundBy,
          isDirect = getIsDirect(mjson)
        )

      case JString(BioRelationMentionOps.string) =>
        new BioRelationMention(
          labels,
          mkArgumentsFromJsonAST(mjson \ "arguments", docMap),
          paths = toPaths(mjson, docMap),
          sentence,
          document,
          keep,
          foundBy
        )

      case JString(BioTextBoundMentionOps.string) =>
        new BioTextBoundMention(
          labels,
          tokInterval,
          sentence,
          document,
          keep,
          foundBy
        )

      // paths involve Mention (not CorefMention)
      case other => toMentionByType(mjson, docMap).get.toBioMention
    }

    // attach display label
    setDisplayLabel(m, mjson)

    // update grounding
    setGrounding(m, mjson)

    // update context
    setContext(m, mjson)

    // update mods
    m.modifications = toModifications(mjson, docMap)
    m
  }

  def toBioMention(mjson: JValue, doc: Document): BioMention = {
    val docsMap = docsToDocumentMap(Seq(doc))
    toBioMention(mjson, docsMap)
  }

  /** Produce a sequence of mentions from a json file */
  def toCorefMentions(file: File): Seq[CorefMention] = toCorefMentions(jsonAST(file))

  /** Produce a Seq[CorefMention] from json */
  def toCorefMentions(json: JValue): Seq[CorefMention] = {
    toCorefMentionsMap(json: JValue).values.toSeq
  }

  /** Produce a Map of id -> mentions from a json file */
  def toCorefMentionsMap(file: File): Map[String, CorefMention] = toCorefMentionsMap(jsonAST(file))
  
  /** Produce a Map of id -> mentions from json */
  def toCorefMentionsMap(json: JValue): Map[String, CorefMention] = {

    require(json \ "documents" != JNothing, "\"documents\" key missing from json")
    require(json \ "mentions" != JNothing, "\"mentions\" key missing from json")

    // build the documents once
    val docMap = OdinJSONSerializer.mkDocumentMap((json \ "documents").asInstanceOf[JObject])
    val mmjson = (json \ "mentions").asInstanceOf[JArray]

    mmjson.arr.map(mjson => toCorefMentionWithId(mjson, docMap)).toMap
  }

  /** Build mention from json of mention and corresponding json map of documents <br>
    * Since a single Document can be quite large and may be shared by multiple mentions,
    * only a reference to the document json is contained within each mention.
    * A map from doc reference to document json is used to avoid redundancies and reduce file size during serialization.
    * */
  def toCorefMention(mjson: JValue, docMap: Map[String, Document]): CorefMention = { 
    toCorefMentionWithId(mjson: JValue, docMap: Map[String, Document])._2
  }

  def toCorefMentionWithId(mjson: JValue, docMap: Map[String, Document]): (String, CorefMention) = {

    val tokInterval = getTokenInterval(mjson)
    // elements shared by all Mention types
    val labels = getLabels(mjson)
    val sentence = getSentence(mjson)
    val docHash = getDocHash(mjson)
    val document = docMap(docHash)
    val keep = getKeep(mjson)
    val foundBy = getFoundBy(mjson)

    // build CorefMention
    // NOTE: while it would be cleaner to create a Mention and THEN add the needed bio and coref attributes,
    // it would not be easy to transform the arguments & trigger post-hoc using the json...
    val mentionId: String = (mjson \ "id").extract[String]
    val m = mjson \ "type" match {
      case JString(CorefEventMentionOps.string) =>
        new CorefEventMention(
          labels,
          // trigger must be (Bio)TextBoundMention
          toCorefMention(mjson \ "trigger", docMap).toCorefMention.asInstanceOf[CorefTextBoundMention],
          mkArgumentsFromJsonAST(mjson \ "arguments", docMap),
          paths = toPaths(mjson, docMap),
          sentence,
          document,
          keep,
          foundBy,
          isDirect = getIsDirect(mjson)
        )

      case JString(CorefRelationMentionOps.string) =>
        new CorefRelationMention(
          labels,
          mkArgumentsFromJsonAST(mjson \ "arguments", docMap),
          paths = toPaths(mjson, docMap),
          sentence,
          document,
          keep,
          foundBy
        )

      case JString(CorefTextBoundMentionOps.string) =>
        new CorefTextBoundMention(
          labels,
          tokInterval,
          sentence,
          document,
          keep,
          foundBy
        )

      // paths involve Mention (not CorefMention)
      case other => toMentionByType(mjson, docMap).get.toCorefMention
    }

    m.antecedents = toAntecedents(mjson, docMap)
    m.sieves = (mjson \ "sieves").extract[Set[String]]

    // attach display label
    setDisplayLabel(m, mjson)

    // update grounding
    setGrounding(m, mjson)

    // update context
    setContext(m, mjson)

    // update mods
    m.modifications = toModifications(mjson, docMap)
    (mentionId, m)
  }

  def toCorefMention(mjson: JValue, doc: Document): CorefMention = {
    val docsMap = docsToDocumentMap(Seq(doc))
    toCorefMention(mjson, docsMap)
  }

  private def toAntecedents(mjson: JValue, docMap: Map[String, Document]): Set[Anaphoric] = mjson \ "antecedents" match {
    case JNothing => Set.empty[Anaphoric]
    case antecedents =>
      antecedents
        .asInstanceOf[JArray]
        .arr
        .map(mjson => toCorefMention(mjson, docMap)).map(_.toCorefMention)
        .toSet
  }

  private def toModifications(mjson: JValue, docMap: Map[String, Document]): Set[Modification] = mjson \ "modifications" match {
    case mods: JArray =>
      mods.arr.map { json => toModification(json, docMap) }.toSet
    case other => Set.empty[Modification]
  }

  private def toModification(mjson: JValue, docMap: Map[String, Document]): Modification = {

    def getMentionOpt(key: String): Option[Mention] = this.getMention(key, mjson, docMap)

    def getMention(key: String): Mention = getMentionOpt(key).get

    def getEvidenceMentionOpt(): Option[Mention] = getMentionOpt("evidence")

    def getEvidenceMention(): Mention = getEvidenceMentionOpt().get

    mjson \ "modification-type" match {
      case JString("PTM") =>
        PTM(
          label = (mjson \ "label").extract[String],
          evidence = getEvidenceMentionOpt(),
          site = getMentionOpt("site"),
          negated = (mjson \ "negated").extract[Boolean]
        )
      case JString("EventSite") =>
        // site is required
        EventSite(site = getMention("site"))
      case JString("Mutant") =>
        // evidence is required
        Mutant(
          // evidence is required
          evidence = getEvidenceMention(),
          foundBy = (mjson \ "foundBy").extract[String]
        )
      case JString("Negation") =>
        // evidence is required
        Negation(evidence = getEvidenceMention())
      case JString("Hypothesis") =>
        // evidence is required
        Hypothesis(evidence = getEvidenceMention())

      case JString("KDtrigger") =>
        KDtrigger(evidence = getEvidenceMention())
      case JString("KOtrigger") =>
        KOtrigger(evidence = getEvidenceMention())
      case JString("DNtrigger") =>
        DNtrigger(evidence = getEvidenceMention())
      case JString("OEtrigger") =>
        OEtrigger(evidence = getEvidenceMention())
      case JString("CHEMtrigger") =>
        CHEMtrigger(evidence = getEvidenceMention())
      case JString("UnassignedTrigger") =>
        UnassignedTrigger(evidence = getEvidenceMention())

      case other => throw new Exception(s"unrecognized modification type '${other.toString}'")
    }
  }

  /** Build mention paths from json */
  private def toPaths(json: JValue, docMap: Map[String, Document]): Map[String, Map[Mention, odin.SynPath]] = {

    /** Create mention from args json for given id */
    def findMention(mentionID: String, json: JValue, docMap: Map[String, Document]): Option[Mention] = {
      // inspect arguments for matching ID
      json \ "arguments" match {
        // if we don't have arguments, we can't produce a Mention
        case JNothing => None
        // Ahoy! There be args!
        case something =>
          // flatten the Seq[Mention.jsonAST] for each arg
          val argsjson: Iterable[JValue] = for {
            mnsjson: JArray <- something.extract[Map[String, JArray]].values
            mjson <- mnsjson.arr
            if (mjson \ "id").extract[String] == mentionID
          } yield mjson

          argsjson.toList match {
            case Nil => None
            case j :: _ => toMentionByType(j, docMap)
          }
      }
    }

    // build paths
    json \ "paths" match {
      case JNothing => Map.empty[String, Map[Mention, odin.SynPath]]
      case contents => for {
        (role, innermap) <- contents.extract[Map[String, Map[String, JValue]]]
      } yield {
        // make inner map (Map[Mention, odin.SynPath])
        val pathMap = for {
          (mentionID: String, pathJSON: JValue) <- innermap.toSeq
          mOp = findMention(mentionID, json, docMap)
          // were we able to recover a mention?
          if mOp.nonEmpty
          m = mOp.get
          edges: Seq[Edge[String]] = pathJSON.extract[Seq[Edge[String]]]
          synPath: odin.SynPath = DirectedGraph.edgesToTriples[String](edges)
        } yield m -> synPath
        // marry role with (arg -> path) info
        role -> pathMap.toMap
      }
    }
  }

  // attach display label
  private def setDisplayLabel(m: Mention, mjson: JValue): Unit = (m, mjson \ "displayLabel") match {
    // covers both coref and bio cases
    case (bm: BioMention, JString(dl)) => bm.displayLabel = dl
    // default to label (needed for things like trigger of a BioEventMention)
    case (bm: BioMention, _) => bm.displayLabel = bm.label
    case other => ()
  }

  private def toContext(json: JValue): Option[Map[String, Seq[String]]] = json \ "context" match {
    case JNothing => None
    case context => Some(context.extract[Map[String, Seq[String]]])
  }
  // update context
  private def setContext(m: Mention, mjson: JValue): Unit = (m, toContext(mjson)) match {
    case (cm: CorefMention, Some(context)) => cm.contextOpt = Some(context)
    case (bm: BioMention, Some(context)) => bm.contextOpt = Some(context)
    case _ => ()
  }

  private def toKBResolution(json: JValue): Option[KBResolution] = json \ "grounding" match {
    case JNothing => None
    case grounding =>
      // build KB resolution object
      val kbr = new KBResolution(
        text = (grounding \ "text").extract[String],
        namespace = (grounding \ "namespace").extract[String],
        id = (grounding \ "id").extract[String],
        species = (grounding \ "species").extract[String]
      )
      Some(kbr)
  }

  // attempt to nominate a grounding resolution
  private def setGrounding(m: Mention, mjson: JValue): Unit = toKBResolution(mjson) match {
    case Some(kbr) =>
      val resolution = Some(Seq(kbr))
      m match {
        case cm: CorefMention => cm.nominate(resolution)
        case bm: BioMention => bm.nominate(resolution)
        case other => ()
      }
    case None => ()
  }

  private def mkArgumentsFromJsonAST(json: JValue, docMap: Map[String, Document]): Map[String, Seq[Mention]] = try {
    val args = json.extract[Map[String, JArray]]
    val argPairs = for {
      (k: String, v: JValue) <- args
      mns: Seq[Mention] = v.arr.flatMap(m => toMentionByType(m, docMap))
    } yield (k, mns)
    argPairs
  } catch {
    case e: org.json4s.MappingException => Map.empty[String, Seq[Mention]]
  }

  def toMentionByType(mjson: JValue, docMap: Map[String, Document]): Option[Mention] = mjson \ "type" match {
    // CorefMentions
    case JString(CorefTextBoundMentionOps.string) => Some(toCorefMention(mjson, docMap))
    case JString(CorefEventMentionOps.string) => Some(toCorefMention(mjson, docMap))
    case JString(CorefRelationMentionOps.string) => Some(toCorefMention(mjson, docMap))
    // BioMentions
    case JString(BioTextBoundMentionOps.string) => Some(toBioMention(mjson, docMap))
    case JString(BioEventMentionOps.string) => Some(toBioMention(mjson, docMap))
    case JString(BioRelationMentionOps.string) => Some(toBioMention(mjson, docMap))
    // Mentions
    case JString(org.clulab.odin.serialization.json.TextBoundMentionOps.string) => Some(OdinJSONSerializer.toMention(mjson, docMap))
    case JString(org.clulab.odin.serialization.json.EventMentionOps.string) => Some(OdinJSONSerializer.toMention(mjson, docMap))
    case JString(org.clulab.odin.serialization.json.RelationMentionOps.string) => Some(OdinJSONSerializer.toMention(mjson, docMap))
    // failure
    case _ => None
  }

  private def getMention(key: String, json: JValue, docMap: Map[String, Document]): Option[Mention] = json \ key match {
    case JNothing => None
    case evidence => toMentionByType(evidence, docMap)
  }

  private def getTokenInterval(mjson: JValue): Interval = Interval(
    (mjson \ "tokenInterval" \ "start").extract[Int],
    (mjson \ "tokenInterval" \ "end").extract[Int]
  )
  private def getLabels(mjson: JValue): List[String] = (mjson \ "labels").extract[List[String]]
  private def getSentence(mjson: JValue): Int = (mjson \ "sentence").extract[Int]
  private def getDocHash(mjson: JValue): String = (mjson \ "document").extract[String]
  private def getKeep(mjson: JValue): Boolean = (mjson \ "keep").extract[Boolean]
  private def getFoundBy(mjson: JValue): String = (mjson \ "foundBy").extract[String]
  private def getIsDirect(mjson: JValue): Boolean = (mjson \ "isDirect").extract[Boolean]
}
