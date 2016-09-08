package org.clulab.reach.serialization.json

import java.io.File

import org.clulab.serialization.json.DocOps
import org.clulab.serialization.json.JSONSerializer._
import org.clulab.odin
import org.clulab.odin._
import org.clulab.reach.grounding.{KBEntry, KBResolution}
import org.clulab.reach.mentions._
import org.clulab.struct.{DirectedGraph, Edge, Interval}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._


/** JSON serialization utilities */
object JSONSerializer {

  def jsonAST(corefmentions: Seq[CorefMention]): JValue = {
    val docsMap = corefmentions.map(m => m.document.equivalenceHash.toString -> m.document.jsonAST).toMap
    val mentionList = JArray(corefmentions.map(m => CorefMentionOps(m).jsonAST).toList)

    ("documents" -> docsMap) ~
    ("mentions" -> mentionList)
  }

  def jsonAST(f: File): JValue = parse(scala.io.Source.fromFile(f).getLines.mkString)

  /** Produce a sequence of biomentions from json */
  def toCorefMentions(json: JValue): Seq[CorefMention] = {

    require(json \ "documents" != JNothing, "\"documents\" key missing from json")
    require(json \ "mentions" != JNothing, "\"mentions\" key missing from json")

    val djson = json \ "documents"
    val mmjson = (json \ "mentions").asInstanceOf[JArray]

    mmjson.arr.map(mjson => toCorefMention(mjson, djson)).map(_.toCorefMention)
  }
  /** Produce a sequence of mentions from a json file */
  def toCorefMentions(file: File): Seq[CorefMention] = toCorefMentions(jsonAST(file))

  /** Build mention from json of mention and corresponding json map of documents <br>
    * Since a single Document can be quite large and may be shared by multiple mentions,
    * only a reference to the document json is contained within each mention.
    * A map from doc reference to document json is used to avoid redundancies and reduce file size during serialization.
    * */
  def toCorefMention(mjson: JValue, djson: JValue): CorefMention = {

    val tokInterval = Interval(
      (mjson \ "tokenInterval" \ "start").extract[Int],
      (mjson \ "tokenInterval" \ "end").extract[Int]
    )
    // elements shared by all Mention types
    val labels = (mjson \ "labels").extract[List[String]]
    val sentence = (mjson \ "sentence").extract[Int]
    val docHash = (mjson \ "document").extract[String]
    val document = toDocument(docHash, djson)
    val keep = (mjson \ "keep").extract[Boolean]
    val foundBy = (mjson \ "foundBy").extract[String]

    def mkArgumentsFromJsonAST(json: JValue): Map[String, Seq[Mention]] = try {
      val args = json.extract[Map[String, JArray]]
      val argPairs = for {
        (k: String, v: JValue) <- args
        mns: Seq[Mention] = v.arr.map(m => toCorefMention(m, djson))
      } yield (k, mns)
      argPairs
    } catch {
      case e: org.json4s.MappingException => Map.empty[String, Seq[Mention]]
    }

    /** Build mention paths from json */
    def toPaths(json: JValue, djson: JValue): Map[String, Map[Mention, odin.SynPath]] = {

      /** Create mention from args json for given id */
      def findMention(mentionID: String, json: JValue, djson: JValue): Option[Mention] = {
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
              case j :: _ => Some(toMention(j, djson))
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
            mOp = findMention(mentionID, json, djson)
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

    // build CorefMention
    // NOTE: while it would be cleaner to create a Mention and THEN add the needed bio and coref attributes,
    // it would not be easy to transform the arguments & trigger post-hoc using the json...
    val m = mjson \ "type" match {
      case JString(CorefEventMention.string) =>
        new CorefEventMention(
          labels,
          // trigger must be (Bio)TextBoundMention
          toCorefMention(mjson \ "trigger", djson).asInstanceOf[CorefTextBoundMention],
          mkArgumentsFromJsonAST(mjson \ "arguments"),
          paths = toPaths(mjson, djson),
          sentence,
          document,
          keep,
          foundBy,
          isDirect = (mjson \ "isDirect").extract[Boolean]
        )

      case JString(CorefRelationMention.string) =>
        new CorefRelationMention(
          labels,
          mkArgumentsFromJsonAST(mjson \ "arguments"),
          paths = toPaths(mjson, djson),
          sentence,
          document,
          keep,
          foundBy
        )

      case JString(CorefTextBoundMention.string) =>
        new CorefTextBoundMention(
          labels,
          tokInterval,
          sentence,
          document,
          keep,
          foundBy
        )

      case other => throw new Exception(s"unrecognized mention type '${other.toString}'")
    }


    m.antecedents = toAntecedents(mjson, djson)
    m.sieves = (mjson \ "sieves").extract[Set[String]]

    // attach display label
    m.displayLabel = (mjson \ "displayLabel").extract[String]

    // update grounding
    toKBResolution(mjson) match {
      case Some(kbr) => m.nominate(Some(Seq(kbr)))
      case None => ()
    }
    // update context
    toContext(mjson) match {
      case Some(context) => m.context = Some(context)
      case None => ()
    }
    // update mods
    m.modifications = toModifications(mjson, djson)
    m
  }

  def toAntecedents(mjson: JValue, djson: JValue): Set[Anaphoric] = mjson \ "antecedents" match {
    case JNothing => Set.empty[Anaphoric]
    case antecedents =>
      antecedents
        .asInstanceOf[JArray]
        .arr
        .map(mjson => toCorefMention(mjson, djson)).map(_.toCorefMention)
        .toSet
  }

  def toModifications(mjson: JValue, djson: JValue): Set[Modification] = mjson \ "modifications" match {
    case mods: JArray => mods.arr.map(json => toModification(json, djson)).toSet
    case _ => Set.empty[Modification]
  }

  def toModification(mjson: JValue, djson: JValue): Modification = mjson \ "type" match {
    case JString("PTM") =>
      PTM(
        label = (mjson \ "label").extract[String],
        evidence = getMention("evidence", mjson, djson),
        site = getMention("site", mjson, djson),
        negated = (mjson \ "negated").extract[Boolean]
      )
    case JString("EventSite") =>
      // site is required
      EventSite(site = getMention("site", mjson, djson).get)
    case JString("Mutant") =>
      // evidence is required
      Mutant(
        // evidence is required
        evidence = getMention("evidence", mjson, djson).get,
        foundBy = (mjson \ "foundBy").extract[String]
      )
    case JString("Negation") =>
      // evidence is required
      Negation(evidence = getMention("evidence", mjson, djson).get)
    case JString("Hypothesis") =>
      // evidence is required
      Hypothesis(evidence = getMention("evidence", mjson, djson).get)
    case other => throw new Exception(s"unrecognized modification type '${other.toString}'")
  }

  private def getMention(key: String, json: JValue, djson: JValue): Option[Mention] = json \ key match {
    case JNothing => None
    case evidence => Some(toCorefMention(json, djson))
  }

  def toKBResolution(json: JValue): Option[KBResolution] = json \ "grounding" match {
    case JNothing => None
    case grounding =>
      // build entry
      val entry = new KBEntry(
        text = (grounding \ "text").extract[String],
        key = (grounding \ "key").extract[String],
        namespace = (grounding \ "namespace").extract[String],
        id = (grounding \ "id").extract[String],
        species = (grounding \ "species").extract[String]
      )
      val kbr = new KBResolution(entry)
      Some(kbr)
  }

  def toContext(json: JValue): Option[Map[String, Seq[String]]] = json \ "context" match {
    case JNothing => None
    case context => Some(context.extract[Map[String, Seq[String]]])
  }
}