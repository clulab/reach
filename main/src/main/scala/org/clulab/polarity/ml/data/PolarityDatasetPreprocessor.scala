package org.clulab.polarity.ml.data

import java.io.PrintWriter
import java.util.{Calendar, Date}
import com.typesafe.scalalogging.LazyLogging
import org.clulab.polarity.{NegativePolarity, Polarity, PositivePolarity}
import org.clulab.reach.{PaperReader, ReachSystem}
import org.clulab.reach.mentions.{BioEventMention, BioMention, CorefEventMention}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}
import org.clulab.reach.mentions.serialization.json.{JSONSerializer, MentionsOps}
import org.clulab.reach.mentions.{MentionOps => ImplicitMentionOps}
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

/**
  * Takes as input a TSV file with a polarity distant supervision dataset and digests it into REACH mentions
  */
object PolarityDatasetPreprocessor extends App with LazyLogging{

  val extractorEngine = PaperReader.reachSystem

  /** Processes a TSV file */
  def digestTsv(path:String, opposingStatements:Boolean = true):Seq[(BioEventMention, Polarity)] = {

    logger.info(s"Processing $path.")

    // Processes a line of the TSV file
    def processLine(l:String):Try[(BioEventMention, Polarity)] = Try{
      logger.info(s"Annotating: $l")
      // Tokenize the line
      val tokens = l.trim.split("\t")

      // Extract the arguments
      val sub = tokens(1).toLowerCase.replace("-", " ")
      val obj = tokens(2).toLowerCase.replace("-", " ")

      // Compute "silver" polarity
      val statement = tokens(0)
      val polarity =
        if(statement startsWith "Activation")
          if(opposingStatements)
            NegativePolarity
          else
            PositivePolarity
        else
        if(opposingStatements)
          PositivePolarity
        else
          NegativePolarity

      // Annotate the sentence
      val sentence = tokens.last
      val extractions = extractorEngine.extractFrom(sentence, "", "", None)

      def matchesCriteria(m:BioMention) = m match {
        case evt:CorefEventMention if evt matches "ComplexEvent" =>

          val theme = evt.controlledArgs() match {
            case Some(args) =>
              args.map(_.text).mkString(" ").toLowerCase
            case None => ""
          }

          val cause = evt.controllerArgs() match {
            case Some(args) =>
              args.map(_.text).mkString(" ").toLowerCase
            case None => ""
          }

          if((theme contains obj) && (cause contains sub))
            true
          else
            false

        case _ => false
      }

      // Locate the event specified by the fields of this TSV line
      val matches = extractions filter matchesCriteria

      if(matches.length > 1)
        logger.debug("More than one event match for row \"$l\"")


      (matches.head.asInstanceOf[BioEventMention], polarity)
    }

    val data = new ArrayBuffer[(BioEventMention, Polarity)]

    try{
      val src = Source.fromFile(path)
      try{
        val lines = src.getLines().toList
        logger.info(s"About to annotate ${lines.size} statements.")
        data ++= (lines map processLine collect { case Success(l) => l })
      }
      catch {
        case e:Exception =>
          logger.error(e.getMessage)
      }
      finally {
        src.close()
      }
    }
    catch {
      case e:Exception =>
        logger.error(e.getMessage)
    }

    data
  }

  def saveOutput(digestedData: Seq[(BioEventMention, Polarity)], outputPath: String): Unit = {
    val (evts, labels) = digestedData.unzip

    val jsonEvts = MentionsOps(evts).jsonAST


    val json =
      ("annotationDate" -> Calendar.getInstance.getTime.toString) ~
      ("events" -> jsonEvts) ~
        ("labels" -> (labels map {
          case PositivePolarity => "PositivePolarity"
          case NegativePolarity => "NegativePolarity"
          case x => throw new UnsupportedOperationException(s"Unsupported polarity type $x")})
        )

    val contents = compact(render(json))

    val pw = new PrintWriter(outputPath)
    pw.print(contents)
    pw.close()
  }

  def parseArguments(args:Array[String]):(Seq[String], String) = {

    if(args.length < 2)
      throw new UnsupportedOperationException("Not enough arguments")
    else {
      val outputPath = args.last
      val inputPaths = args.dropRight(1) filter (_.toLowerCase.endsWith(".tsv"))

      (inputPaths, outputPath)
    }
  }

  val (filePaths, outputPath) = parseArguments(args)

  val digestedData = filePaths flatMap {
    p =>
      val isOpposing =
      if(p.toLowerCase contains "concurring")
        false
      else
        true

      digestTsv(p, isOpposing)
  }

  logger.info(s"Extracted ${digestedData.length} annotations")

  saveOutput(digestedData, outputPath)

  def loadAnnotations(path:String): Seq[(BioEventMention, Polarity)] = {
    val src = Source.fromFile(path)
    val txt = src.mkString
    src.close()

    val ast = parse(txt)


    val evts:Seq[JValue] =
      for{
        JObject(child) <- ast
        JField("events", e) <- child
      } yield e

    val labels:Seq[Polarity] =
      for{
        JArray(labels) <- ast \\ "polarityLabels"
        JString(label) <- labels
      } yield {
        label match {
          case "PositivePolarity" => PositivePolarity
          case "NegativePolarity" => NegativePolarity
          case s => throw new UnsupportedOperationException(s"Unrecognized polarity label $s")
        }
      }

    val mentions = JSONSerializer.toBioMentions(evts.head) collect { case evt:BioEventMention => evt }

    println(labels)

    mentions zip labels
  }
}
