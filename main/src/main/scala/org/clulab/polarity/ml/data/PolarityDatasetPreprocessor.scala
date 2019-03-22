package org.clulab.polarity.ml.data

import com.typesafe.scalalogging.LazyLogging
import org.clulab.polarity.{NegativePolarity, Polarity, PositivePolarity}
import org.clulab.reach.{PaperReader, ReachSystem}
import org.clulab.reach.mentions.{BioEventMention, BioMention}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Takes as input a TSV file with a polarity distant supervision dataset and digests it into REACH mentions
  */
object PolarityDatasetPreprocessor extends App with LazyLogging{

  val extractorEngine = PaperReader.reachSystem

  // TODO: Make this safer
  val filePaths = args filter (_.toLowerCase.endsWith(".tsv"))

  val digestedData = filePaths flatMap {
    p =>
      val isOpposing =
        if(p.toLowerCase contains "concurring")
          false
        else
          true

      digestTsv(p, isOpposing)
  }

  // TODO: Parametrize this
  val outputPath = ""

  saveOutput(digestedData, outputPath)

  /** Processes a TSV file */
  def digestTsv(path:String, opposingStatements:Boolean = true):Seq[(BioEventMention, Polarity)] = {

    // Processes a line of the TSV file
    def processLine(l:String):Try[(BioEventMention, Polarity)] = Try{
      // Tokenize the line
      val tokens = l.trim.split("\t")

      // Extract the arguments
      val sub = tokens(1).toLowerCase
      val obj = tokens(2).toLowerCase

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
      val extractions = extractorEngine.extractFrom(sentence, "", "")

      def matchesCriteria(m:BioMention) = m match {
        case evt:BioEventMention if evt matches "ComplexEvent" =>

          val theme = evt.themeArgs() match {
            case Some(args) =>
              args.map(_.text).mkString(" ")
            case None => ""
          }

          val cause = evt.controllerArgs() match {
            case Some(args) =>
              args.map(_.text).mkString(" ")
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
        val lines = src.getLines().take(3)
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

    data.toSeq
  }

  def saveOutput(digestedData: Array[(BioEventMention, Polarity)], outputPath: String) = ???
}
