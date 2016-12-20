package org.clulab.reach.dyce

import com.typesafe.scalalogging.LazyLogging
import org.json4s._
import org.json4s.native.JsonMethods._

import io.Source
import java.io.{File, FilenameFilter}
import java.util.Date
import java.text.SimpleDateFormat

import org.clulab.utils.Serializer

/**
  * Counts the activations on a FriesFormat output to do statistics over them
  * Created by enrique on 19/12/16.
  */
object ActivationsCounter extends App with LazyLogging{


  def parseEvents(path:String):Iterable[(String, String, Boolean)] = {

    // Read the file
    val text = Source.fromFile(path).getLines.mkString("\n")

    // Parse the code
    val json = parse(text)

    // Get the activations out of the frames
    val frames = json \ "frames"

    val activations:Iterable[Option[(String, String, Boolean)]] = for(frame <- frames.children) yield {
      val JString(id) = frame \ "frame-id"

      frame \ "type" match {
        case JString("activation") =>

          val controllers =
            for{ JArray(arg) <- frame \ "arguments"
                 JObject(o) <- arg
                 JField("type", JString(t)) <- o
                 if t == "controller"
               } yield o

          val controller = controllers.size match {
            case 1 =>
              val c = controllers(0)
              val values = for{(k,v) <- c; if k == "arg"} yield v
              val JString(controllerVal) = values(0)
              Some(controllerVal)
            case 0 => None
            case _ => throw new RuntimeException(s"An activation has more than one controller: $id")
          }

          val controlleds =
            for{ JArray(arg) <- frame \ "arguments"
                 JObject(o) <- arg
                 JField("type", JString(t)) <- o
                 if t == "controlled"
               } yield o

          val controlled = controlleds.size match {
            case 1 =>
              val c = controlleds(0)
              val values = for{(k,v) <- c; if k == "arg"} yield v
              val JString(controlledVal) = values(0)
              Some(controlledVal)
            case 0 => None
            case _ => throw new RuntimeException(s"An activation has more than one controlled: $id")
          }

          (controller, controlled) match {
            case (Some(cr), Some(cd)) =>

              frame \ "subtype" match {
                case JString("positive-activation") => Some((cr, cd, true))
                case JString("negative-activation") => Some((cr, cd, false))
                case _ => throw new RuntimeException(s"An activation isn't positive or negative: $id")
              }
            case _ => None
          }

        case _ => None
      }
    }

    activations collect { case Some(x) => x }
  }

  def parseEntities(path:String):Iterable[(String, Participant)] = {
    // Read the file
    val text = Source.fromFile(path).getLines.mkString("\n")

    // Parse the code
    val json = parse(text)

    // Get the participants out of the frames
    val frames = json \ "frames"

    val participants:Iterable[Option[(String, Participant)]] = for(frame <- frames.children) yield {
      val JString(fid) = frame \ "frame-id"

      frame \ "frame-type" match {
        case JString("entity-mention") =>
          val JString(ns) = frame \ "xrefs" \ "namespace"
          val JString(id) = frame \ "xrefs" \ "id"

          Some((fid -> Participant(ns, id)))
        case _ => None
      }
    }

    participants collect { case Some(x) => x }

  }

  def parseInfo(entitiesPath:String, eventsPath:String):Iterable[Connection] = {
    // Read the entities and make dictionary
    val entities:Map[String, Participant] = parseEntities(entitiesPath).toMap

    // Read the events
    val rawEvents = parseEvents(eventsPath)

    // Make connection objects out of the events after resolving them to the participants
    val events:Iterable[Connection] = rawEvents map {
      re =>
        val controller = entities.lift(re._1)
        val controlled = entities.lift(re._2)
        (controller, controlled) match {
          case (Some(cr), Some(cd)) => Some(Connection(cr, cd, re._3, ""))
          case _ => None
        }
    } collect { case Some(x) => x }

    events
  }

  def parseDirectory(dirPath:String):Iterable[Iterable[Connection]] = {
    val dir = new File(dirPath)

    // Get the file name prefixes
    val prefixes = dir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = if(name.endsWith("events.json")) true else false
    }).map{
      f =>
        val abPath = f.getAbsolutePath
        val prefix = abPath.split("\\.").dropRight(2).mkString(".")
        prefix
    }

    // Parse each document
    val results = prefixes.par.map{
      prefix =>
        logger.info(s"Reading $prefix")
        parseInfo(s"$prefix.entities.json", s"$prefix.events.json")
    }.seq

    // Return
    results
  }

  // Read all the directories from the command line
  val counts = args flatMap parseDirectory

  val dateFormat = new SimpleDateFormat("yyyy_MM_dd_HH_mm");
  val date = new Date();
  val stamp = dateFormat.format(date)

  // Serialize the results
  Serializer.save[Iterable[Iterable[Connection]]](counts, s"activation_counts_$stamp.ser")
}
