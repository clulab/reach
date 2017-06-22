/***
  *  Extracts scores from .ser files in a directory
  */


package org.clulab.context.ml

import com.typesafe.config.ConfigFactory
//import utils.Serializer
import org.clulab.utils.Serializer
import java.io.File
import scala.math.BigDecimal
import org.apache.commons.io.FileUtils


object ScoreExtractor{
    //getListofFiles from http://alvinalexander.com/scala/how-to-list-files-in-directory-filter-names-scala
    def getListOfFiles(dir: File, extensions: List[String]): List[File] = {
      dir.listFiles.filter(_.isFile).toList.filter { file =>
        extensions.exists(file.getName.endsWith(_))
      }
    }

    def getScores(arguments:Array[String]):Map[String, BigDecimal] = {
        // Gets the scores of specified type.  Function currently reads in args
        // Get the input directory containing the .ser files:
        val inputDirectoryString = arguments(0)
        // Get the Score type, of either 'F1', 'Precision', or 'Recall':
        val scoreTypeString = arguments(1)

        println(s"Extracting ${scoreTypeString} scores.")

        // Get list of .ser files:
        // Get certain extension: http://www.avajava.com/tutorials/lessons/how-do-i-get-all-files-with-certain-extensions-in-a-directory-including-subdirectories.html
        //val files = (new File(inputDirectory)).listFiles
        val extensions:List[String] = List("ser")
        val files:List[File] = getListOfFiles(new File(inputDirectoryString), extensions)

        println("Found Files:")
        files.foreach {
            file => println(file.toString)
        }

        // Read in serialized files and extract score:
        //Instatiate empty map:
        var subsetScoreMap = Map.empty[String, BigDecimal]
        //var i = 0
        files.foreach {
            file =>
            println(s"Reading in serialized file: ${file}")
            //READ IN SERIALIZED FILE:
            val results:CrossValResults = Serializer.load(file.toString)

            // Extract feature families to be stored as the key of subsetScoreMap:
            println(s"Set of feature families:")
            var key = ""
            results.featureFamilies.foreach {
                featureFamily =>
                    key ++= featureFamily.toString.dropRight(2)
                    key ++= "_"
            }

            // drop last underscore in key:
            key = key.dropRight(1)
            println(key)
            //
            //

            // Extract score:
            scoreTypeString match{
                case "F1" =>  println(s"Extracting ${scoreTypeString} Score:")
                case "Precision" => println(s"Extracting ${scoreTypeString} Score:")
                case "Recall" =>  println(s"Extracting ${scoreTypeString} Score:")
                case _ => throw new RuntimeException("Unexpected score type string.  Second argument to Scala application should be: F1, Precision, or Recall.")
            }
            //Get score type:
            def matchScoreType(scoreType:String, results:CrossValResults):BigDecimal = {
                scoreType match {
                    case "F1" => results.results.f1Score
                    case "Precision" => results.results.precision
                    case "Recall" => results.results.recall
                }
            }
            val scoreValue:BigDecimal = matchScoreType(scoreTypeString, results)
            println(s"${scoreTypeString} Score:")
            println(scoreValue)

            // Add key-value pair to Map:
            //val scoreValue = 1.234 * i
            subsetScoreMap += (key -> scoreValue)
            println(subsetScoreMap)
            //i += 1
    }
    // Return map:
    subsetScoreMap
  }
}