// Sorts feature subsets by specified score (F1, Precision, or Recall).
// First argument is the input directory of serialized results
// Second argument is the score type (F1, Precision, Recall, or All).  Providing All returns all scores but sorted by F1


//package org.clulab.context.ml

import java.io.FileWriter
import java.io.File
import java.io.BufferedWriter

import org.clulab.context.ml.ScoreExtractor


object SortByScore extends App {
    //Get args:
    val arguments = args
    val scoreTypeString = arguments(1)
    
    // if you only want one score type:
    val stringListScoreTypes = List("F1", "Precision", "Recall")
    if (stringListScoreTypes.contains(scoreTypeString)) {
        //Get all scores in a map where the key is the string of feature families and the value is cross validation score (F1, Precision, or Recall):
        val scoreMap = ScoreExtractor.getScores(arguments)
        
        // The output directory in which to save sorted output (in .csv):
        val outputDirectoryString = arguments(2)
    
        val savePath = new File(new File(outputDirectoryString), s"Sorted_${args(1)}_Scores.csv").getAbsolutePath
        
        // Sort map by value:
        //https://stackoverflow.com/questions/2972871/how-to-sort-a-scala-collection-mapjava-lang-string-int-by-its-values
        // Sort map by value in tuple:
        // https://stackoverflow.com/questions/26445478/scala-sort-a-map-based-on-tuple-values
        println("Sorted feature subsets:")
        val sortedSB = new StringBuilder
        scoreMap.toSeq.sortBy(-_._2).foreach{
            keyVal =>
            //println(keyVal.toString)
            var keyValString = keyVal.toString.drop(1)
            keyValString = keyValString.dropRight(1)
            println(keyValString)
            sortedSB ++= keyValString
            sortedSB ++= "\n"
        }
        print(sortedSB.toString())
        val file = new File(savePath.toString)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(sortedSB.toString)
        bw.close()
        println(s"Sorted file written to: ${savePath.toString()}")
    }
    else {
        val scoreMap = ScoreExtractor.getScores(arguments)
    
        // The output directory in which to save sorted output (in .csv):
        val outputDirectoryString = arguments(2)
    
        val savePath = new File(new File(outputDirectoryString), s"Sorted_${args(1)}_Scores.csv").getAbsolutePath
    
        // Sort map by value:
        //https://stackoverflow.com/questions/2972871/how-to-sort-a-scala-collection-mapjava-lang-string-int-by-its-values
        // Sort map by value in tuple:
        // https://stackoverflow.com/questions/26445478/scala-sort-a-map-based-on-tuple-values
        println("Sorted feature subsets:")
        val sortedSB = new StringBuilder
        scoreMap.toSeq.sortBy(-_._2).foreach{
            keyVal =>
                println(keyVal.toString)
                var keyValString = keyVal.toString.drop(1)
                keyValString = keyValString.dropRight(1)
                println(keyValString)
                sortedSB ++= keyValString
                sortedSB ++= "\n"
        }
        print(sortedSB.toString())
        val file = new File(savePath.toString)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(sortedSB.toString)
        bw.close()
        println(s"Sorted file written to: ${savePath.toString()}")
    }
}