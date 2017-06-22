// Sorts feature subsets by specified score (e.g. F1)
// Should call ScoreExtractor

//package org.clulab.context.ml

import java.io.FileWriter
import java.io.File
import java.io.BufferedWriter

import org.clulab.context.ml.ScoreExtractor


object SortByScore extends App {
    //Get args:
    val arguments = args
    
    //Get all scores in a map where the key is the string of feature families and the value is cross validation score (F1, Precision, or Recall):
    val scoreMap = ScoreExtractor.getScores(arguments)
    
    def write(path: String, txt: String): Unit = {
        import java.nio.file.{Paths, Files}
        import java.nio.charset.StandardCharsets
        
        Files.write(Paths.get(path), txt.getBytes(StandardCharsets.UTF_8))
    }
    
    // The output directory in which to save sorted output (in .csv):
    val outputDirectoryString = arguments(2)
    val savePath = new StringBuilder
    savePath ++= outputDirectoryString
    savePath ++= "/"
    savePath ++= "Sorted_"
    savePath ++= args(1)
    savePath ++= "_Scores.csv"
    // Sort map by value:
    //https://stackoverflow.com/questions/2972871/how-to-sort-a-scala-collection-mapjava-lang-string-int-by-its-values
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