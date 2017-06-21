// Sorts feature subsets by specified score (e.g. F1)
// Should call ScoreExtractor

//package org.clulab.context.ml

import org.clulab.context.ml.ScoreExtractor


object SortByScore extends App {
    //Get args:
    val arguments = args

    val scoreMap = ScoreExtractor.getScores(arguments)

    // Sort map by value:
    //https://stackoverflow.com/questions/2972871/how-to-sort-a-scala-collection-mapjava-lang-string-int-by-its-values

}