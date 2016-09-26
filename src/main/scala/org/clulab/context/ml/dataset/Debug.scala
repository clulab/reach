package org.clulab.context.ml.dataset

import java.io.File
import org.clulab.serialization._

object Debug extends App{

  val datasetDir = args(0)
  val dirs = new File(datasetDir).listFiles.filter(_.isDirectory)

  for(d <- dirs){
    val anns = ArticleAnnotations.readPaperAnnotations(d.getPath)
    // println
    // println(s"== ${d.getPath} ==")
    // val doc = new DocumentSerializer().load(anns.preprocessed.get.serializedDoc)
    // val sentences = anns.sentences
    //
    // if(doc.sentences.size != sentences.size){
    //   println(s"${d.getPath} sentences: ${sentences.size}\tdoc sentences: ${doc.sentences.size}")
    // }
    //
    // println("First sentences:")
    // println(sentences(0))
    // println(doc.sentences(0).getSentenceText)
    // println
    // println("Last sentences:")
    // println(sentences(sentences.size-1))
    // println(doc.sentences(sentences.size-1).getSentenceText)

    // println("==================")
    // doc.sentences.map(_.getSentenceText).foreach(println)
    // (0 until sentences.size).map(doc.sentences).map(_.getSentenceText).foreach(println)

  }
}
