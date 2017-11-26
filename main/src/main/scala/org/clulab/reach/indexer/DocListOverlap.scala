package org.clulab.reach.indexer

import java.io.{FileWriter, PrintWriter}

import scala.io.Source
import scala.collection.mutable

/**
  * Created by mihais on 6/7/17.
  */
object DocListOverlap {
  def main(args:Array[String]): Unit = {
    val oldFile = args(0)
    val newFile = args(1)

    val oldDocs = readDocNames(oldFile)
    val newDocs = readDocNames(newFile)
    println(s"Found ${newDocs.size} new documents.")

    val pw = new PrintWriter(new FileWriter("diff.txt"))
    for(nn <- newDocs) {
      if(oldDocs.contains(nn)) pw.print("OLD ")
      else pw.print("NEW ")
      pw.println(nn)
    }
    pw.close()
  }

  def readDocNames(fn:String): Set[String] = {
    val names = new mutable.HashSet[String]()
    for(l <- Source.fromFile(fn).getLines()) {
      val tokens = l.split("\\s+")
      if(tokens.length > 0) {
        val n = tokens(0).replaceAll(".json", "")
        names += n
      } else {
        println("Invalid line: " + l)
      }
    }
    names.toSet
  }
}
