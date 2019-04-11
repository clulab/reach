package org.clulab.reach.context
import org.clulab.reach.PaperReader
import com.typesafe.config.ConfigFactory
import java.io._
object GenerateOutputFiles extends App {
    val config = ConfigFactory.load()
    val currentPaperPath = config.getString("papersDir").concat("/PMC420486.nxml")
    val file = new File(currentPaperPath)
    val (_, mentions) = PaperReader.readPaper(file)
    println(mentions.size + " number of mentions captured for current paper")
}




