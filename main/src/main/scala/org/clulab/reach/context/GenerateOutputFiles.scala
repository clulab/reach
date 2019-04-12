package org.clulab.reach.context
import org.clulab.reach.{PaperReader, ReachSystem}
import com.typesafe.config.ConfigFactory
import java.io._
import scala.collection.immutable.ListMap
import ai.lum.nxmlreader.NxmlReader
import org.clulab.odin.EventMention
import org.clulab.reach.PaperReader.{config, contextEngineParams, ignoreSections, nxmlReader, preproc, procAnnotator}
import org.clulab.reach.context.ContextEngineFactory.Engine
object GenerateOutputFiles extends App {
    val config = ConfigFactory.load()
    val currentPaperPath = config.getString("papersDir").concat("/PMC420486.nxml")
    val pathForSentences = config.getString("contextEngine.params.sentencesToFile")
    val pathForEvents = config.getString("contextEngine.params.eventIntervalsToFile")
    val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
    val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
    lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
        contextEngineType = contextEngineType,
        contextParams = contextEngineParams)
    val file = new File(currentPaperPath)

    // reading nxml doc and converting it to Document, to create sentences.txt file
    val nxmlDoc = nxmlReader.read(file)
    val document = reachSystem.mkDoc(nxmlDoc)
    val collectSent = collection.mutable.ListBuffer[String]()
    for(s <- document.sentences) {
        val currentSent = s.words.mkString(" ")
        collectSent += currentSent
    }

    val pw = new PrintWriter(pathForSentences)
    for(sen <- collectSent) {
        pw.write(sen)
    }
    pw.close()


    // extracting mentions to write to evtIntervals.txt
    val mentions = reachSystem.extractFrom(document)
    val evtMentionsOnly  = mentions.collect{case evt:EventMention => (evt.sentence, evt.tokenInterval)}
    val groupBySentInd = evtMentionsOnly.groupBy(_._1)
    val sorted = ListMap(groupBySentInd.toSeq.sortBy(_._1):_*)
    val pwevent = new PrintWriter(pathForEvents)
    for((sentInd, group) <- sorted) {
        val inter = group.map(_._2)
        val intervalStringed = inter.map(i => {
            val str = s"${i.start.toString}-${i.end.toString}"
            str
        })
        val string = intervalStringed.mkString(" ")
        val toWrite = sentInd.toString.concat(string)
        pwevent.write(toWrite)
    }
    pwevent.close()




}




