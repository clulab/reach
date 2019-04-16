package org.clulab.reach.context
import org.clulab.reach.{ReachSystem}
import com.typesafe.config.ConfigFactory
import java.io._

import scala.collection.immutable.ListMap
import ai.lum.nxmlreader.NxmlReader
import org.clulab.odin.EventMention
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.mentions.BioTextBoundMention
object GenerateOutputFiles extends App {
    println("Inside generate output class")
    val config = ConfigFactory.load()
    val currentPaperPath = config.getString("papersDir").concat("/PMC420486.nxml")
    val pathForSentences = config.getString("contextEngine.params.sentencesToFile")
    val pathForEvents = config.getString("contextEngine.params.eventIntervalsToFile")
    val pathForContextMentions = config.getString("contextEngine.params.contextMentionIntervalsToFile")
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
        pw.write("\n")
    }
    pw.close()


    // extracting mentions to write to evtIntervals.txt
    val mentions = reachSystem.extractFrom(document)
    val evtMentionsOnly  = mentions.collect{case evt:EventMention => (evt.sentence, evt.tokenInterval)}
    val groupBySentIndEvt = evtMentionsOnly.groupBy(_._1)
    val sorted = ListMap(groupBySentIndEvt.toSeq.sortBy(_._1):_*)
    val pwevent = new PrintWriter(pathForEvents)
    for((sentInd, group) <- sorted) {
        val inter = group.map(_._2)
        val intervalStringed = inter.map(i => {
            val str = s"${i.start.toString}-${i.end.toString}"
            str
        })
        val string = intervalStringed.mkString(" ")
        val toWrite = s"${sentInd.toString} ${string}"
        pwevent.write(toWrite)
        pwevent.write("\n")
    }
    pwevent.close()

    // extracting context mentions to write to file
    val contextMentions = mentions filter ContextEngine.isContextMention map (_.asInstanceOf[BioTextBoundMention])
    val groupsBySentIndexCtx = contextMentions.groupBy(_.sentence)
    val sortedContextGroups = ListMap(groupsBySentIndexCtx.toSeq.sortBy(_._1):_*)
    val pwctx = new PrintWriter(pathForContextMentions)
    for((sentId, ctxGroup) <- sortedContextGroups) {
        val ctx = ctxGroup.map(bt => (bt.nsId(), bt.tokenInterval, bt.sentenceObj))
        val ctxMentionStr = ctx.map(c => {
            val trigger = c._3.words.slice(c._2.start, c._2.end+1)
            val triggerWords = trigger.mkString(" ")
            val s = s"${c._2.start}%${c._2.end}%${triggerWords}%${c._1}"
            s
        })
        val mk = ctxMentionStr.mkString(" ")
        val str = s"${sentId.toString} ${mk}"
        pwctx.write(str)
        pwctx.write("\n")
    }
    pwctx.close()







}




