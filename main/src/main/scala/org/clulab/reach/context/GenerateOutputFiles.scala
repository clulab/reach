package org.clulab.reach.context
import org.clulab.reach.ReachSystem
import com.typesafe.config.ConfigFactory
import java.io._

import scala.collection.immutable.ListMap
import ai.lum.nxmlreader.NxmlReader
import org.clulab.odin.EventMention
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.mentions.BioTextBoundMention
import org.clulab.struct.Interval
object GenerateOutputFiles extends App {
    val config = ConfigFactory.load()
    val typeOfPaper = config.getString("polarityContext.typeOfPaper")
    //val dirForType = if(typeOfPaper.length != 0) config.getString("papersDir").concat(s"/${typeOfPaper}") else config.getString("papersDir")
    val dirForType = config.getString("polarityContext.paperTypeResourceDir").concat(typeOfPaper)
    val nxmlReader = new NxmlReader(ignoreSections.toSet, transformText = preproc.preprocessText)
    val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
    lazy val reachSystem = new ReachSystem(processorAnnotator = Some(procAnnotator),
        contextEngineType = contextEngineType,
        contextParams = contextEngineParams)


    // ********* Writing to sentences.txt ************
    // reading nxml doc and converting it to Document, to create sentences.txt file
    val fileListUnfiltered = new File(dirForType)
    val fileList = fileListUnfiltered.listFiles().filter(x => x.getName.endsWith(".nxml"))
    for(file <- fileList) {
        val pmcid = file.getName.slice(0,file.getName.length-5)
        val outPaperDirPath = config.getString("svmContext.contextOutputDir").concat(s"${typeOfPaper}/${pmcid}")
        // creating output directory if it doesn't already exist
        val outputPaperDir = new File(outPaperDirPath)
        if(!outputPaperDir.exists()) {
            outputPaperDir.mkdirs()
        }
        val pathForSentences = outPaperDirPath.concat("/sentences.txt")
        val pathForEvents = outPaperDirPath.concat("/event_intervals.txt")
        val pathForContextMentions = outPaperDirPath.concat("/mention_intervals.txt")


        val nxmlDoc = nxmlReader.read(file)
        val document = reachSystem.mkDoc(nxmlDoc)
        val collectSent = collection.mutable.ListBuffer[String]()
        for (s <- document.sentences) {
            val currentSent = s.words.mkString(" ")
            collectSent += currentSent
        }

        val sentenceFile = new File(pathForSentences)
        if (!sentenceFile.exists()) {
            sentenceFile.createNewFile()
        }
        val pw = new PrintWriter(sentenceFile)
        for (sen <- collectSent) {
            pw.write(sen)
            pw.write("\n")
        }
        pw.close()
        // ********* Finished writing to sentences.txt ************


        // ********* Writing to event_intervals.txt *********
        val mentions = reachSystem.extractFrom(document)
        val evtMentionsOnly = mentions.collect { case evt: EventMention => (evt.sentence, evt.tokenInterval) }
        val groupBySentIndEvt = evtMentionsOnly.groupBy(_._1)
        val sorted = ListMap(groupBySentIndEvt.toSeq.sortBy(_._1): _*)
        val eventsFile = new File(pathForEvents)
        if (!eventsFile.exists()) {
            eventsFile.createNewFile()
        }
        val pwevent = new PrintWriter(eventsFile)
        for ((sentInd, group) <- sorted) {
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
        // ********* Finished writing to event_intervals.txt *********

        // ********* Writing to mention_intervals.txt *********
        val contextMentions = mentions filter ContextEngine.isContextMention map (_.asInstanceOf[BioTextBoundMention])
        val groupsBySentIndexCtx = contextMentions.groupBy(_.sentence)
        val sortedContextGroups = ListMap(groupsBySentIndexCtx.toSeq.sortBy(_._1): _*)
        val mentionsFile = new File(pathForContextMentions)
        if (!mentionsFile.exists())
            mentionsFile.createNewFile()

        val pwctx = new PrintWriter(mentionsFile)
        for ((sentId, ctxGroup) <- sortedContextGroups) {
            val ctx = ctxGroup.map(bt => (bt.nsId(), bt.tokenInterval, bt.sentenceObj))
            val ctxMentionStr = ctx.map(c => {
                var trigger = c._3.words.slice(c._2.start, c._2.end + 1)
                if (trigger(trigger.length - 1) == "and" || trigger(trigger.length - 1) == ",")
                    trigger = trigger.slice(0, trigger.length - 1)
                val triggerWords = trigger.mkString("_")
                val s = s"${c._2.start}%${c._2.end}%${triggerWords}%${c._1}"
                s
            })
            val mk = ctxMentionStr.mkString(" ")
            val str = s"${sentId.toString} ${mk}"
            pwctx.write(str)
            pwctx.write("\n")
        }
        pwctx.close()
        // ********* Finished writing to mention_intervals.txt *********
    }








}




