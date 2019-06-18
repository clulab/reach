package org.clulab.reach.context.context_exec

import java.io.{File, PrintWriter}

import ai.lum.nxmlreader.NxmlReader
import com.typesafe.config.ConfigFactory
import org.clulab.odin.EventMention
import org.clulab.reach.PaperReader.{contextEngineParams, ignoreSections, preproc, procAnnotator}
import org.clulab.reach.ReachSystem
import org.clulab.reach.context.ContextEngine
import org.clulab.reach.context.ContextEngineFactory.Engine
import org.clulab.reach.context.context_utils.EventContextPairGenerator
import org.clulab.reach.mentions.{BioEventMention, BioTextBoundMention}

import scala.collection.immutable.ListMap

object GenerateOutputFiles extends App {
    // This class runs the papers through reach, and writes to file mention_intervals.txt, event_intervals.txt and sentences.txt.
    // These files are required to run the annotator web app that was designed by Zechy.
    val config = ConfigFactory.load()
    val typeOfPaper = config.getString("polarityContext.typeOfPaper")
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
        /*contents of the sentences.txt file are in the following format:
        sentence0
        sentence1
        sentence2
          .
          .
          .
        sentencen
        The sentences are written by line in increasing order of sentenceIndex, as detected by reach.
         */
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
        // for a given sentence index 1, if there is one event mention from token 3 to 5 and one from 7 to 9,
        // the file will contain the line: 1 3-5 7-9
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
        // This file logs the context mentions line by line, and each looks something like this:
        // 0 12%14%Stem_Cell_Hypothesis%cl:CL:0000034
        // This means that in sentence 0, between the tokens 12 and 14, we found the words Stem Cell Hypothesis, whose grounding ID is cl:CL:0000034
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


        // ********* Writing to polarity file *********
        type Pair = (BioEventMention, BioTextBoundMention)
        type EventID = String
        type ContextID = (String, String)
        val pathForPolarityOutput = outPaperDirPath.concat("/polarity_output.csv")
        val crossProducter = new EventContextPairGenerator(mentions, contextMentions)
        val pairs = crossProducter.yieldContextEventPairs()
        val activationPapers = List("PMC2958340", "PMC2910130", "PMC4236140", "PMC4142739", "PMC4446607", "PMC4092102")
        val inhibitionPapers = List("PMC2587086", "PMC3138418", "PMC3666248", "PMC2636845", "PMC3635065", "PMC3640659", "PMC2686753", "PMC3119364")
        val polarityFile = new File(pathForPolarityOutput)
        if (!polarityFile.exists())
            polarityFile.createNewFile()
        val printWriter = new PrintWriter(polarityFile)
        val header = List("PMCID", "evtID", "ctxID", "polarity_type", "evt_sentence", "ctx_sentence")
        val headStr = header.mkString(",")
        printWriter.write(headStr)
        printWriter.write("\n")

        def extractEvtId(evt:BioEventMention):EventID = {
            val sentIndex = evt.sentence
            val tokenIntervalStart = (evt.tokenInterval.start).toString()
            val tokenIntervalEnd = (evt.tokenInterval.end).toString()
            sentIndex+tokenIntervalStart+tokenIntervalEnd
        }

        for((evtID, ctxID) <- pairs) {
            val perRowEntry = collection.mutable.ListBuffer[String]()
            val docId = evtID.document.id match {
                case Some(id) => s"PMC${id.split("_")(0)}"
                case None => "Unknown"
            }
            perRowEntry += docId
            perRowEntry += extractEvtId(evtID)
            perRowEntry += ctxID.nsId()
            val polarityType = if(activationPapers.contains(docId)) "activation" else if(inhibitionPapers.contains(docId)) "inhibition" else "unknown"
            perRowEntry += polarityType
            val evtSentInd = evtID.sentence
            val evtSent = document.sentences(evtSentInd).words.mkString(" ")
            perRowEntry += evtSent
            val ctxSentID = ctxID.sentence
            val ctxSent = document.sentences(ctxSentID).words.mkString(" ")
            perRowEntry += ctxSent
            val perRowString = perRowEntry.mkString(",")
            printWriter.write(perRowString)
            printWriter.write("\n")
        }
        // ********* Finished writing to polarity file *********

    }










}
