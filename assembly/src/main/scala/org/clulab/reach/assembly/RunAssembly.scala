package org.clulab.reach.assembly

import org.apache.commons.io.FilenameUtils
import org.clulab.reach.assembly.relations.corpus.{Corpus, CorpusReader, EventPair}
import org.clulab.odin.Mention
import org.clulab.reach.PaperReader
import org.clulab.reach.mentions._
import org.clulab.reach.mentions.serialization.json._
import org.clulab.utils.Serializer

import scala.collection.parallel.ForkJoinTaskSupport
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import java.io.File

import org.clulab.reach.assembly.relations.SieveEvaluator
import org.clulab.reach.assembly.relations.SieveEvaluator.Performance
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import org.json4s._
import ai.lum.common.FileUtils._
import java.nio.charset.StandardCharsets.UTF_8

import org.clulab.reach.{ReachSystem, context}
import org.clulab.processors.bionlp.BioNLPProcessor

import scala.collection.mutable.ArrayBuffer

object RunAnnotationEval extends App with LazyLogging {

  val config = ConfigFactory.load()
//  val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDir")).instances

  // gather precedence relations corpus
  val evalGoldPath = config.getString("assembly.evalGold")
  val evalMentionsPath = config.getString("assembly.evalMentions")

  val (posGold, testMentions) = {

    if(new File(evalGoldPath).exists & new File(evalMentionsPath).exists) {
      logger.info("Serialized files exist")
      val pg = Serializer.load[Seq[PrecedenceRelation]](evalGoldPath)
      val tm = Serializer.load[Seq[Mention]](evalMentionsPath)
      (pg, tm)
    } else {
      logger.info("Serialized files not found")
      val epsOld: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDirOldEval")).instances
      val newMentions = Corpus.loadMentions(config.getString("assembly.corpus.corpusDirNewEval"))
      val eps = Corpus.softAlign(epsOld, newMentions)
      // gather precedence relations corpus
//      val precedenceAnnotations = CorpusReader.filterRelations(eps, precedenceRelations)
//      val noneAnnotations = CorpusReader.filterRelations(eps, noRelations ++ subsumptionRelations ++ equivalenceRelations)

      val (posGoldNested, testMentionsNested) = (for {
        ep <- eps
        e1 = ep.e1
        e2 = ep.e2
      } yield {
        // short-term assembly manager to get at mentions easier
        val am = AssemblyManager()
        am.trackMentions(Seq(e1, e2))
        val goldRel = ep.relation match {
          case "E1 precedes E2" =>
            Seq(PrecedenceRelation(am.getEER(e1), am.getEER(e2), Set.empty[Mention], "gold"))
          case "E2 precedes E1" =>
            Seq(PrecedenceRelation(am.getEER(e2), am.getEER(e1), Set.empty[Mention], "gold"))
          case _ => Nil
        }
        (goldRel, Seq(e1, e2))
      }).unzip

      val pg = posGoldNested.flatten.seq
      val tm = testMentionsNested.flatten.distinct.seq

      (pg, tm)
    }
  }

  println("="*20)
  println(s"num of labels:${posGold.length}")
  println("="*20)

  println("sieve\trule\tp\tr\tf1\ttp\tfp\tfn")

  for {
    (lbl, sieveResult) <- SieveEvaluator.applyEachSieve(testMentions)
  } {
    val predicted = sieveResult.getPrecedenceRelations
    val smoothing = 0.00001

    // TODO: check with Gus. The original expression was "posGold exists(...)". Is this equivalent to "posGold.exists(...)"?
    val tp = predicted.count(p => posGold.exists(g => g.isEquivalentTo(p, ignoreMods = false)))
    val fp = predicted.count(p => ! posGold.exists(g => g.isEquivalentTo(p, ignoreMods = false)))
    val fn = posGold.count(g => ! predicted.exists(p => p.isEquivalentTo(g, ignoreMods = false)))

    // micro performance
    val p = tp / (tp + fp + smoothing)
    val r = tp / (tp + fn + smoothing)
    val f1 = (2 * p * r) / (p + r + smoothing)

    // for the whole sieve
    val sievePerformance = Performance(lbl, "**ALL**", p, r, f1, tp, fp, fn)

    val rulePerformance: Seq[Performance] = {
      val rulePs = predicted.groupBy(pr => (pr.foundBy, pr.evidence.head.foundBy))
      val allRtp = rulePs.mapValues(_.count(p => posGold exists(g => g.isEquivalentTo(p, ignoreMods = false))))
      val allRfp = rulePs.mapValues{_.count{p =>
        val isFP = ! posGold.exists(g => g.isEquivalentTo(p, ignoreMods = false))
        //if(isFP) displayMention(p.evidence.head)
        isFP
      }
      }
      val allRfn = {
        val res = for {
          (foundBy, group) <- rulePs
          gold = posGold.count(g => ! group.exists(p => p.isEquivalentTo(g, ignoreMods = false)))
        } yield (foundBy, gold)
        res
      }

      val rp = for {
        foundBy <- rulePs.keys
      } yield {
        val tp = allRtp.getOrElse(foundBy, 0)
        val fp = allRfp.getOrElse(foundBy, 0)
        val fn = allRfn.getOrElse(foundBy, 0)

        // micro performance
        val p = tp / (tp + fp + smoothing)
        val r = tp / (tp + fn + smoothing)
        val f1 = (2 * p * r) / (p + r + smoothing)

        // for the rule
        Performance (foundBy._1, foundBy._2, p, r, f1, tp, fp, fn)
      }
      rp.toSeq
    }

    (rulePerformance :+ sievePerformance).sortBy(_.p).foreach(perf => println(perf.mkRow))
  }
}

object EvalUnlabeldEventPairs extends App with LazyLogging {

  val evalMentionsPath = "/work/zhengzhongliang/2020_ASKE/20210117/paper_0_1000/mention-data/"
  val testMentions = ArrayBuffer[Mention]()
  Corpus.loadMentions(evalMentionsPath).foreach{keyValuePair => testMentions.appendAll(keyValuePair._2)}

  for {
    (lbl, sieveResult) <- SieveEvaluator.applyEachSieve(testMentions)
  } {
    val predicted = sieveResult.getPrecedenceRelations

    println(s"number of test mentions:${testMentions.length}")
    println(s"number of precedence relationships:${predicted.size}")

    for (precedRel <- predicted){
      println("\t"+"-"*20)
      println("\t"+s"evidence size:${precedRel.evidence.size}")
    }

    scala.io.StdIn.readLine("waiting for the next sieve")
  }


}

/**
  * Serialize each paper in a directory to json
  */
object SerializePapersToJSON extends App with LazyLogging {

  import org.clulab.reach.mentions.serialization.json._

  val config = ConfigFactory.load()
  val papersDir = new File(config.getString("papersDir"))
  val outDir = new File(config.getString("outDir"))
  // the number of threads to use for parallelization
  val threadLimit = config.getInt("threadLimit")

  logger.info(s"papersDir: ${papersDir.getAbsolutePath}")
  logger.info(s"outDir: ${outDir.getAbsolutePath}")
  logger.info(s"threads: $threadLimit")
  val papers = papersDir.listFiles.par
  papers.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))

  for {
    paper <- papers
    paperID = FilenameUtils.removeExtension(paper.getName)
    outFile = new File(s"$outDir/$paperID.json")
    if !outFile.exists
  } {
    val mentions = PaperReader.getMentionsFromPaper(paper)
    val cms: Seq[CorefMention] = mentions.map(_.toCorefMention)
    logger.info(s"extracted ${mentions.size} mentions for $paperID")
    cms.saveJSON(outFile, pretty = true)
    logger.info(s"saved json to $outFile")
  }
}

object SerializePapersToJSONByMentionData extends App with LazyLogging{
  implicit val formats = DefaultFormats

  val config = ConfigFactory.load()
  val corpusDirOldTrain = config.getString("assembly.corpus.corpusDirOldTrain")
  val corpusDirOldEval = config.getString("assembly.corpus.corpusDirOldEval")
  //val jsonDirOldTrain = config.getString("assembly.corpus.jsonDirOldTrain")
  //val jsonDirOldEval = config.getString("assembly.corpus.jsonDirOldEval")

  val corpusDirNewTrain = config.getString("assembly.corpus.corpusDirNewTrain")
  val corpusDirNewEval = config.getString("assembly.corpus.corpusDirNewEval")
  //val jsonDirNewTrain = config.getString("assembly.corpus.jsonDirNewTrain")
  //val jsonDirNewEval = config.getString("assembly.corpus.jsonDirNewEval")

  val threadLimit = config.getInt("threadLimit")

  val procAnnotator = new BioNLPProcessor()
  lazy val reachSystem = new ReachSystem(
    processorAnnotator = Some(procAnnotator)
  )

  private case class AssemblyAnnotation(
                                         id: Int,
                                         text: String,
                                         relation: String,
                                         `annotator-id`: String,
                                         coref: Boolean,
                                         `e1-id`: String,
                                         `e2-id`: String,
                                         confidence: Double,
                                         `paper-id`: String,
                                         notes: Option[String]
                                       )

  private case class PMIDAnnotation(`paper-id`: String) //used to get the paper ID from the event pairs json.

  def readPMIDsFromOldEventPairs(corpusDirOld:String):Seq[String] = {
    val epsJAST = parse(new File(corpusDirOld, s"event-pairs.json"))
    val allPMIDs = for {aa <- epsJAST.extract[Seq[PMIDAnnotation]]} yield aa.`paper-id`

    allPMIDs.distinct
  }

  def readDocumentTextByPMID(corpusDirOld:String):Map[String, String] = {
    val docDataDir = corpusDirOld+"/mention-data/"
    val pmids = readPMIDsFromOldEventPairs(corpusDirOld)

    var numHit = 0
    var numMiss = 0
    val docTextMap = scala.collection.mutable.Map[String, String]()
    for (pmid <- pmids) {
      val docFileName = docDataDir+pmid.toString+"-mention-data.json"
      if (new File(docFileName).exists()){
        numHit+=1
        // TODO: not sure there is a better choice to parse the document json
        val documentObjRaw = parse(new File(docFileName)).extract[Map[String, Any]]
        val documentObj = documentObjRaw("documents").asInstanceOf[Map[String, Any]]
        val paperIDInFile = documentObj.keys.head.toString

        val documentText = documentObj(paperIDInFile).asInstanceOf[Map[String, String]]("text")

        docTextMap(pmid.toString) = documentText.toString
      }
      else {numMiss+=1}
    }
    println(s"Num paper matched: ${numHit}, Num paper missed: ${numMiss}")
    docTextMap.toMap
  }

  def annotateDocumentsAndSaveOutput(corpusDirOld:String, corpusDirNew:String):Unit = {
    val docTextMap = readDocumentTextByPMID(corpusDirOld)

    //    val contextEngineType = Engine.withName(config.getString("contextEngine.type"))
    //    val contextConfig = config.getConfig("contextEngine.params").root
    //    val contextEngineParams: Map[String, String] =
    //      context.createContextEngineParams(contextConfig)
    // initialize ReachSystem

    val allPapers = docTextMap.toSeq.par // allDocs is a Seq of tuple ((id, text), (id, text), (id, text), ...)
    allPapers.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadLimit))

    var nDone = 0
    var nSkipped = 0
    for ((paperID, paperText) <- allPapers){
      logger.info(s"processing paper $paperID")
      try {
        val outFile = new File(s"$corpusDirNew/mention-data/$paperID-mention-data.json")

        val mentions = reachSystem.extractFrom(paperText, paperID, "")
        val cms: Seq[CorefMention] = mentions.map(_.toCorefMention)
        cms.saveJSON(outFile, pretty = false)
        logger.info(s"\tsaved json to $outFile")
        nDone+=1
      }
      catch{
        case _ => {
          nSkipped+=1
          logger.info("\tpaper skipped")
        }

      }
    }
    logger.info(s"processing done! N done: ${nDone}, N skipped: ${nSkipped}")

    //writeJSON(paperMentionsMap.toMap, corpusDirNew, false) //what does this pretty do?
  }

  annotateDocumentsAndSaveOutput(corpusDirOldTrain, corpusDirNewTrain)
  annotateDocumentsAndSaveOutput(corpusDirOldEval, corpusDirNewEval)
}