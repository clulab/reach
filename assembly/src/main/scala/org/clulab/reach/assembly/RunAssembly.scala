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

object EvalUnlabeledEventPairs extends App with LazyLogging {

  val evalMentionsPath = "/work/zhengzhongliang/2020_ASKE/20210117/"

  val testCorpus = Corpus(evalMentionsPath)

  println(s"number of event pairs: ${testCorpus.instances.length}")

  // building event pair "event hash to index"
  val hash2IdxMap = scala.collection.mutable.Map[String, Int]()
  val mentionHashIdxMap = scala.collection.mutable.Map[String, Int]()
  val mentionFeatureIdxMap = scala.collection.mutable.Map[String, Int]()
  for (idx <- testCorpus.instances.indices){
    val ep = testCorpus.instances(idx)

    val e1Hash = ep.e1.hashCode().toString
    val e2Hash = ep.e2.hashCode().toString

    val e1Features = ep.e1.document.id.getOrElse("") + "," + ep.e1.sentence.toString + "," + ep.e1.start.toString + "," + ep.e1.end.toString
    val e2Features = ep.e2.document.id.getOrElse("") + "," + ep.e2.sentence.toString + "," + ep.e2.start.toString + "," + ep.e2.end.toString

    if (hash2IdxMap.contains(e1Features +";"+e2Features)) {
      println("repeated event pair encountered when building event pair index!")
    }
    else{
      hash2IdxMap(e1Features +";"+e2Features) = idx
    }

    if (mentionHashIdxMap.contains(e1Hash)) {mentionHashIdxMap(e1Hash) +=1 } else {mentionHashIdxMap(e1Hash) = 1}
    if (mentionHashIdxMap.contains(e2Hash)) {mentionHashIdxMap(e2Hash) +=1 } else {mentionHashIdxMap(e2Hash) = 1}

    if (mentionFeatureIdxMap.contains(e1Features)) {mentionFeatureIdxMap(e1Features) += 1} else {mentionFeatureIdxMap(e1Features) = 1}
    if (mentionFeatureIdxMap.contains(e2Features)) {mentionFeatureIdxMap(e2Features) += 1} else {mentionFeatureIdxMap(e2Features) = 1}

  }

  println(mentionHashIdxMap.map{x => x._2>1})
  println(mentionFeatureIdxMap.map{x => x._2>1})

  for {
    (lbl, sieveResult) <- SieveEvaluator.applyEachSieve(testCorpus.mentions)
  } {
    val predicted = sieveResult.getPrecedenceRelations
    val fullPredLabelsListToSave = ArrayBuffer[(Int, Int)]()

    var invalidMentionHashCount  = 0
    var invalidMentionFeatureCount = 0

    for (precedRel <- predicted){
      // The event in the prediction can be accessed by: precedRel.before.sourceMention.get.text
      // The event hash can be accessed by: precedRel.before.sourceMention.get.hashCode().toString
      val e1 = precedRel.before.sourceMention.get
      val e2 = precedRel.after.sourceMention.get
      val e1Features = e1.document.id.getOrElse("") + "," + e1.sentence.toString + "," + e1.start.toString + "," + e1.end.toString
      val e2Features = e2.document.id.getOrElse("") + "," + e2.sentence.toString + "," + e2.start.toString + "," + e2.end.toString

      val e1Hash = e1.hashCode().toString
      val e2Hash = e2.hashCode().toString

      if (hash2IdxMap.contains(e1Features +";"+e2Features)){
        fullPredLabelsListToSave.append((hash2IdxMap(e1Features +";"+e2Features), 1))  // E1 precedes E2
      }

      else if (hash2IdxMap.contains(e2Features +";"+e1Features)) {
        fullPredLabelsListToSave.append((hash2IdxMap(e2Features +";"+e1Features), 2))   // E2 precedes E1

      }
      else {
        println("This should not happen!")
      }

      if (!mentionHashIdxMap.contains(e1Hash)) {invalidMentionHashCount += 1}
      if (!mentionHashIdxMap.contains(e2Hash)) {invalidMentionHashCount += 1}
      if (!mentionFeatureIdxMap.contains(e1Features)) {invalidMentionFeatureCount += 1}
      if (!mentionFeatureIdxMap.contains(e2Features)) {invalidMentionFeatureCount += 1}

    }
    println(fullPredLabelsListToSave)
    println(s"invalid mention hash count ${invalidMentionHashCount}, invalid mention feature count:${invalidMentionFeatureCount}")
    scala.io.StdIn.readLine("-"*40)


    // Tuple to match: paper id, sentence id, text span. label
    // TODO, print the mention's hash, see if new mentions are predicted (not new event pairs)
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