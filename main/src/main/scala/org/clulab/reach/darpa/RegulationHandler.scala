package org.clulab.reach.darpa

import java.io

import org.clulab.odin._
import org.clulab.reach.mentions._
import org.clulab.struct.Interval

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}


object RegulationHandler {

  /** Keywords for each regulation type */
  val keywordKD: Seq[String] = Seq("sirna", "silencing", "silenced", "si-", "sh-", "shrna", "knockdown", "knock-down")//Seq("sirna", "silencing", "si-", "sh-", "shrna", "knockdown", "knock-down")
  val keywordKO: Seq[String] = Seq("knockout", "ko", "-/-")
  val keywordDN: Seq[io.Serializable] = Seq("dn-", "dominant-negative", ("dominant", "negative")) // for dependencies
  val keywordDNuni: Seq[String] = Seq("dn-", "dominant-negative") // for unigram tokens
  val keywordDNmulti: Seq[(String, String)] = Seq(("dominant", "negative")) // for bigram tokens
  val keywordOE: Seq[String] = Seq("overexpress", "oe")//Seq("overexpress", "overexpression", "oe")
  val keywordCHEM: Seq[Product] = Seq(("chemical", "inhibition", "of"), ("inhibitor", "of"))


  val keywordIdf = getRegulationKeywordsIdf
  println(keywordIdf)
  scala.io.StdIn.readLine()


  def detectRegulationsLinguistic(mentions: Seq[Mention], state:State): Seq[Mention] = {
    // do something very smart to handle triggers
    // and then return the mentions

    // Iterate over the BioEventMentions
    mentions foreach {
        case event:BioEventMention =>

          val dependencies = event.sentenceObj.dependencies //all deps for the sentence
          val eventTokInterval = event.tokenInterval //token interval for the event
          //find the indices of words in the sentence that are in the event span
          for (wordIdx <- event.sentenceObj.words.indices if wordIdx >= eventTokInterval.start & wordIdx < eventTokInterval.end) {
            //based on the word index for the words inside the event span, get all outgoing deps for this word
            val allOutgoingFromWord = event.sentenceObj.dependencies.get.outgoingEdges(wordIdx)
            //for each outgoing relation
            for (outgoing <- allOutgoingFromWord) {
              //if the node at the end of the outgoing edge is one of the triggers
              if (keywordKD contains event.sentenceObj.words(outgoing._1).toLowerCase) {
                //add the KD modification
                event.modifications += KDtrigger(new BioTextBoundMention(
                  Seq("KDtrigger_trigger"),
                  Interval(outgoing._1), //index of the relevant newly-discovered node?
                  sentence = event.sentence,
                  document = event.document,
                  keep = event.keep,
                  foundBy = event.foundBy
                ))
              }
              else if (keywordKO contains event.sentenceObj.words(outgoing._1).toLowerCase) {
                //add the KO modification
                event.modifications += KOtrigger(new BioTextBoundMention(
                  Seq("KOtrigger_trigger"),
                  Interval(outgoing._1), //index of the relevant newly-discovered node?
                  sentence = event.sentence,
                  document = event.document,
                  keep = event.keep,
                  foundBy = event.foundBy
                ))
              }
              else if (keywordDN contains event.sentenceObj.words(outgoing._1).toLowerCase) {
                //add the DN modification
                event.modifications += DNtrigger(new BioTextBoundMention(
                  Seq("DNtrigger_trigger"),
                  Interval(outgoing._1), //index of the relevant newly-discovered node?
                  sentence = event.sentence,
                  document = event.document,
                  keep = event.keep,
                  foundBy = event.foundBy
                ))
              }
              else if (keywordOE contains event.sentenceObj.words(outgoing._1).toLowerCase) {
                //add the OE modification
                event.modifications += OEtrigger(new BioTextBoundMention(
                  Seq("OEtrigger_trigger"),
                  Interval(outgoing._1), //index of the relevant newly-discovered node?
                  sentence = event.sentence,
                  document = event.document,
                  keep = event.keep,
                  foundBy = event.foundBy
                ))
              }
              else if (keywordCHEM contains event.sentenceObj.words(outgoing._1).toLowerCase) {
                //add the CHEM modification
                event.modifications += CHEMtrigger(new BioTextBoundMention(
                  Seq("CHEMtrigger_trigger"),
                  Interval(outgoing._1), //index of the relevant newly-discovered node?
                  sentence = event.sentence,
                  document = event.document,
                  keep = event.keep,
                  foundBy = event.foundBy
                ))
              }
            }
          }

          ///////////////////////////////////////////////////

          ///////////////////////////////////////////////////
          // Check for the presence of some regulation keywords
          // in all the sentence except the tokens

          // First, extract the trigger's range from the mention
          val interval = event.trigger.tokenInterval

          //val pairs = for (lemma <- event.lemmas) yield (1, lemma)
          val pairs = event.tokenInterval zip event.lemmas.get

          val pairsL = pairs takeWhile (_._1 < interval.start)
          val pairsR = pairs dropWhile (_._1 <= interval.end)

          // Get the evidence for the existing regulations to avoid duplicates
          val evidence:Set[Int] = event.modifications flatMap {
                  case mod:KDtrigger => mod.evidence.tokenInterval
                  case mod:KOtrigger => mod.evidence.tokenInterval
                  case mod:DNtrigger => mod.evidence.tokenInterval
                  case mod:OEtrigger => mod.evidence.tokenInterval
                  case mod:CHEMtrigger => mod.evidence.tokenInterval
                  case _ => Nil
              }


          /** Check for single-token triggers */

          // knockdown triggers
          for{
            (ix, lemma) <- pairsL ++ pairsR
            if (keywordKD contains lemma) && !(evidence contains ix)
          }{
              event.modifications += KDtrigger(new BioTextBoundMention(
                Seq("KDtrigger_trigger"),
                Interval(ix),
                sentence = event.sentence,
                document = event.document,
                keep = event.keep,
                foundBy = event.foundBy
              ))
            }

          // knockout triggers
          for{
            (ix, lemma) <- pairsL ++ pairsR
            if (keywordKO contains lemma) && !(evidence contains ix)
          }{
            event.modifications += KOtrigger(new BioTextBoundMention(
              Seq("KOtrigger_trigger"),
              Interval(ix),
              sentence = event.sentence,
              document = event.document,
              keep = event.keep,
              foundBy = event.foundBy
            ))
          }

          // dominant negative triggers
          // not sure about 'dominant-negative' -> a trigram?
          for{
            (ix, lemma) <- pairsL ++ pairsR
            if (keywordDNuni contains lemma) && !(evidence contains ix)
          }{
            event.modifications += DNtrigger(new BioTextBoundMention(
              Seq("DNtrigger_trigger"),
              Interval(ix),
              sentence = event.sentence,
              document = event.document,
              keep = event.keep,
              foundBy = event.foundBy
            ))
          }

          // overexpression triggers
          for{
            (ix, lemma) <- pairsL ++ pairsR
            if (keywordOE contains lemma) && !(evidence contains ix)
          }{
            event.modifications += OEtrigger(new BioTextBoundMention(
              Seq("OEtrigger_trigger"),
              Interval(ix),
              sentence = event.sentence,
              document = event.document,
              keep = event.keep,
              foundBy = event.foundBy
            ))
          }

          def flattenTuples(left:(Int, String), right:(Int, String)) = {
            (
              (left._1, right._1),
              (left._2, right._2)
            )
          }

          /** bi/trigram triggers */

          // dominant negative bigrams
          val dnVerbs = keywordDNmulti
          // Introduce bigrams for two-token keywords in both sides of the trigger
          for(side <- Seq(pairsL, pairsR)){
            val bigrams = (side zip side.slice(1, side.length)) map (x =>
              flattenTuples(x._1, x._2)
            )

            for{
              (interval, bigram) <- bigrams
              if (dnVerbs contains bigram) && (evidence intersect (interval._1 to interval._2+1).toSet).isEmpty
            }
              {
                event.modifications += DNtrigger(new BioTextBoundMention(
                Seq("DNtrigger_trigger"),
                Interval(interval._1, interval._2 + 1),
                sentence = event.sentence,
                document = event.document,
                keep = event.keep,
                foundBy = event.foundBy
              ))}

          }

          // chemical inhibition bigrams
          // 'chemical inhibition of' trigram?
          val chemVerbs = keywordCHEM
          // Introduce bigrams for two-token keywords in both sides of the trigger
          for(side <- Seq(pairsL, pairsR)){
            val bigrams = (side zip side.slice(1, side.length)) map (x =>
              flattenTuples(x._1, x._2)
              )

            for{
              (interval, bigram) <- bigrams
              if (chemVerbs contains bigram) && (evidence intersect (interval._1 to interval._2+1).toSet).isEmpty
            }
            {
              event.modifications += CHEMtrigger(new BioTextBoundMention(
                Seq("CHEMtrigger_trigger"),
                Interval(interval._1, interval._2 + 1),
                sentence = event.sentence,
                document = event.document,
                keep = event.keep,
                foundBy = event.foundBy
              ))}

          }
          ///////////////////////////////////////////////////
        case _ => ()
    }

    mentions
  }

  //
  // this is leftover from NegationHandler; not sure if we need anything here for Regulations
  //

  // Alter Regulation modifications in-place
//  def handleRegulations(ms: Seq[BioMention]): Unit = {
//    ms foreach { m =>
//      val (negMods, other) = m.modifications.partition(_.isInstanceOf[Negation])
//      val negationModifications = negMods.map(_.asInstanceOf[Negation])
//
//      // count the negations
//      negationModifications match {
//        // 0 or 1 Neg modifications means no change...
//        case noChange if noChange.size <= 1 => ()
//        // if we have an even number of Negations, remove them all
//        case pos if pos.size % 2 == 0 =>
//          m.modifications = other
//        // if we have an odd number, report only the first...
//        case neg if neg.size % 2 != 0 =>
//          val singleNeg =
//            negationModifications
//              .toSeq
//              .sortBy(_.evidence.tokenInterval)
//              .head
//          m.modifications = other + singleNeg
//      }
//    }
//  }

  def detectRegulationsBaseline(mentions: Seq[Mention]): Seq[Mention] = {
    // do something very smart to handle triggers. Use the whole sentence to determine the type of regulation
    // Count the number of occurrences of keywords. The most occurrences indicates the type.
    // and then return the mentions

    // Iterate over the BioEventMentions
    mentions foreach {
      case event:BioEventMention =>
      val lemmas_raw = event.sentenceObj.words
      val lemmas = lemmas_raw.map(_.toLowerCase())
      val trigger_start = event.trigger.start
      val trigger_end = event.trigger.end

      var kdCount = 0
      var koCount = 0
      var dnCount = 0
      var oeCount = 0
      var chemCount = 0
      val kdDisList = ArrayBuffer[Int](lemmas.length)
      val koDisList = ArrayBuffer[Int](lemmas.length)
      val dnDisList = ArrayBuffer[Int](lemmas.length)
      val oeDisList = ArrayBuffer[Int](lemmas.length)
      val chemDisList = ArrayBuffer[Int](lemmas.length)
      // First detect regulation type with 1 keyword
      for ((lemma, lemma_index) <- lemmas.zipWithIndex) {
        for (keyword <- keywordKD if lemma.contains(keyword)){
          kdCount += 1
          val dist = if (trigger_start>lemma_index) trigger_start-lemma_index else if (lemma_index>=trigger_end) lemma_index-trigger_end+1 else 0
          kdDisList += dist
        }
        for (keyword <- keywordKO if lemma.contains(keyword)){
          koCount += 1
          val dist = if (trigger_start>lemma_index) trigger_start-lemma_index else if (lemma_index>=trigger_end) lemma_index-trigger_end+1 else 0
          koDisList +=dist
        }
        for (keyword <- keywordDN if lemma.contains(keyword)){
          dnCount += 1
          val dist = if (trigger_start>lemma_index) trigger_start-lemma_index else if (lemma_index>=trigger_end) lemma_index-trigger_end+1 else 0
          dnDisList +=dist
        }
        for (keyword <- keywordOE if lemma.contains(keyword)) {
          oeCount += 1
          val dist = if (trigger_start>lemma_index) trigger_start-lemma_index else if (lemma_index>=trigger_end) lemma_index-trigger_end+1 else 0
          oeDisList +=dist
        }
      }

      // Then detect regulation type with a sequence of keywords
      // Hard code these for now. TODO: code these in more general way
      val dnCountTuple1 =countSubSeqMatch(lemmas, List("dominant", "negative"), trigger_start, trigger_end)
      dnCount+=dnCountTuple1._1
      dnDisList++=dnCountTuple1._2
      val chemCountTuple1 =countSubSeqMatch(lemmas, List("chemical", "inhibition", "of"), trigger_start, trigger_end)
      chemCount+=chemCountTuple1._1
      chemDisList++=chemCountTuple1._2
      val chemCountTuple2 =countSubSeqMatch(lemmas, List("inhibitor", "of"), trigger_start, trigger_end)
      chemCount+=chemCountTuple2._1
      chemDisList++=chemCountTuple2._2

      val regTokenCounts = Map("KD"-> kdCount, "KO"-> koCount,"DN"-> dnCount,"OE"-> oeCount,"CHEM"-> chemCount)
      val regMinDis = Map("KD"-> kdDisList.min, "KO"-> koDisList.min,"DN"-> dnDisList.min,"OE"-> oeDisList.min,"CHEM"-> chemDisList.min)

      val maxKWCount = regTokenCounts.maxBy { case (key, value) => value }  // get the pair with the largest count
      val mostPossibleRegByCount = regTokenCounts.filter(e =>e._2== maxKWCount._2)  // get all pairs with the largest count

      // get regulation type by count and distance
      val regType = if (mostPossibleRegByCount.size==1){
        mostPossibleRegByCount.keys.head
      }
      else {
        if (mostPossibleRegByCount.values.head==0){
          "None"
        }
        else{
          val mostPossibleRegByDist = regMinDis.filter(e => mostPossibleRegByCount.keysIterator.contains(e._1))
          val mostPossibleReg = mostPossibleRegByDist.minBy { case (key, value) => value }
          mostPossibleReg._1
        }
      }


      if (regType=="KD"){
        event.modifications += KDtrigger(new BioTextBoundMention(
          Seq("KDtrigger_trigger"),
          Interval(0), //index of the relevant newly-discovered node?
          sentence = event.sentence,
          document = event.document,
          keep = event.keep,
          foundBy = event.foundBy
        ))
      }
      else if (regType=="KO"){
        event.modifications += KOtrigger(new BioTextBoundMention(
          Seq("KOtrigger_trigger"),
          Interval(0), //index of the relevant newly-discovered node?
          sentence = event.sentence,
          document = event.document,
          keep = event.keep,
          foundBy = event.foundBy
        ))
      }
      else if (regType=="DN"){
        event.modifications += DNtrigger(new BioTextBoundMention(
          Seq("DNtrigger_trigger"),
          Interval(0), //index of the relevant newly-discovered node?
          sentence = event.sentence,
          document = event.document,
          keep = event.keep,
          foundBy = event.foundBy
        ))
      }
      else if (regType=="OE"){
        event.modifications += OEtrigger(new BioTextBoundMention(
          Seq("OEtrigger_trigger"),
          Interval(0),
          sentence = event.sentence,
          document = event.document,
          keep = event.keep,
          foundBy = event.foundBy
        ))
      }
      else if (regType=="CHEM"){
        event.modifications += CHEMtrigger(new BioTextBoundMention(
          Seq("CHEMtrigger_trigger"),
          Interval(0),
          sentence = event.sentence,
          document = event.document,
          keep = event.keep,
          foundBy = event.foundBy
        ))
      }

//        get regulation type by distance only
//
//      val mostPossibleReg = regMinDis.minBy { case (key, value) => value }
//      val regType = if (mostPossibleReg._2==lemmas.length){
//        "None"
//      }
//      else {mostPossibleReg._1}


//      println(regType)
//      regType
      case _ => ()
    }
    mentions

  }

  def countSubSeqMatch(lemmas:Seq[String], keywords:Seq[String], trigger_start:Int, trigger_end:Int):(Int, ArrayBuffer[Int]) = {
    var count = 0
    val posList = ArrayBuffer[Int]()
    val kw_span = keywords.length
    for (index <- 0 until lemmas.length - kw_span) {
      if (lemmas.slice(index, index + keywords.length) == keywords) {
        count += 1
        val dist = if (trigger_start > index + kw_span) trigger_start - (index + kw_span) else if (index >= trigger_end) index - trigger_end + 1 else 0
        posList += dist
      }
    }
    (count, posList)
  }


  def getRegulationKeywordsIdf():scala.collection.mutable.Map[String, Float] = {
    val relFile: BufferedSource = Source.fromURL(getClass.getResource("/tsv/regulations/relevantSentencesLinguistic.tsv"))
    val file: String = relFile.mkString
    val lines: Array[String] = file.split("\n")

    // The list stores all of the keywords.
    val keywordsAll = (keywordKD++keywordKO++keywordDNuni++keywordOE).to[mutable.ArrayBuffer]
    keywordsAll += "dominant negative"
    keywordsAll += "chemical inhibition of"
    keywordsAll += "inhibitor of"

    //Initialize the document frequency map for the keywords
    val keywordsDF = mutable.Map[String, Int]()
    for (keyword <- keywordsAll){
      keywordsDF(keyword)=0
    }
    for ((line, lineNum) <- lines.zipWithIndex) {
      val index = lines.indexOf(line).toString

      val splitLine = line.split("\t")
      val sentence = splitLine(0).toLowerCase()
      val lemmas = sentence.split(" ")

      // Initilize a map of flags of keywords. The DF score for a keyword can only be added by 1 in a document.
      val keywordsFlag = mutable.Map[String, Int]()
      for (keyword <- keywordsAll){
        keywordsFlag(keyword)=0
      }

      // get keyword document frequency for one-word keywords
      for ((lemma, lemma_index) <- lemmas.zipWithIndex) {
        for (keyword <- keywordKD++keywordKO++keywordDNuni++keywordOE if lemma.contains(keyword) & keywordsFlag(keyword)==0){
          keywordsDF(keyword)+=1
          keywordsFlag(keyword)+=1
        }
      }

      // get keyword document frequency for multi-word keywords
      keywordsDF("dominant negative") = if (countSubSeqMatch(lemmas, Seq("dominant", "negative"), 0, 0)._1>0){
        keywordsDF("dominant negative")+1
      } else {
        keywordsDF("dominant negative")
      }
      keywordsDF("chemical inhibition of") = if (countSubSeqMatch(lemmas, Seq("chemical", "inhibition", "of"), 0, 0)._1>0){
        keywordsDF("chemical inhibition of")+1
      } else {
        keywordsDF("chemical inhibition of")
      }
      keywordsDF("inhibitor of") = if (countSubSeqMatch(lemmas, Seq("inhibitor", "of"), 0, 0)._1>0){
        keywordsDF("inhibitor of")+1
      } else {
        keywordsDF("inhibitor of")
      }
    }


    println("keywords DF:", keywordsDF)
    // Compute the final IDF score for each keyword and return
    val keywordsIDF = scala.collection.mutable.Map[String, Float]()
    for (keyword <- keywordsAll){
      keywordsIDF(keyword) = Math.log(((lines.length+1).toFloat/(keywordsDF(keyword)+1)).toFloat).toFloat
    }
    keywordsIDF
  }
}


// TODO: this function is for debugging purpose only. Delete this when merging the code.
object reguTestZ extends App {
  val keywordCHEM: Seq[Product] = Seq(("chemical", "inhibition", "of"), ("inhibitor", "of"))
  println(keywordCHEM(1))


  val list1 = List(1,2,3,4,3,4,5,6,4,3,4,5,6,3,4)
  val list2 = List(3,4)

  var count = 0
  for (index <- 0 until (list1.length-list2.length)){
    println(list1.slice(index, index+list2.length), list2)
    if (list1.slice(index, index+list2.length)==list2){
      count +=1
    }
  }
  println(count)


  val list3 = List(4,5,6, list2)
  println(list3)

  println(list3.contains(3))
  println(list3.contains(4))


  val ele = list1.find(_==3)
  println(ele)


  val ab1 = ArrayBuffer[Int](1,2)
  println(ab1)


  list1 foreach {
    case number:Int =>
      println("====")
      println(number)
  }

  val mapA = scala.collection.mutable.Map("A"->0, "B"->1)

  mapA("A")=mapA("A")+1
  println(mapA)

  println(list1.contains(list2))

  println((list1++list2).to[ArrayBuffer])
  println(list1)




}
