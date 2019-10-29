package org.clulab.reach

import org.clulab.reach.darpa.RegulationHandler.regulationClassifierBaseline
import org.clulab.reach.mentions._
import org.scalatest.{FlatSpec, Matchers}

import scala.io.{BufferedSource, Source}



class RegulationTests extends FlatSpec with Matchers{

  // split sentences into ir/relevant and passing/failing
//  val outFilenameIrrelevant = "irrelevantSentences.tsv"
//  val pwIrr = new PrintWriter(outFilenameIrrelevant)
//
//  val outFilenameRelevant = "relevantSentences.tsv"
//  val pwRel = new PrintWriter(outFilenameRelevant)
//
//  val outFilenameFail = "failingSentences.tsv"
//  val pwFail = new PrintWriter(outFilenameFail)
//
//  val outFilenamePass = "passingSentences.tsv"
//  val pwPass = new PrintWriter(outFilenamePass)


  // methods for checking for correct regulation triggers
  def getKDRegulation(mention:BioMention): Set[Modification] = mention.modifications filter {
    case mod:KDtrigger => true
    case _ => false
  }
  def getKORegulation(mention:BioMention): Set[Modification] = mention.modifications filter {
    case mod:KOtrigger => true
    case _ => false
  }
  def getDNRegulation(mention:BioMention): Set[Modification] = mention.modifications filter {
    case mod:DNtrigger => true
    case _ => false
  }
  def getOERegulation(mention:BioMention): Set[Modification] = mention.modifications filter {
    case mod:OEtrigger => true
    case _ => false
  }
  def getCHEMRegulation(mention:BioMention): Set[Modification] = mention.modifications filter {
    case mod:CHEMtrigger => true
    case _ => false
  }


  // load tsv files from resources
  val passingFile: BufferedSource = Source.fromURL(getClass.getResource("/tsv/regulations/passingSentences.tsv"))
  val failingFile: BufferedSource = Source.fromURL(getClass.getResource("/tsv/regulations/failingSentences.tsv"))
  val originalFile: BufferedSource = Source.fromURL(getClass.getResource("/tsv/regulations/expt_stmts.tsv"))
  val relFile: BufferedSource = Source.fromURL(getClass.getResource("/tsv/regulations/relevantSentences.tsv"))
  val file: String = relFile.mkString
  val lines: Array[String] = file.split("\n")

  for (line <- lines) {
    val index = lines.indexOf(line).toString

    val splitLine = line.split("\t")

    // get the type of the regulation
    var regulationType = splitLine(0).toLowerCase
    regulationType match {
      case "sirna" | "silencing" | "si-" | "sh-" | "shrna" => regulationType = "knockdown"
      case "knockout" | "ko" | "-/-" => regulationType = "knockout"
      case "dn-" | "dominant-negative" | "dominant negative" => regulationType = "dominant negative"
      case "overexpress" | "overexpression" | "oe" => regulationType = "overexpression"
      case "chemical inhibition of" | "inhibitor of" => regulationType = "chemical inhibition"
      case _ => regulationType = "NONE"
    }

    // get the polarity of the regulation
    var regulationPolarity = splitLine(1).split("\\(")(0)
    regulationPolarity match {
      case "DecreaseAmount" => regulationPolarity = "Negative_regulation"
      case "IncreaseAmount" => regulationPolarity = "Positive_regulation"
      case _ => regulationPolarity = "NONE"
    }

    // get the sentence text
    val sentence = splitLine(6)

    // get the "controller" argument of the regulation
    var controller = splitLine(3)

    // get the "controlled" argument of the regulation
    var controlled = splitLine(5)


    /** used this to preprocess data to get relevant and irrelevant sentences */
//    val mentions = getBioMentions(sentence).filter(_ matches "Event")
//    val reg = mentions.find(_.label == regulationPolarity)
//
//    val allArguments = if (reg != None) reg.get.arguments else None
//
//    val realController = if (allArguments != None) reg.get.arguments("controller").head.text else None
//    val realControlled = if (allArguments != None) reg.get.arguments("controlled").head.text else None
//
//    println(index)
//    println("IDEAL CONTROLLER:\t"+controller)
//    println("REAL CONTROLLER:\t"+realController)
//    println("IDEAL CONTROLLED:\t"+controlled)
//    println("REAL CONTROLLED:\t"+realControlled)
//
//    if (realController.toString.contains(controller) && realControlled.toString.contains(controlled)){
//      pwRel.write(line)
//    }
//    else{
//      pwIrr.write(line)
//    }


//    /** test for regulation modifications */
//    index+":\t"+sentence should "contain a mention with a " + regulationType + " modification" in {
//      val mentions = getBioMentions(sentence).filter(_ matches "Event")
//      val reg = mentions.find(_.label == regulationPolarity)
//      regulationType match {
//        case "knockdown" => getKDRegulation(reg.head) should be('nonEmpty)
//        case "knockout" => getKORegulation(reg.head) should be('nonEmpty)
//        case "dominant negative" => getDNRegulation(reg.head) should be('nonEmpty)
//        case "overexpression" => getOERegulation(reg.head) should be('nonEmpty)
//        case "chemical inhibition" => getCHEMRegulation(reg.head) should be('nonEmpty)
//        case _ => println("NONE")
//      }
//    }

    // Baseline regulation classifier: classify events by the count of keywords
    index+":\t"+sentence should "contain a mention with a " + regulationType + " modification" in {
      val lemmas = sentence.split(" ")
      regulationType match {
        case "knockdown" => regulationClassifierBaseline(lemmas) should be("KD")
        case "knockout" => regulationClassifierBaseline(lemmas) should be("KO")
        case "dominant negative" => regulationClassifierBaseline(lemmas) should be("DN")
        case "overexpression" => regulationClassifierBaseline(lemmas) should be("OE")
        case "chemical inhibition" => regulationClassifierBaseline(lemmas) should be("CHEM")
        case _ => println("NONE")
      }
    }



    /** test for controller and controlled ONLY */
    /** don't need this now after preprocessing! */
//    // use number of passing tests here as denominator
//    index+":\t"+sentence should "contain the right controller AND controlled" in {
//      val mentions = getBioMentions(sentence).filter(_ matches "Event")
//      val reg = mentions.find(_.label == regulationPolarity)
//      val extractedController = reg.get.arguments("controller").head.text//.contains(controller) should be(true)
//      val extractedControlled = reg.get.arguments("controlled").head.text//.contains(controlled) should be(true)
//      extractedController.contains(controller) should be(true)
//      extractedControlled.contains(controlled) should be(true)
////          println(index)
////          println("IDEAL CONTROLLER:\t"+controller)
////          println("REAL CONTROLLER:\t"+reg.get.arguments("controller").head.text)
////          println("IDEAL CONTROLLED:\t"+controlled)
////          println("REAL CONTROLLED:\t"+reg.get.arguments("controlled").head.text)
////          println(reg.get.arguments("controller").head.text.contains(controller))
//        }

    }
}
