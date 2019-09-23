package org.clulab.reach

import java.io.PrintWriter

import org.scalatest.{FlatSpec, Matchers}
import org.clulab.reach.mentions._
import TestUtils._

import scala.io.{BufferedSource, Source}



class RegulationTests extends FlatSpec with Matchers{

  val outFilenameIrrelevant = "irrelevantSentences.tsv"
  val pwIrr = new PrintWriter(outFilenameIrrelevant)

  val outFilenameRelevant = "relevantSentences.tsv"
  val pwRel = new PrintWriter(outFilenameRelevant)

  val outFilenameFail = "failingSentences.tsv"
  val pwFail = new PrintWriter(outFilenameFail)

  val outFilenamePass = "passingSentences.tsv"
  val pwPass = new PrintWriter(outFilenamePass)


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


  // load tsv file from resources
  val originalFile: BufferedSource = Source.fromURL(getClass.getResource("/tsv/expt_stmts.tsv"))//.mkString
//  val relFile: BufferedSource = Source.fromURL(getClass.getResource("relevantSentences.tsv"))
  val file: String = originalFile.mkString
  val lines: Array[String] = file.split("\n")

  for (line <- lines.tail) {
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
//    println("REGULATION TYPE:\t"+regulationType)

    // get the polarity of the regulation
    var regulationPolarity = splitLine(1).split("\\(")(0)
    regulationPolarity match {
      case "DecreaseAmount" => regulationPolarity = "Negative_regulation"
      case "IncreaseAmount" => regulationPolarity = "Positive_regulation"
      case _ => regulationPolarity = "NONE"
    }
//    println("POLARITY:\t"+regulationPolarity)

    // get the sentence text
    var sentence = splitLine(6)

    // get the controller of the regulation
    var controller = splitLine(3)
//    println("CONTROLLER:\t"+controller)

    // get the controlled of the regulation
    var controlled = splitLine(5)
//    println("CONTROLLED:\t"+controlled)

    val mentions = getBioMentions(sentence).filter(_ matches "Event")
    val reg = mentions.find(_.label == regulationPolarity)
    //todo: What is happening here? why does it work in the test but not here where I want it?!
    val realController = if (reg.get.arguments("controller").nonEmpty) reg.get.arguments("controller").head.text else "None"
    val realControlled = "None"
    println(index)
//    println("IDEAL CONTROLLER:\t"+controller)
//    println("REAL CONTROLLER:\t"+realController)
//    println("IDEAL CONTROLLED:\t"+controlled)
//    println("REAL CONTROLLED:\t"+realControlled)
    println(realController.contains(controller) && realControlled.contains(controller))


    // test for regulation modifications ONLY
    // use number of passing tests here as numerator
//    index+":\t"+sentence should "contain a mention with a " + regulationType + " modification" in {
//      val mentions = getBioMentions(sentence).filter(_ matches "Event")
//      val reg = mentions filter (_ matches regulationPolarity)
//      regulationType match {
//        case "knockdown" => getKDRegulation(reg.head) should be('nonEmpty)
//        case "knockout" => getKORegulation(reg.head) should be('nonEmpty)
//        case "dominant negative" => getDNRegulation(reg.head) should be('nonEmpty)
//        case "overexpression" => getOERegulation(reg.head) should be('nonEmpty)
//        case "chemical inhibition" => getCHEMRegulation(reg.head) should be('nonEmpty)
//        case _ => println("NONE")
//      }
//    }



//    // test for controller and controlled ONLY
//    // use number of passing tests here as denominator
//    index+":\t"+sentence should "contain the right controller AND controlled" in {
//      val mentions = getBioMentions(sentence).filter(_ matches "Event")
//      val reg = mentions.find(_.label == regulationPolarity)
//      val extractedController = reg.get.arguments("controller").head.text//.contains(controller) should be(true)
//      val extractedControlled = reg.get.arguments("controlled").head.text//.contains(controlled) should be(true)
//      extractedController.contains(controller) should be(true)
//      extractedControlled.contains(controlled) should be(true)
////      pwRel.println(line)
////          println(index)
////          println("IDEAL CONTROLLER:\t"+controller)
////          println("REAL CONTROLLER:\t"+reg.get.arguments("controller").head.text)
////          println("IDEAL CONTROLLED:\t"+controlled)
////          println("REAL CONTROLLED:\t"+reg.get.arguments("controlled").head.text)
////          println(reg.get.arguments("controller").head.text.contains(controller))
//        }
    }
  pwRel.close()
  pwIrr.close()
}
