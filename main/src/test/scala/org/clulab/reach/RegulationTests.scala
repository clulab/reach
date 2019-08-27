package org.clulab.reach

import org.scalatest.{Matchers, FlatSpec}
import org.clulab.reach.mentions._
import TestUtils._
import scala.io.Source



class RegulationTests extends FlatSpec with Matchers{


  // methods for checking for correct regulation triggers
  def getKDRegulation(mention:BioMention) = mention.modifications filter {
    case mod:KDtrigger => true
    case _ => false
  }
  def getKORegulation(mention:BioMention) = mention.modifications filter {
    case mod:KOtrigger => true
    case _ => false
  }
  def getDNRegulation(mention:BioMention) = mention.modifications filter {
    case mod:DNtrigger => true
    case _ => false
  }
  def getOERegulation(mention:BioMention) = mention.modifications filter {
    case mod:OEtrigger => true
    case _ => false
  }
  def getCHEMRegulation(mention:BioMention) = mention.modifications filter {
    case mod:CHEMtrigger => true
    case _ => false
  }


  // load tsv file from resources
  val file = Source.fromURL(getClass.getResource("/tsv/expt_stmts.tsv")).mkString
  val lines = file.split("\n")

  for (line <- lines.tail) {
    var index = lines.indexOf(line).toString()

    var splitLine = line.split("\t")

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



    // test for general regulation
    index+":\t"+sentence should "contain a mention with a " + regulationType + " modification" in {
      var mentions = getBioMentions(sentence).filter(_ matches "Event")
      var reg = mentions filter (_ matches regulationPolarity)
      regulationType match {
        case "knockdown" => getKDRegulation(reg.head) should be ('nonEmpty)
        case "knockout" => getKORegulation(reg.head) should be ('nonEmpty)
        case "dominant negative" => getDNRegulation(reg.head) should be ('nonEmpty)
        case "overexpression" => getOERegulation(reg.head) should be ('nonEmpty)
        case "chemical inhibition" => getCHEMRegulation(reg.head) should be ('nonEmpty)
        case _ => println("NONE")
      }
    }


    // test for controller AND controlled
//    index+":\t"+sentence should "contain the right controller AND controlled" in {
//      var mentions = getBioMentions(sentence).filter(_ matches "Event")
//      var reg = mentions.find(_.label == regulationPolarity)
//      reg.get.arguments("controller").head.text.contains(controller) should be(true)
//      reg.get.arguments("controlled").head.text.contains(controlled) should be(true)
//      println(index)
//      println("IDEAL CONTROLLER:\t"+controller)
//      println("REAL CONTROLLER:\t"+reg.get.arguments("controller").head.text)
//      println("IDEAL CONTROLLED:\t"+controlled)
//      println("REAL CONTROLLED:\t"+reg.get.arguments("controlled").head.text)
//      println(reg.get.arguments("controller").head.text.contains(controller))
    }


    // test for controlled
//    index+":\t"+sentence should "contain the right controlled" in {
//      var mentions = getBioMentions(sentence).filter(_ matches "Event")
//      var reg = mentions.find(_.label == regulationPolarity)
//      reg.get.arguments("controlled").head.text.contains(controlled) should be(true)
////      println("IDEAL CONTROLLED:\t"+controlled)
////      println("REAL CONTROLLED:\t"+reg.get.arguments("controlled").head.text)
//    }

  }





//  val sen1 = "Specifically, AQP4 siRNA interference was able to decrease AQP4 protein expression in the CBT area after 48 h when pathological edema was not apparent."
//
//  sen1 should "contain a mention with a knockdown modification" in {
//    val mentions = getBioMentions(sen1).filter(_ matches "Event")
//    val negreg = mentions filter (_ matches "Negative_regulation")
//    getKDRegulation(negreg.head) should have size (1)
//  }
//
//  sen1 should "contain the right controller" in {
//    val mentions = getBioMentions(sen1).filter(_ matches "Event")
//    val reg = mentions.find(_.label == "Negative_regulation")
//    reg.get.arguments("controller").head.text.contains("AQP4") should be (true)
//  }
//
//  sen1 should "contain the right controlled" in {
//    val mentions = getBioMentions(sen1).filter(_ matches "Event")
//    val reg = mentions.find(_.label == "Negative_regulation")
//    reg.get.arguments("controlled").head.text.contains("AQP4") should be (true)
//  }

}
