package org.clulab.reach

import org.clulab.reach.TestUtils._
import org.scalatest.{FlatSpec, Matchers}
import org.clulab.reach.mentions.BioEventMention

class Zhengzhong_MaskTestMihai extends FlatSpec with Matchers {

  def showMask(sentence: String): Unit = {

    val mentions = getBioMentions(sentence)
    info(s"Num mentions: ${mentions.size}")

    println("=====================")
    val classifierTemp = new org.clulab.polarity.ml.DeepLearningPolarityClassifier()
    println(mentions(0).sentenceObj.words.toSeq)
    val bioEventM = mentions filter {
      case em: BioEventMention => true
      case _ => false
    }
    for (bioM <- bioEventM) {
      if (bioM.isInstanceOf[BioEventMention]) {
        val masked_lemmas = classifierTemp.maskEvent(bioM.sentenceObj.words.clone(), bioM.asInstanceOf[BioEventMention], "tag")
        println("\t", masked_lemmas.toSeq)

        if (bioM.arguments.contains("controller")) {
          println("\t\tcontroller: ", bioM.arguments("controller").head.text, "\ttype:", bioM.arguments("controller").head.getClass.getName, bioM.arguments("controller").head.start, bioM.arguments("controller").head.end)
        } else {
          println("\t\tNo controller")
        }
        if (bioM.arguments.contains("controlled")) {
          println("\t\tcontrolled: ", bioM.arguments("controlled").head.text, "\ttype:", bioM.arguments("controlled").head.getClass.getName, bioM.arguments("controlled").head.start, bioM.arguments("controlled").head.end)
        } else {
          println("\t\tNo controlled")
        }
      }
    }
  }
  val senList = List(
    "ASPP1 phosphorylates ASPP2",
    "ASPP1 promotes the phosphorylation of ASPP2",
    "The ubiquitination of ASPP1 promotes the phosphorylation of ASPP2",
    "The ubiquitination of ASPP1 promotes the phosphorylation of ASPP2 by ASPP1"
  )

  for (sen <- senList){
    showMask(sen)
  }
}

