package org.clulab.reach

import org.clulab.polarity.ml.ManualCheckModel.lstmClassifier
import org.clulab.reach.TestUtils._
import org.clulab.reach.mentions.BioEventMention
import org.scalatest.{FlatSpec, Matchers}

class Zhengzhong_MaskTestMihai extends FlatSpec with Matchers {

  val classifierTemp = new org.clulab.polarity.ml.DeepLearningPolarityClassifier()

  def showMask(sentence: String): Unit = {

    val mentions = getBioMentions(sentence)
    info(s"Num mentions: ${mentions.size}")

    println("=====================")
    println(mentions(0).sentenceObj.words.toSeq)
    val bioEventM = mentions filter {
      case em: BioEventMention => true
      case _ => false
    }
    for (bioM <- bioEventM) {
        println("\t", bioM.sentenceObj.words.slice(bioM.start, bioM.end).toSeq)
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
  val senList = List(
    "ASPP1 phosphorylates ASPP2.",
    "ASPP1 activates ASPP2.",
    "The ubiquitinated Ras protein phosphorylates AKT.",
    "ASPP1 promotes the phosphorylation of ASPP2.",
    "The ubiquitination of ASPP1 promotes the phosphorylation of ASPP2.",
    "The ubiquitination of ASPP1 promotes the phosphorylation of ASPP2 by ASPP1."
  )

  val senList2 = List(
    "EGFR silencing deactivates MAPK1",
    "EGFR deletion deactivates MAPK1",
    "The TSC2 and TSC1 tuberous sclerosis complex, acting downstream of AKT, negatively regulates mTORC1 by inhibiting the GTPase activity of Rheb (Ras Homolog Enriched in Brain), which is a positive regulator of mTORC1.",
    "In addition, Knockdown of CDK5 induced growth inhibition and knockdown of TP53 reduced silencing CDK5 mediated growth inhibition in the presence or absence of paclitaxel (XREF_FIG).",
    "IL-6 knockdown impaired the function of ASPP2"
  )

  for (sen <- senList2){
    showMask(sen)
  }


  val senList3 = List(
    "__controller__ silencing deactivates __controlled__",
    "__controller__ deletion deactivates __controlled__",
    "Knockdown of __controller__ induced growth inhibition and knockdown of __controlled__"
  )

  val polarityRule3 = List(0,0,1)

  for (index <- senList3.indices){
    lstmClassifier.predictManual(senList3(index), polarityRule3(index))
  }

}

