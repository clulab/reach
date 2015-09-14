package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.reach.mentions._
import TestUtils._

class HypothesisTests extends FlatSpec with Matchers{

    def getHyphoteses(mention:BioMention) = mention.modifications filter {
      case mod:Hypothesis => true
      case _ => false
    }

    val sen1 = "RAS suggests the phosphorylation of MEK"

    sen1 should "contain a mention with a hyphothesis modification" in {
      val mentions = parseSentence(sen1).filter(_ matches "Event")

      val phospho = mentions filter (_ matches "Phosphorylation")
      getHyphoteses(phospho.head) should have size (1)
    }

    val sen2 = "After extensive experimentation, the process hints that RAS phosphorylates MEK"

    sen2 should "contain a mention with a hyphothesis modification" in {
      val mentions = parseSentence(sen2).filter(_ matches "Event")

      val phospho = mentions filter (_ matches "Phosphorylation")
      getHyphoteses(phospho.head) should have size (1)
    }

    val sen3 = "Now, we hypothesize that RAS phosphorylates MEK"

    sen3 should "contain a mention with a hyphothesis modification" in {
      val mentions = parseSentence(sen3).filter(_ matches "Event")

      val phospho = mentions filter (_ matches "Phosphorylation")
      getHyphoteses(phospho.head) should have size (1)
    }

    val sen4 = "The presence of p53 indicates the phosphorylation of MEK"

    sen4 should "contain a mention with a hyphothesis modification" in {
      val mentions = parseSentence(sen3).filter(_ matches "Event")

      val phospho = mentions filter (_ matches "Phosphorylation")
      getHyphoteses(phospho.head) should have size (1)
    }

}
