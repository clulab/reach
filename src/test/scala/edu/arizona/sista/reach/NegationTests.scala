package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.reach.mentions._
import TestUtils._

class NegationTests extends FlatSpec with Matchers{

    def getNegations(mention:BioMention) = mention.modifications filter {
      case mod:Negation => true
      case _ => false
    }


    val sen1 = "RAS does not phosphorylate MEK"

    sen1 should "contain a mention with a negation modification" in {
      val mentions = getBioMentions(sen1)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (1)

      val phospho = mentions filter (_ matches ("Phosphorylation"))
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)

    }

    val sen2 = "RAS doesn't phosphorylate MEK"

    sen2 should "contain a mention with a negation modification" in {
      val mentions = getBioMentions(sen2)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (1)

      val phospho = mentions filter (_ matches ("Phosphorylation"))
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)

    }

    val sen3 = "RAS is not phosphorylating MEK"

    sen3 should "contain a mention with a negation modification" in {
      val mentions = getBioMentions(sen3)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (1)

      val phospho = mentions filter (_ matches ("Phosphorylation"))
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)

    }

    val sen4 = "RAS isn't phosphorylating MEK"

    sen4 should "contain a mention with a negation modification" in {
      val mentions = getBioMentions(sen4)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (1)

      val phospho = mentions filter (_ matches ("Phosphorylation"))
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)

    }

    val sen5 = "RAS wasn't phosphorylated"

    sen5 should "contain a mention with a negation modification" in {
      val mentions = getBioMentions(sen5)

      mentions filter (_ matches "Event") should have size (1)

      val phospho = mentions filter (_ matches "Phosphorylation")
      phospho should have size (1)
      getNegations(phospho.head) should have size (1)
    }

    val sen6 = "RAS fails to phosphorylate MEK"

    sen6 should "contain a mention with a negation modification" in {
      val mentions = getBioMentions(sen6)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (1)

      val phospho = mentions filter (_ matches "Phosphorylation")
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)
    }

    val sen7 = "RAS fails phosphorylating MEK"

    sen7 should "contain a mention with a negation modification" in {
      val mentions = getBioMentions(sen7)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (1)

      val phospho = mentions filter (_ matches "Phosphorylation")
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)
    }

    val sen8 = "RAS plays no role in the phosphorylation of MEK"

    sen8 should "contain a mention with a negation modification" in {
      val mentions = getBioMentions(sen8)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (1)

      val phospho = mentions filter (_ matches "Phosphorylation")
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)
    }

    val sen9 = "RAS plays little role in the phosphorylation of MEK"

    sen9 should "contain a mention with a negation modification" in {
      val mentions = getBioMentions(sen9)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (1)

      val phospho = mentions filter (_ matches "Phosphorylation")
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)
    }

    // Negative tests

    val sen10 = "RAS phosphorylates MEK"

    sen10 should "not contain a mention with a negation modification" in {
      val mentions = getBioMentions(sen10)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (0)

      val phospho = mentions filter (_ matches "Phosphorylation")
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)
    }

    // Multiple negation tests
    val sen11 = "RAS doesn't fail to phosphorylate MEK"

    sen11 should "not contain any negation mentions" in {
      val mentions = getBioMentions(sen11)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (0)

      val phospho = mentions filter (_ matches "Phosphorylation")
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)
    }

    val sen12 = "RAS fails not to phosphorylate MEK"

    sen12 should "not contain any negation mentions" in {
      val mentions = getBioMentions(sen12)

      mentions filter (_ matches "Event") should have size (2)

      val pos_reg = mentions filter (_ matches ("Positive_regulation"))
      pos_reg should have size (1)
      getNegations(pos_reg.head) should have size (0)

      val phospho = mentions filter (_ matches "Phosphorylation")
      phospho should have size (1)
      getNegations(phospho.head) should have size (0)
    }
}
