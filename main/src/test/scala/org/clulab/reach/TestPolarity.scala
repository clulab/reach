package org.clulab.reach

import org.scalatest.{FlatSpec, Matchers}
import org.clulab.reach.mentions._
import TestUtils._
import com.typesafe.scalalogging.LazyLogging

class TestPolarity extends FlatSpec with Matchers{

  val sen1 = "Proteolysis means destruction, therefore this is actually a negative regulation of the amount of p27"
  sen1 should "contain a negative regulation" in {
    val mentions = getBioMentions(sen1).filter(_ matches "ComplexEvent")

    // TODO: Write a function to identify the specific event
    info(s"Num mentions: ${mentions.size}")
    // Test for theh display label
    // TODO perhaps use the "matches" method?
    mentions.map (_.displayLabel) should contain ("Negative_regulation")
  }

  val sen2 = """ABT-737, a small molecule BH3-only mimetic developed by Abbott Laboratories, specifically targets the antiapoptotic bcl-2 members Bcl-2, Bcl-xL and Bcl-w."""
  sen2 should "have an inhihbition" in {
    val mentions = getBioMentions(sen2).filter(_ matches "ComplexEvent")

    // TODO: Write a function to identify the specific event
    info(s"Num mentions: ${mentions.size}")
    // TODO make sure this labels is the correct for "inhibition"
    mentions map (_.displayLabel) should contain ("Negative_regulation")
  }

  val sen3 = """We suggest that this difference is because of reduced autocrine and paracrine CD40 activation by the lower level of CD40L expressed on plasmid transduced CLL cells and the shorter time - course of vaccine manufacture (5 h versus 3 days)."""
  sen3 should "have double negations, hence positive polarity" in {

    val mentions = getBioMentions(sen3).filter(_ matches "ComplexEvent")
    // TODO: Write a function to identify the specific event
    info(s"Num mentions: ${mentions.size}")


    mentions map (_.displayLabel) should contain ("Positive_regulation")
  }

  val sen4 = """These observations support the model that Sufu is the major negative regulator of FP induction and that activated Smo restricts the inhibitory function of Sufu through Kif7."""
  sen4 should "have a negative regulation due to \"restricts the ... function of\" AND positive polarity" in {
    val mentions = getBioMentions(sen4).filter(_ matches "ComplexEvent")
    // TODO: Write a function to identify the specific event
    info(s"Num mentions: ${mentions.size}")

    // TODO: Is this the correct label?
    mentions map (_.displayLabel) should contain ("Positive_regulation")
  }


  val sen5 = """Previous studies have shown that EGFR signaling mediated by the MEK and ERK and PI3K and AKT pathways is essential for RPE cell proliferation and survival [XREF_BIBR]."""
  sen5 should "ERK should be downstream of EGFR" in {
    val mentions = getBioMentions(sen5).filter(_ matches "ComplexEvent")

    val mention = mentions filter {
      m =>
        val controller = m.arguments("controller").head
        val controlled = m.arguments("controlled").head

        if(controller.text == "EGFR" && controlled.text == "ERK")
          true
        else
          false
    }

    mention should have size 1
  }

  val sen6 = """At the whole muscle level, the same two week HFD protocol did not result in significantly diminished insulin stimulated Akt activation 9."""
  sen6 should "Have positive polarity" in {
    val mentions = getBioMentions(sen5).filter(_ matches "ComplexEvent")

    // TODO: Write a function to identify the specific event
    mentions.head.displayLabel should startWith ("Positive")
  }

}
