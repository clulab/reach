package org.clulab.reach.context

import org.clulab.reach.TestUtils.getBioMentions
import org.clulab.reach.mentions.BioEventMention
import org.scalatest.{FlatSpec, Matchers}


class TestContextMetaData extends FlatSpec with Matchers{

    private val paragraph =
      """
        |This is a sentence where I mention human and yeast.\
        |In the second sentence I mention human again and liver.\
        |The third sentence doesn't mention any context.\
        |Finally, we will state an event, for example: Mek phosphorylates RAS.\
        |""".stripMargin

    paragraph should "contain metadata for human, yeast and liver" in {
      val mentions = getBioMentions(paragraph)

      val events:Seq[BioEventMention] = mentions collect { case m:BioEventMention => m }


      events should have size 1

      val event = events.head
      val context = event.context.get
      val metaData = event.contextMetaData.get

      // It should contain two species assignments and one Organ assignment
      val speciesContext = context("Species")
      speciesContext should have size 2
      speciesContext should contain ("taxonomy:9606")
      speciesContext should contain ("taxonomy:4932")

      val organContext = context("Organ")
      organContext should have size 1
      organContext should contain ("uberon:UBERON:0002107")

      // Now test the metadata counts
      metaData(("Species", "taxonomy:9606")) should equal (Map(2 -> 1, 3 ->1))
      metaData(("Species", "taxonomy:4932")) should equal (Map(3 ->1))
      metaData(("Organ","uberon:UBERON:0002107")) should equal (Map(2 -> 1))

    }
}
