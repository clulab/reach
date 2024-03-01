package org.clulab.reach.assembly

import org.clulab.reach.PaperReader
import org.clulab.reach.assembly.relations.corpus.EventPair
import org.clulab.reach.mentions.CorefMention
import org.clulab.reach.utils.MentionManager
import org.scalatest.{FlatSpec, Matchers}

class TestHash  extends FlatSpec with Matchers {
  val mentionManager = new MentionManager()
  val testReach = PaperReader.reachSystem
  val text = "Tbet Rag2 mice (Garrett et al., 2010) as well as Bacteroides spp. (Bloom et al., 2011), Helicobacter spp. (Fox et al., 2011), and Bilophila wadsworthia (Devkota et al., 2012) in Il10 have been shown to enhance intestinal inflammation.The acute dextran sulfate sodium"
  val allMentions = testReach.extractFrom(text, "serialization-test", "1", None)
  val sortedMentions = allMentions.sortBy { mention => (mention.startOffset, mention.endOffset) }

  val assemblyManager = AssemblyManager()

  behavior of "Hash"

  it should "compute the expected value for an EventPair" in {
    val expectedHash = 316669350
    val corefMentions = sortedMentions.collect { case corefMention: CorefMention => corefMention }
    val e1 = corefMentions.head
    val e2 = corefMentions.last
    val eventPair = new EventPair(e1, e2, "relation", 42d, "annotatorID", None)
    val actualHash = eventPair.equivalenceHash

    actualHash should be (expectedHash)
  }
}
