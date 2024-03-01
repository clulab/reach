package org.clulab.reach

import org.clulab.reach.grounding.InMemoryKB.KBEntry
import org.clulab.reach.grounding.{InMemoryKB, KBResolution}
import org.clulab.reach.utils.MentionManager
import org.scalatest.{FlatSpec, Matchers}

class TestHash  extends FlatSpec with Matchers {
  val mentionManager = new MentionManager()
  val testReach = PaperReader.reachSystem
  val text1 = "Mek was not phosphorylized by AKT1"
  val text2 = "Mouse AKT2 phosphorylates PTHR2 in chicken adenoid."
  val text3 = "Tbet Rag2 mice (Garrett et al., 2010) as well as Bacteroides spp. (Bloom et al., 2011), Helicobacter spp. (Fox et al., 2011), and Bilophila wadsworthia (Devkota et al., 2012) in Il10 have been shown to enhance intestinal inflammation.The acute dextran sulfate sodium"
  val allTexts = Seq(text1, text2, text3)
  val sortedMentions = allTexts.flatMap { text =>
    val mentions = testReach.extractFrom(text, "serialization-test", "1", None)

    mentions.sortBy { mention => (mention.startOffset, mention.endOffset) }
  }

  behavior of "Hash"

  it should "compute the expected value for a Mention" in {
    val expectedHashes = Array(27986141, -396507223, 1590560579, -1891512069, -914654348, 408527033, -1487373899, -2017652764, 1558808406, 2017209834, -1279750485, -37832763, 200095485, 2020390684, 1876313014, 795103862, 220919393)
    val actualHashes = sortedMentions.map(mentionManager.computeHash)

    actualHashes should be (expectedHashes)
  }

  it should "compute the expected value for a KBResolution" in {
    val expectedHash = 1782466108
    val kbResolution = new KBResolution("text", "namespace", "id", "species")
    val actualHash = kbResolution.hashCode

    actualHash should be (expectedHash)
  }

  it should "compute the expected value for a KBEntry" in {
    val expectedHash = 578280303
    val kbEntry = new KBEntry("text", "namespace", "id", "species")
    val actualHash = kbEntry.hashCode

    actualHash should be (expectedHash)
  }
}
