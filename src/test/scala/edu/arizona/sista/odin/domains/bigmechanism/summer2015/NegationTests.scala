package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.bionlp.mentions.BioEventMention
import edu.arizona.sista.bionlp.mentions.Negation

import TestUtils._

class NegationTests extends FlatSpec with Matchers{
  val sentences = Seq(
    "RAS does not phosphorylate MEK",
    "RAS doesn't phosphorylate MEK",
    "RAS is not phosphorylating MEK",
    "RAS isn't phosphorylating MEK",
    "RAS fails to phosphorylate MEK",
    "RAS fails phosphorylating MEK",
    "RAS plays no role in the phosphorylation of MEK",
    "RAS plays little role in the phosphorylation of MEK")

    for(sentence <- sentences){
      sentence should "contain an event with a negation mofification" in {
        val mentions = parseSentence(sentence)

        val events = mentions filter {
          case event:BioEventMention =>
            true
          case _ =>
            false
        }

        val modifications = events flatMap (_.modifications) filter {
          case m:Negation => true
          case _ => false
        }

        modifications should have size (1)
      }
    }
}
