package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.bionlp.mentions.BioMention
import edu.arizona.sista.bionlp.{FriesEntry, ReachSystem}
import org.scalatest.{Matchers, FlatSpec}

import edu.arizona.sista.odin.domains.bigmechanism.dryrun2015._

import scala.util.Try

/**
 * New tests based on the new summer system
 * User: mihais
 * Date: 5/13/15
 */
class TestSummer2015Training extends FlatSpec with Matchers {
  // instantiate ReachSytem for tests
  val reach = new ReachSystem

  // test data
  val sent1 = "The ubiquitinated Ras binds AKT and ASPP2."
  val sent2 = "The ubiquitinated Ras protein binds AKT."
  val docId = "testdoc"
  val chunkId = "1"

  "ReachSystem" should "extract only binary bindings" in {
    val entry = FriesEntry(docId, chunkId, "example", "example", isTitle = false, sent1)
    val result = Try(reach.extractFrom(entry))
    result.isSuccess should be (true)

    // this MUST produce Binding(Ras, AKT) and Binding(Ras, ASPP2)
    // TODO: fails! Produces 4 bindings! (MARCO)

    val mentions = printMentions(result)
    val bindings = mentions.filter(_.label == "Binding")
    bindings.size should be (2) // we must have exactly two bindings here

    for(b <- bindings) {
      b.arguments.get("theme").get.size should be (2) // each binding must have exactly two themes
      b.arguments.get("theme").get.find(_.text == "Ras").isDefined should be (true) // Ras is a theme in all these
    }
  }

  it should "extract a binding when an entity modifies \"protein\"" in {
    val entry = FriesEntry(docId, chunkId, "example", "example", isTitle = false, sent2)
    val result = Try(reach.extractFrom(entry))
    result.isSuccess should be (true)

    // this MUST produce Binding(Ras, AKT)
    // TODO: fails! Produces no bindings!
    // TODO: Entity modifying "protein" works for Phospho. It MUST work for all events! (GUS)

    val mentions = printMentions(result)
    val bindings = mentions.filter(_.label == "Binding")
    bindings.size should be (1) // we must have exactly 1 binding2 here

    for(b <- bindings) {
      b.arguments.get("theme").get.size should be (2) // each binding must have exactly two themes
      b.arguments.get("theme").get.find(_.text == "Ras").isDefined should be (true) // Ras is a theme
    }
  }

  def printMentions(result:Try[Seq[BioMention]]):Seq[BioMention] = {
    val mentions = result.get
    println("Mentions:")
    for(m <- mentions) {
      mentionToStrings(m).foreach(println(_))
      println()
    }
    mentions
  }
}
