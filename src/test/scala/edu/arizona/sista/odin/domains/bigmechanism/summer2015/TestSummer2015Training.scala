package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import scala.util.Try
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.bionlp.mentions.BioMention
import edu.arizona.sista.bionlp.{FriesEntry, ReachSystem}
import edu.arizona.sista.bionlp.display._

/**
 * New tests based on the new summer system
 * User: mihais
 * Date: 5/13/15
 */
class TestSummer2015Training extends FlatSpec with Matchers {
  // instantiate ReachSystem for tests
  val reach = new ReachSystem

  // test data
  val sent1 = "The ubiquitinated Ras binds AKT and ASPP2."
  val sent2 = "The ubiquitinated Ras protein binds AKT."
  val sent3 = "AKT binds AKT."
  val sent3b = "binding to the L858R EGFR"
  val sent4 = "The AKT binding was successful."
  val sent5 = "The phosphorylation on AKT was great."
  val sent6 = "The phosphorylated AKT binds to ASPP2."
  val sent6b = "The ubiquitinated AKT binds to ASPP2."

  "ReachSystem" should "extract only binary bindings" in {
    val mentions = parseSentence(sent1)

    // this MUST produce Binding(Ras, AKT) and Binding(Ras, ASPP2)
    // TODO: fails! Produces 4 bindings! (MARCO)

    val bindings = mentions.filter(_.label == "Binding")
    bindings.size should be(2) // we must have exactly two bindings here

    for (b <- bindings) {
      b.arguments.get("theme").get.size should be(2) // each binding must have exactly two themes
      b.arguments.get("theme").get.find(_.text == "Ras").isDefined should be(true) // Ras is a theme in all these
    }
  }

  it should "extract a binding when an entity modifies \"protein\"" in {
    val mentions = parseSentence(sent2)

    // this MUST produce Binding(Ras, AKT)
    // TODO: fails! Produces no bindings!
    // TODO: Entity modifying "protein" works for Phospho. It MUST work for all events! (GUS)

    val bindings = mentions.filter(_.label == "Binding")
    bindings.size should be(1) // we must have exactly 1 binding2 here

    for (b <- bindings) {
      b.arguments.get("theme").get.size should be(2) // each binding must have exactly two themes
      b.arguments.get("theme").get.find(_.text == "Ras").isDefined should be(true) // Ras is a theme
    }
  }

  it should "not extract a binding between two mentions of the same protein" in {
    var mentions = parseSentence(sent3)

    // this MUST produce no bindings!
    // TODO: Fails! (GUS or MARCO)

    var binds = mentions.find(_.label == "Binding")
    binds.size should be(0)

    mentions = parseSentence(sent3b)

    // this MUST produce no bindings!
    // something fishy because of PwS (GUS)

    binds = mentions.find(_.label == "Binding")
    binds.size should be(0)
  }

  it should "not produce unary bindings" in {
    val mentions = parseSentence(sent4)
    val bindings = mentions.find(_.label == "Binding")
    bindings.size should be(0)
  }

  it should "not produce a phosphorylation based on the preposition \"on\"" in {
    // TODO: Fails! (GUS)
    val mentions = parseSentence(sent5)
    val p = mentions.find(_.label == "Phosphorylation")
    p.size should be(0)
  }

  it should "not find a PTMs as events" in {
    // TODO: Both fail! (DANE + MARCO)
    var mentions = parseSentence(sent6)
    val p = mentions.find(_.label == "Phosphorylation") // Dane: this is a PTM not an event!
    p.size should be(0) // Dane
    var b = mentions.find(_.label == "Binding") // Marco: why does this fail??
    b.size should be (1) // Marco

    var uentions = parseSentence(sent6)
    val u = mentions.find(_.label == "Ubiquitination")
    u.size should be(0)
    b = mentions.find(_.label == "Binding")
    b.size should be (1)
  }

  def parseSentence(sentence:String):Seq[BioMention] = {
    val docId = "testdoc"
    val chunkId = "1"
    val entry = FriesEntry(docId, chunkId, "example", "example", isTitle = false, sentence)
    val result = Try(reach.extractFrom(entry))
    result.isSuccess should be (true)
    val mentions = printMentions(result)
    mentions
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
