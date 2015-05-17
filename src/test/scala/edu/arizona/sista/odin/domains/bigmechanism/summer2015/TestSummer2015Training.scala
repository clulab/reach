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

  //
  // General issues observed with the system
  //

  // TODO: don't report AKT + ASPP2 as a binding
  "ReachSystem" should "extract only binary bindings" in {
    val mentions = parseSentence(sent1)

    // this MUST produce Binding(Ras, AKT) and Binding(Ras, ASPP2)
    // TODO: fails! Produces 3 bindings, instead of 2! (MARCO)

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

    mentions = parseSentence(sent6b)
    val u = mentions.find(_.label == "Ubiquitination")
    u.size should be(0)
    b = mentions.find(_.label == "Binding")
    b.size should be (1)
  }

  //
  // from MITRE's training data
  //

  val sent7 = "JAK3 phosphorylates three HuR residues (Y63, Y68, Y200)"
  it should "extract 3 phosphorylations and 3 positive regulations" in {
    // TODO: this fails because of bad syntax around Hur. Fix with a surface rule for phosphorylation? (GUS)
    val mentions = parseSentence(sent7)

    val p = mentions.find(_.label == "Phosphorylation")
    p.size should be (3)
    val r = mentions.find(_.label == "Positive_regulation")
    r.size should be (3)
  }

  val sent8 = "We demonstrate that the RBD of PI3KC2β binds nucleotide-free Ras in vitro."
  it should "extract \"site of protein\" patterns" in {
     // TODO: this fails because we do not capture "site of protein" (GUS)
     // Also: if the entity modification has no type, it should be propagated up in the event using the entity
     val mentions = parseSentence(sent8)

     val f = mentions.find(_.label == "Family")
     f.size should be (1)
     val p = mentions.find(_.label == "Gene_or_gene_product")
     p.size should be (1)

     val b = mentions.find(_.label == "Binding")
     b.size should be (1)
  }

  val sent9 = "Nucleotide free Ras inhibits PI3KC2Beta activity."
  val sent9b = "Nucleotide free Ras inhibits PI3KC2Beta."
  val sent9c = "Nucleotide free Ras inhibits activation of PI3KC2Beta."
  val sent9d = "Addition of Ras inhibits PI3KC2Beta."
  val sent9e = "Increase of Ras dose inhibits PI3KC2Beta."
  it should "extract negative activation patterns" in {
     var mentions = parseSentence(sent9)
     mentions.find(_.label == "Negative_activation").size should be (1)

     mentions = parseSentence(sent9b)
     mentions.find(_.label == "Negative_activation").size should be (1)

     mentions = parseSentence(sent9c)
     mentions.find(_.label == "Negative_activation").size should be (1)

     mentions = parseSentence(sent9d)
     mentions.find(_.label == "Negative_activation").size should be (1)

     mentions = parseSentence(sent9e)
     mentions.find(_.label == "Negative_activation").size should be (1)
  }

  val sent10 = "Experiments revealed ubiquitination at Lys residues 104 and 147 of K-Ras"
  val sent10b = "Experiments revealed ubiquitination at Lys residues 117, 147, and 170 for H-Ras."
  it should "extract multiple different ubiquitinations" in {
    var mentions = parseSentence(sent10)
    mentions.find(_.label == "Ubiquitination") should be (2) 

    mentions = parseSentence(sent10b)
    mentions.find(_.label == "Ubiquitination") should be (3) 
  }

  val sent11 = "Ubiquitinated Ras activates Raf and PI3K."
  it should "extract multiple different positive activations" in {
    var mentions = parseSentence(sent11)
    mentions.find(_.label == "Positive_activation") should be (2) 
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
