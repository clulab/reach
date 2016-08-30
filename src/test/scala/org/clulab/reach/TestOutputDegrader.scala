package org.clulab.reach

import org.scalatest.{FlatSpec, Matchers}
import org.clulab.reach.mentions._
import TestUtils._


class TestOutputDegrader extends FlatSpec with Matchers {

  // Check that controller is converted to an entity + PTM(Phos) (adapt from this test)
  val sent1 = "The phosphorylation of ASPP1 inhibits the ubiquitination of ASPP2"
  sent1 should "contain a controller with a PTM" in {
    val mentions = getFlattenedBioMentionsFromText(sent1)
    val reg = mentions.find(_ matches "Negative_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    // Before flattening, the text of the controller is the phospho event
    controller.text should be ("ASPP1")
    controller.modifications should have size (1)
    controller.modifications.head.label should be ("Phosphorylation")
    controlled.labels should contain ("Ubiquitination")
    controlled.asInstanceOf[BioEventMention].isDirect should be (false)
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key ("cause")
    controlled.arguments("theme").head.text should be ("ASPP2")
  }

  // Check that controller is converted to a Complex (adapt from this test)
  val sent2 = "The binding of ASPP1 and ASPP2 promotes the phosphorylation of MEK"
  sent2 should "contain a controller with a complex" in {
    val mentions = getFlattenedBioMentionsFromText(sent2)
    val reg = mentions.find(_ matches "Positive_regulation")
    reg should be ('defined)
    reg.get.arguments should contain key ("controller")
    reg.get.arguments should contain key ("controlled")
    reg.get.arguments("controller") should have size (1)
    reg.get.arguments("controlled") should have size (1)
    val controller = reg.get.arguments("controller").head.toBioMention
    val controlled = reg.get.arguments("controlled").head.toBioMention
    controller.text should be ("ASPP1 and ASPP2")
    controller.labels should contain ("Complex")
    controlled.labels should contain ("Phosphorylation")
    controlled.arguments should contain key ("theme")
    controlled.arguments should not contain key ("cause")
    controlled.arguments("theme").head.text should be ("MEK")
    controlled.asInstanceOf[BioEventMention].isDirect should be (false)
  }

  // Check that controller is converted to a Complex (adapt from this test)
  val sent3 = "The binding of BS1 and BS2 promotes the phosphorylation of MEK"
  sent3 should "contain one positive regulation" in {
    val mentions = getFlattenedBioMentionsFromText(sent3)
    val posReg = mentions.filter(_ matches "Positive_regulation")
    posReg should have size (1)
    posReg.head.arguments should contain key ("controller")
    posReg.head.arguments should contain key ("controlled")
    posReg.head.arguments("controller") should have size (1)
    posReg.head.arguments("controlled") should have size (1)
    val controller = posReg.head.arguments("controller").head.toBioMention
    val controlled = posReg.head.arguments("controlled").head.toBioMention
    controller.matches("Complex") should be (true)
    controlled.matches("Phosphorylation") should be (true)
    controlled.asInstanceOf[BioEventMention].isDirect should be (false)
  }

  // Check that controller is converted to an entity + PTM(Phos) (adapt from this test)
  val sent4 = "The phosphorylation of MEK activates K-Ras."
  sent4 should "contain 1 activation with a phosphorylation event as its controller" in {
    val mentions = getFlattenedBioMentionsFromText(sent4)
    val activations = mentions.filter(_ matches "Positive_activation")
    activations should have size (1)
    val mods = activations.head.arguments("controller").head.toBioMention.modifications.map(_.label)
    mods should contain ("Phosphorylation")
  }

  // Check that controller is converted to an entity + PTM(Phos) (adapt from this test)
  val sent5 = "The phosphorylation of MEK deactivates K-Ras."
  sent5 should "contain 1 Negative Activation with a phosphorylation event as its controller" in {
    val mentions = getFlattenedBioMentionsFromText(sent5)
    val negActs = mentions.filter(_ matches "Negative_activation")
    negActs.length should be (1)
    val mods = negActs.head.arguments("controller").head.toBioMention.modifications.map(_.label)
    mods should contain ("Phosphorylation")
    // We shouldn't pick up any Positive Activations
    mentions.count(_ matches "Positve_activation") should be (0)
  }

  // Test Binding -> Complex conversion for controllers of activations
  val sent6 = "The Mek-Ras-Akt1 complex activates ASPP1"
  sent6 should "contain 1 Positive Activation with a COMPLEX as its controller" in {
    val mentions = getFlattenedBioMentionsFromText(sent6)
    val posActs = mentions.filter(_ matches "Positive_activation")
    posActs.length should be (1)
    posActs.head.arguments("controller") should have size 1
    val controller = posActs.head.arguments("controller").head
    // should've been converted to a Complex
    controller.label should equal ("Complex")
    // only the arg "theme"
    controller.arguments should contain key "theme"
    controller.arguments.keySet should have size 1
    // there should be 3 themes
    controller.arguments("theme") should have size 3
  }

  // Test Binding -> Complex conversion for controllers of activations
  val sent7 = "the binding of MEK and RAS activates ASPP1"
  sent7 should "contain 1 Positive Activation with a COMPLEX as its controller" in {
    val mentions = getFlattenedBioMentionsFromText(sent7)
    val posActs = mentions.filter(_ matches "Positive_activation")
    posActs.length should be (1)
    posActs.head.arguments("controller") should have size 1
    val controller = posActs.head.arguments("controller").head
    // should've been converted to a Complex
    controller.label should equal ("Complex")
    // only the arg "theme"
    controller.arguments should contain key "theme"
    controller.arguments.keySet should have size 1
    // there should be 3 themes
    controller.arguments("theme") should have size 2
  }

  // No conversion should happen here
  val sent8 = "The Mek-Ras-Akt1 complex is not well-studied"
  sent8 should "contain a BINDING" in {
    val mentions = getFlattenedBioMentionsFromText(sent8)
    mentions.count(_ matches "Event") should be (1)
    // binding should NOT have been converted to a Complex
    mentions.count(_ matches "Binding") should be (1)
  }
}
