package edu.arizona.sista.bionlp.reach.ruler

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import TestResources.{bioproc, extractor}
import DarpaEvalUtils._

/**
 * Created by gus on 1/16/15.
 */


class DemoTests extends FunSuite with BeforeAndAfter  {

  def summarizeError(sentence: String, label: String, assignedParty: String): String = s"Failed ${label} test for sentence:\n\tWe measured transcription activation in the presence of ASPP2, which is phosphorylated by Ras.\n\tResponsible: ${assignedParty}"


  val text = "IKK contains two catalytic subunits, IKKalpha and IKKbeta, both of which are able to correctly phosphorylate IkappaB."
  val doc = bioproc.annotate(text)
  val mentions = extractor.extractFrom(doc)
  val assignedParty = "GUS"

  info(text)
  test("there should be a phosphorylation of IkappaB") {
    assert(hasEventWithArguments("Phosphorylation", List("IkappaB"), mentions), summarizeError(text, "Phosphorylation", assignedParty))
  }

  test("there should be an up-regulation between IKKalpha and the phosphorylation of IkappaB") {
    assert(hasUpRegulationByEntity("IKKalpha", "Phosphorylation", List("IkappaB"), mentions), summarizeError(text, "UpRegulation", assignedParty))
  }

  test("there should be an up-regulation between IKKbeta and the phosphorylation of IkappaB") {
    assert(hasUpRegulationByEntity("IKKalpha", "Phosphorylation", List("IkappaB"), mentions), summarizeError(text, "UpRegulation", assignedParty))
  }
  //test("In the future")(pending)
}

