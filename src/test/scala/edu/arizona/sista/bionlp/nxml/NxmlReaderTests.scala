package edu.arizona.sista.bionlp.nxml

import io.Source
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.bionlp.FriesEntry


class NxmlReaderTests extends FlatSpec with Matchers {

  // Behavior shared among all NXML documents
  def nxmlDocument(xml:String, tsv:Seq[FriesEntry]) ={

    it must "have a single article-title entry" in {
      pending
    }

    it must "not have any reference" in {
      pending
    }

    it should "have the same number of entries as the tsv" in {
      pending
    }
  }

  // Set up the fixtures
  def tsv1 = Source.fromURL(getClass.getResource("/tsv/PMC113262.tsv")).getLines
  .toList.map {
    line =>
    val tokens = line.split("\t")
    FriesEntry(
      "PMC113262",
      tokens(0),
      tokens(2),
      tokens(1),
      if(tokens(3) == "1") true else false,
      tokens(4)
    )
  }

  def reader = new NxmlReader

  def filteredReader = new NxmlReader(Seq("materials|methods", "supplementary-material"))

  def nxml1 = Source.fromURL(getClass.getResource("/tsv/PMC113262.nxml")).mkString

  def tsv2 = Source.fromURL(getClass.getResource("/tsv/PMC1702562.tsv")).getLines
  .toList.map {
    line =>
    val tokens = line.split("\t")
    FriesEntry(
      "PMC1702562",
      tokens(0),
      tokens(2),
      tokens(1),
      if(tokens(3) == "1") true else false,
      tokens(4)
    )
  } filter {
    e => !(e.sectionId == "references" && e.sectionId == "abstract" )
  }

  def nxml2 = Source.fromURL(getClass.getResource("/tsv/PMC1702562.nxml")).mkString
  /////////

  // Tests
  behavior of "PMC113262"

  it should behave like nxmlDocument(nxml1, tsv1)

  it should "have four abstract entries" in {
    pending
  }

  it should "have a normalized section in the abstract" in {
    pending
  }

  it should "have six different sections" in {
    pending
  }

  it should "have five different sections when filtering" in {
    pending
  }

  behavior of "PMC1702562"

  it should behave like nxmlDocument(nxml2, tsv2)

  it should "have one abstract entry" in {
    pending
  }

  it should "have five different sections" in {
    pending
  }

  it should "have three different sections when filtering" in {
    pending
  }
  ////////
}
