package edu.arizona.sista.bionlp.nxml

import io.Source
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.bionlp.FriesEntry


class NxmlReaderTests extends FlatSpec with Matchers {

  // Behavior shared among all NXML documents
  def nxmlDocument(xml:String, tsv:Seq[FriesEntry]) ={

    it must "have a single article-title entry" in {
      val entries = reader.readNxml(xml, "a document name")

      val size = entries.filter(_.sectionId == "article-title").size

      size should equal (1)
    }

    // Reconsider this test, as it is meaningless since NxmlReader doesn't extract
    // references from the xml document
    it must "not have any reference" in {
      val entries = reader.readNxml(xml, "a document name")

      val size = entries.filter(_.sectionId == "references").size

      size should equal (0)
    }

    it should "have the same number of entries as the tsv" in {
      val numTsvEntries = tsv.size
      val entries = reader.readNxml(xml, "a document name") filter (_.sectionId != "abstract")

      entries.size should equal (numTsvEntries)
    }

    it must "not contain any citation-removal artifact" in {
      val entries = reader.readNxml(xml, "a document name")
      val detector = reader.citationArtifact


      val found = (entries map {
        e:FriesEntry =>
          val matches = detector.findAllIn(e.text).toSeq
          for(m <- matches){
            info(s"Found ${m} in ${e.text}")
          }
          matches.size != 0
      }).foldLeft(false)(_ || _)

      found should be (false)

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
  } filter {
    e => !(e.sectionId == "references" && e.sectionId == "abstract" )
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

  it should "have four abstract entries, with their respective titles" in {
    val entries = reader.readNxml(nxml1, "PMC113262")
    val absEntries = entries filter (_.sectionId == "abstract")

    val size = absEntries.filter(!_.isTitle).size
    info(s"The number of abstract entries is $size")
    size should equal (4)

    val titleSize = absEntries.filter(_.isTitle).size
    info(s"The number of abstract entry titles is $titleSize")
    titleSize should equal (4)
  }

  it should "have six different sections" in {
    val entries = reader.readNxml(nxml1, "PMC113262")
      .filter(_.sectionId != "abstract")
      .filter(_.sectionId != "article-title")

    val sections = entries.map( e => e.sectionName ).toSet

    for(section <- sections){
      info(s"Found section $section")
    }

    sections.size should equal (6)
  }

  it should "have five different sections when filtering" in {
    val entries = filteredReader.readNxml(nxml1, "PMC113262")
      .filter(_.sectionId != "abstract")
      .filter(_.sectionId != "article-title")

    val sections = entries.map( e => e.sectionName ).toSet

    for(section <- sections){
      info(s"Found section $section")
    }

    sections.size should equal (5)
  }

  behavior of "PMC1702562"

  it should behave like nxmlDocument(nxml2, tsv2)

  it should "have three abstract entries, and only one title" in {
    val entries = reader.readNxml(nxml2, "PMC1702562")
    val absEntries = entries filter (_.sectionId == "abstract")

    val size = absEntries.filter(!_.isTitle).size
    info(s"The number of abstract entries is $size")
    size should equal (3)

    val titleSize = absEntries.filter(_.isTitle).size
    info(s"The number of abstract entry titles is $titleSize")
    titleSize should equal(1)
  }

  it should "have five different sections" in {
    val entries = reader.readNxml(nxml2, "PMC1702562")
      .filter(_.sectionId != "abstract")
      .filter(_.sectionId != "article-title")

    val sections = entries.map( e => e.sectionName ).toSet

    for(section <- sections){
      info(s"Found section $section")
    }

    sections.size should equal (5)
  }

  it should "have three different sections when filtering" in {
    val entries = filteredReader.readNxml(nxml2, "PMC1702562")
      .filter(_.sectionId != "abstract")
      .filter(_.sectionId != "article-title")

    val sections = entries.map( e => e.sectionName ).toSet

    for(section <- sections){
      info(s"Found section $section")
    }

    sections.size should equal (3)
  }
  ////////
}
