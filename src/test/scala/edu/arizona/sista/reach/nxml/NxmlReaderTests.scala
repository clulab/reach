package edu.arizona.sista.reach.nxml

import edu.arizona.sista.reach.TestUtils._
import io.Source
import org.scalatest.{Matchers, FlatSpec}

trait Fixtures {
  // Set up the fixtures
  def tsv1 = Source.fromURL(getClass.getResource("/tsv/PMC2958468.tsv")).getLines
  .toList.map {
    line =>
    val tokens = line.split("\t")
    FriesEntry(
      "PMC2958468",
      tokens(0),
      tokens(2),
      tokens(1),
      if(tokens(3) == "1") true else false,
      tokens(4)
    )
  }.filter {
    e => !(e.sectionId == "references" || e.sectionId == "abstract" )
  }.filter {
    e => e.sectionId == "article-title" || e.sectionName.startsWith("s")
  }.takeWhile {
    // We only care about the lines that correspond to the <body/> tag.
    // All other will be handled differently (better) by NxmlReader than nxml2fries.py
    // This number is hard-coded and was determined by Enrique.
    e => e.chunkId != "87"
  }.filter {
    e => !e.sectionId.startsWith("supm-")
  }

  def reader = testReader

  def filteredReader = new NxmlReader(Seq("materials|methods", "supplementary-material"))

  def nxml1 = Source.fromURL(getClass.getResource("/tsv/PMC2958468.nxml")).mkString

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
  }.filter {
    e => e.sectionId == "article-title" || e.sectionName.startsWith("s")
  }.takeWhile {
    // We only care about the lines that correspond to the <body/> tag.
    // All other will be handled differently (better) by NxmlReader than nxml2fries.py
    // This number is hard-coded and was determined by Enrique.
    e => e.chunkId != "133"
  }.filter {
    e => !e.sectionId.startsWith("supm-")
  }

  def nxml2 = Source.fromURL(getClass.getResource("/tsv/PMC1702562.nxml")).mkString
  /////////
}

class NxmlReaderTests extends FlatSpec with Matchers with Fixtures {

  // Behavior shared among all NXML documents
  def nxmlDocument(xml:String, lastChunk:String, tsv:Seq[FriesEntry]) ={

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
      val entries = reader.readNxml(xml, "a document name") filter {
        _.sectionId != "abstract"
      } takeWhile {
        _.chunkId != lastChunk
      } filter {
        !_.sectionId.startsWith("fig-")
      } filter {
        !_.sectionId.startsWith("supm-")
      }

      info(s"Number of relevant entries from the NxmlReader: ${entries.size}")
      info(s"Number of relevant entries in the tsv: $numTsvEntries")

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

  // Tests
  behavior of "PMC2958468"

  it should behave like nxmlDocument(nxml1, "87", tsv1)

  it should "have two abstract entries with no titles in them" in {
    val entries = reader.readNxml(nxml1, "PMC2958468")
    val absEntries = entries filter (_.sectionId == "abstract")

    val size = absEntries.filter(!_.isTitle).size
    info(s"The number of abstract entries is $size")
    size should equal (2)

    val titleSize = absEntries.filter(_.isTitle).size
    info(s"The number of abstract entry titles is $titleSize")
    titleSize should equal (0)
  }

  it should "have four different sections" in {
    val entries = reader.readNxml(nxml1, "PMC2958468")
      .filter(_.sectionId != "abstract")
      .filter(_.sectionId != "article-title")
      .filter(!_.sectionId.startsWith("fig-"))
      .filter(!_.sectionId.startsWith("supm-"))

    val sections = entries.map( e => e.sectionName ).toSet

    for(section <- sections){
      info(s"Found section $section")
    }

    sections.size should equal (4)
  }

  it should "have three different sections when filtering" in {
    val entries = filteredReader.readNxml(nxml1, "PMC2958468")
      .filter(_.sectionId != "abstract")
      .filter(_.sectionId != "article-title")
      .filter(!_.sectionId.startsWith("fig-"))
      .filter(!_.sectionId.startsWith("supm-"))

    val sections = entries.map( e => e.sectionName ).toSet

    for(section <- sections){
      info(s"Found section $section")
    }

    sections.size should equal (3)
  }

  it should "have eight different figures with their corresponding label" in {
    val entries = filteredReader.readNxml(nxml1, "PMC2958468")
      .filter(_.sectionId.startsWith("fig-"))

    val numCaptions = entries.filter(!_.isTitle).size
    val numLabels = entries.filter(_.isTitle).size

    info(s"The number of figure captions is $numCaptions")
    numCaptions should equal (8)

    info(s"The number of figure labels is $numLabels")
    numLabels should equal (8)
  }

  behavior of "PMC1702562"

  it should behave like nxmlDocument(nxml2, "104", tsv2)

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

  it should "have six different sections" in {
    val entries = reader.readNxml(nxml2, "PMC1702562")
      .filter(_.sectionId != "abstract")
      .filter(_.sectionId != "article-title")
      .filter(!_.sectionId.startsWith("fig-"))
      .filter(!_.sectionId.startsWith("supm-"))

    val sections = entries.map( e => e.sectionName ).toSet

    for(section <- sections){
      info(s"Found section $section")
    }

    sections.size should equal (6)
  }

  it should "have four different sections when filtering" in {
    val entries = filteredReader.readNxml(nxml2, "PMC1702562")
      .filter(_.sectionId != "abstract")
      .filter(_.sectionId != "article-title")
      .filter(!_.sectionId.startsWith("fig-"))
      .filter(!_.sectionId.startsWith("supm-"))

    val sections = entries.map( e => e.sectionName ).toSet

    for(section <- sections){
      info(s"Found section $section")
    }

    sections.size should equal (4)
  }

  it should "have seven different figures with their corresponding label" in {
    val entries = filteredReader.readNxml(nxml2, "PMC1702562")
      .filter(_.sectionId.startsWith("fig-"))

    val numCaptions = entries.filter(!_.isTitle).map(_.sectionId).toSet.size
    val numLabels = entries.filter(_.isTitle).map(_.sectionId).toSet.size

    info(s"The number of figure captions is $numCaptions")
    numCaptions should equal (7)

    info(s"The number of figure labels is $numLabels")
    numLabels should equal (7)
  }
  ////////
}
