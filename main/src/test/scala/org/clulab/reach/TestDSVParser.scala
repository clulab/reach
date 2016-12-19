package org.clulab.reach

import org.clulab.reach.utils.DSVParser
import org.scalatest.{Matchers, FlatSpec}
import java.io.File


class TestDSVParser extends FlatSpec with Matchers {
  val parser = new DSVParser

  val examplePaper = new File(getClass.getResource("/inputs/test-csv/PMC1234335.csv").toURI)

  examplePaper.getName should "use \",\" delimiter" in {
    val delimiter = parser.getDelimiter(examplePaper)
    delimiter should be(",")
  }

  it should "contain 6 FriesEntries" in {
    val entries = parser.toFriesEntries(examplePaper)
    entries should have size(6)
  }

  val funkyCSVString = """1,2,"Furthermore, phosphorylated X does Y""""

  "DSVParser" should s"correctly split '$funkyCSVString' into three columns" in {
    funkyCSVString.split(DSVParser.CSV) should have size(3)
    funkyCSVString.split(parser.getSplitPattern(",")) should have size(3)
  }
}
