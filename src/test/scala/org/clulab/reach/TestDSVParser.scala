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
}
