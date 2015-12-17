package edu.arizona.sista.reach

import edu.arizona.sista.reach.utils.CSVParser
import org.scalatest.{Matchers, FlatSpec}

/**
 *
 * User: mihais
 * Date: 10/30/15
 */
class TestCSVParser extends FlatSpec with Matchers {
  val parser = new CSVParser
  val sen1 = "one, two, \"three and four\" , \" five \""

  sen1 should "contain four CSV fields" in {
    val tokens = parser.splitLine(sen1)
    println("The tokens are: " + tokens.mkString("; "))
    tokens should have size(4)
  }

  val sen2 = "PMC2900437,1,\"Lysates of U2OS cells transfected with HA-tagged B56γ3 or F395C were immunoprecipitated with anti-HA antibody, then analyzed by western blot against p53, Cyclin G, ERK, Sgo, PP2A A, HA, and vinculin (vinc) antibodies.\",\"Mutant\""
  sen2 should "contain four CSV fields" in {
    val tokens = parser.splitLine(sen2)
    println("The tokens are: " + tokens.mkString("; "))
    tokens should have size(4)
  }

  val sen3 = "PMC4047089,200,We also observed increased phosphorylation of ATR and MCM2 in the S1333A-ATR cell line and slightly decreased MCM2 phosphorylation in the S1333D cell line (Fig. 5A).,\"In vitro, the basal kinase activity of S1333A-ATR is higher than wild type (Fig. 1C). To test if this is true in cells, we analyzed basal phosphorylation levels of multiple ATR substrates in three wild type, three S1333A, and three S1333D clonal cell lines without any added genotoxic stress. Phosphorylation levels were analyzed by calculating the ratio of phosphorylated protein to total protein and then normalized to wild type ATR. S1333A-ATR cells contain higher levels of phosphorylated CHK1 compared to wild type and S1333D-ATR (Fig. 5A). We also observed increased phosphorylation of ATR and MCM2 in the S1333A-ATR cell line and slightly decreased MCM2 phosphorylation in the S1333D cell line (Fig. 5A). However, we did not detect significantly decreased levels of pCHK1 and pATR in the S1333D-ATR cells. Figure 5B illustrates that the difference in pCHK1 levels in the cells is not due to small differences in ATR expression levels since there was no correlation between ATR protein expression and the pCHK1/CHK1 ratio measured by immunoblotting.\""
  sen3 should "contain four CSV fields" in {
    val tokens = parser.splitLine(sen3)
    println("The tokens are: " + tokens.mkString("; "))
    tokens should have size(4)
  }
}
