package org.clulab.reach

import org.scalatest.{FlatSpec, Matchers}
import TestUtils.getMentionsFromText


class TestTemplaticAutoEvents extends FlatSpec with Matchers {

  val example1 = "EGFR autophosphorylates on a tyrosine residue."
  example1 should "contain an AutoPhosphorylation with a Site" in {

    val mentions = getMentionsFromText(example1)
    val autophos = mentions filter(_ matches "AutoPhosphorylation")
    val regs = mentions filter(_ matches "Positive_regulation")

    autophos should have size (1)
    autophos.head.arguments.keySet should contain ("site")
    autophos.head.arguments.keySet should contain ("theme")
    autophos.head.arguments("theme") should have size (1)
    autophos.head.arguments("theme").head.text should equal ("EGFR")

    regs shouldNot be (empty)
    regs.head.arguments.keySet should contain ("controller")
    regs.head.arguments("controller") should have size (1)
    regs.head.arguments("controller").head.text should equal ("EGFR")
  }

  val example2 = "EGFR phosphorylates itself."
  example2 should "contain an AutoPhosphorylation without a Site" in {

    val mentions = getMentionsFromText(example2)
    val autophos = mentions filter(_ matches "AutoPhosphorylation")
    val regs = mentions filter(_ matches "Positive_regulation")

    autophos should have size (1)
    autophos.head.arguments.keySet shouldNot contain ("site")
    autophos.head.arguments.keySet should contain ("theme")
    autophos.head.arguments("theme") should have size (1)
    autophos.head.arguments("theme").head.text should equal ("EGFR")

    regs shouldNot be (empty)
    regs.head.arguments.keySet should contain ("controller")
    regs.head.arguments("controller") should have size (1)
    regs.head.arguments("controller").head.text should equal ("EGFR")
  }

  val example3 = "However, stimulation with EGF for varying time intervals revealed no significant differences in the levels of autophosphorylation of EGFR in cells expressing wild type Gab1 versus the Gab1 F446/472/589 mutant."
  example3 should "contain an AutoPhosphorylation without a Site" in {

    val mentions = getMentionsFromText(example3)
    val autophos = mentions filter(_ matches "AutoPhosphorylation")
    val regs = mentions filter(_ matches "Positive_regulation")

    autophos should have size (1)
    autophos.head.arguments.keySet shouldNot contain ("site")
    autophos.head.arguments.keySet should contain ("theme")
    autophos.head.arguments("theme") should have size (1)
    autophos.head.arguments("theme").head.text should equal ("EGFR")

    regs shouldNot be (empty)
    regs.head.arguments.keySet should contain ("controller")
    regs.head.arguments("controller") should have size (1)
    regs.head.arguments("controller").head.text should equal ("EGFR")
  }

  val example4 = "As has been previously reported, recruitment of Shp2 by Gab1 does not alter the magnitude or kinetics of tyrosine autophosphorylation of EGFR."
  example4 should "contain an AutoPhosphorylation with a Site" in {

    val mentions = getMentionsFromText(example4)
    val autophos = mentions filter (_ matches "AutoPhosphorylation")
    val regs = mentions filter (_ matches "Positive_regulation")

    autophos should have size (1)
    autophos.head.arguments.keySet should contain ("site")
    autophos.head.arguments.keySet should contain ("theme")
    autophos.head.arguments("theme") should have size (1)
    autophos.head.arguments("theme").head.text should equal("EGFR")

    regs shouldNot be (empty)
    regs.head.arguments.keySet should contain ("controller")
    regs.head.arguments("controller") should have size (1)
    regs.head.arguments("controller").head.text should equal("EGFR")
  }

  val example5 = "Levels of EGFR autophosphorylation are represented linearly following quantitation by densitometry and normalization for protein expression levels."
  example5 should "contain an AutoPhosphorylation without a Site" in {

    val mentions = getMentionsFromText(example5)
    val autophos = mentions filter (_ matches "AutoPhosphorylation")
    val regs = mentions filter (_ matches "Positive_regulation")

    autophos should have size (1)
    autophos.head.arguments.keySet shouldNot contain ("site")
    autophos.head.arguments.keySet should contain ("theme")
    autophos.head.arguments("theme") should have size (1)
    autophos.head.arguments("theme").head.text should equal("EGFR")

    regs shouldNot be (empty)
    regs.head.arguments.keySet should contain ("controller")
    regs.head.arguments("controller") should have size (1)
    regs.head.arguments("controller").head.text should equal("EGFR")
  }

  val example6 = "Because the substrates of Shp2 are for the most part unknown, we were additionally interested in examining the state of EGFR tyrosine phosphorylation following treatment with EGF in order to determine if the failure of Gab1 to bind p85, and potentially recruit Shp2, would influence levels of EGFR autophosphorylation."
  example6 should "contain an AutoPhosphorylation without a Site" in {

    val mentions = getMentionsFromText(example6)
    val autophos = mentions filter (_ matches "AutoPhosphorylation")
    val regs = mentions filter (_ matches "Positive_regulation")

    autophos should have size (1)
    autophos.head.arguments.keySet shouldNot contain ("site")
    autophos.head.arguments.keySet should contain ("theme")
    autophos.head.arguments("theme") should have size (1)
    autophos.head.arguments("theme").head.text should equal("EGFR")

    regs shouldNot be (empty)
    regs.head.arguments.keySet should contain ("controller")
    regs.head.arguments("controller") should have size (1)
    regs.head.arguments("controller").head.text should equal("EGFR")
  }

  val example7 = "The experiment presented in Fig shows that all cell lines exhibit EGFR autophosphorylation in response to EGF treatment, while only cells expressing the ectopically introduced ErbB3 protein show ErbB3 tyrosine phosphorylation in response to EGF stimulation."
  example7 should "contain an AutoPhosphorylation without a Site" in {

    val mentions = getMentionsFromText(example5)
    val autophos = mentions filter (_ matches "AutoPhosphorylation")
    val regs = mentions filter (_ matches "Positive_regulation")

    autophos should have size (1)
    autophos.head.arguments.keySet shouldNot contain ("site")
    autophos.head.arguments.keySet should contain ("theme")
    autophos.head.arguments("theme") should have size (1)
    autophos.head.arguments("theme").head.text should equal("EGFR")

    regs shouldNot be (empty)
    regs.head.arguments.keySet should contain ("controller")
    regs.head.arguments("controller") should have size (1)
    regs.head.arguments("controller").head.text should equal("EGFR")
  }
}

