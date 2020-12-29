package org.clulab.reach.assembly

import org.clulab.odin.ExtractorEngine
import org.clulab.reach.RuleReader
import org.clulab.reach.assembly.sieves.AssemblyActions
import org.scalatest.{FlatSpec, Matchers}


/**
  * Validate rules
  */
class TestAssemblyRules extends FlatSpec with Matchers {

  val grammmarsDir = "/org/clulab/reach/assembly/grammars"
  val intrasententialRules = s"$grammmarsDir/intrasentential.yml"
  val intersententialRules = s"$grammmarsDir/intersentential.yml"
  val bioDRBRules = s"$grammmarsDir/biodrb-patterns.yml"
  val mihailaRules = s"$grammmarsDir/mihaila-causal-discourse.yml"
  val actions = new AssemblyActions

  intrasententialRules should "compile without error" in {
    // read rules and initialize state with existing mentions
    val rules:String = RuleReader.readResource(intrasententialRules)
    val ee = ExtractorEngine(rules, actions)
    ee.extractors should not be empty
  }

  intersententialRules should "compile without error" in {
    // read rules and initialize state with existing mentions
    val rules:String = RuleReader.readResource(intersententialRules)
    val ee = ExtractorEngine(rules, actions)
    ee.extractors should not be empty
  }

  bioDRBRules should "compile without error" in {
    // read rules and initialize state with existing mentions
    val rules:String = RuleReader.readResource(bioDRBRules)
    val ee = ExtractorEngine(rules, actions)
    ee.extractors should not be empty
  }

  mihailaRules should "compile without error" in {
    // read rules and initialize state with existing mentions
    val rules:String = RuleReader.readResource(mihailaRules)
    val ee = ExtractorEngine(rules, actions)
    ee.extractors should not be empty
  }
}
