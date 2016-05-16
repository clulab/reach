package edu.arizona.sista.assembly.export

import edu.arizona.sista.odin.Mention


trait AssemblyLink{
  val foundBy: String
}

case class CausalPrecedence(before: Mention, after: Mention, foundBy: String) extends AssemblyLink

case class Equivalence(m1: Mention, m2: Mention, foundBy: String) extends AssemblyLink

case class Subsumption(general: Mention, specific: Mention, foundBy: String) extends AssemblyLink