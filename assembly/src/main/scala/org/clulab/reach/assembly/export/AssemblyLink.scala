package org.clulab.reach.assembly.export

import org.clulab.odin.Mention


trait AssemblyLink{
  val foundBy: String
}

case class CausalPrecedence(before: Mention, after: Mention, foundBy: String) extends AssemblyLink

case class Equivalence(m1: Mention, m2: Mention, foundBy: String) extends AssemblyLink

case class Subsumption(general: Mention, specific: Mention, foundBy: String) extends AssemblyLink
