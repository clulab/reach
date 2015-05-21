package edu.arizona.sista.odin.domains.bigmechanism.summer2015

/**
  * Specialized lookup key transformation methods, for writing local KB accessors.
  *   Written by Tom Hicks. 5/21/2015.
  *   Last Modified: Initial creation.
  */
object LocalKeyTransforms {

  /** List of transform methods to apply for alternate Protein lookups. */
  val proteinKeyTransforms = Seq(stripMutantProtein _, unmutateProteinKey _)


  /** Return the portion of the key string before a trailing mutation phrase, if found
    * in the given key string, else return the key unchanged. */
  def stripMutantProtein (key:String): String = {
    val keyPat = """(.*)\w\smutant""".r     // mutation phrase at end of string
    return key match {
      case keyPat(lhs) => lhs
      case _ => key
    }
  }

  /** Return the protein portion of a mutatation-protein string, if found
    * in the given key string, else return the key unchanged. */
  def unmutateProteinKey (key:String): String = {
    val keyPat = """\w+-(\w+)""".r          // hyphen-separated words
    return key match {
      case keyPat(rhs) => rhs
      case _ => key
    }
  }

}
