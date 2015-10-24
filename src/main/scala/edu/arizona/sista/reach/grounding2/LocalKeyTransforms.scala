package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.reach.grounding2.LocalKBConstants._

/**
  * Specialized lookup key transformation methods, for writing local KB accessors.
  *   Written by Tom Hicks. 10/22/2015.
  *   Last Modified: Initial refactoring.
  */
object LocalKeyTransforms {

  /** List of transform methods to apply for alternate Protein lookups. */
  val proteinKeyTransforms = Seq( stripProteinSuffixes _,
                                  stripFamilySuffixes _,
                                  stripMutantProtein _,
                                  unmutateProteinKey _ )


  /** Return the portion of the key string minus one of the protein family suffixes,
    * if found in the given key string, else return the key unchanged. */
  def stripFamilySuffixes (key:String): String = {
    return LocalKBUtils.stripASuffix(FamilyStopSuffixes, key)
  }

  /** Return the portion of the key string before a trailing mutation phrase,
    * if found in the given key string, else return the key unchanged. */
  def stripMutantProtein (key:String): String = {
    val phosMutePat = """phosphorylated\s+(.*)\s+\w+\s+mutant""".r  // phosphorylation/mutation phrase
    val mutePat = """(.*)\s+\w+\s+mutant""".r  // mutation phrase at end of string
    return key match {
      case phosMutePat(lhs) => lhs
      case mutePat(lhs) => lhs
      case _ => key
    }
  }

  /** Return the portion of the key string minus one of the protein suffixes, if found
    * in the given key string, else return the key unchanged. */
  def stripProteinSuffixes (key:String): String = {
    return LocalKBUtils.stripASuffix(ProteinStopSuffixes, key)
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
