package edu.arizona.sista.reach.grounding2

import edu.arizona.sista.reach.grounding2.ReachKBConstants._

/**
  * REACH-related methods for transforming text strings into potential keys for lookup in KBs.
  *   Written by Tom Hicks. 11/10/2015.
  *   Last Modified: Rename this trait again for consistency.
  */
trait ReachKBKeyTransforms extends KBKeyTransforms {

  /** Canonicalize the given text string into a key for both storage and lookup. */
  def makeCanonicalKey (text:String): String = {
    var key:String = text.toLowerCase
    // KeyStopWords.foreach { word => key = key.replaceAll(word, "") }
    key = key.filterNot(KeyCharactersToRemove)
    return stripASuffix(AllKeysStopSuffixes, key)
  }

  /** Return the portion of the key string minus one of the protein family suffixes,
    * if found in the given key string, else return the key unchanged. */
  def stripFamilySuffixes (key:String): String = {
    return stripASuffix(FamilyStopSuffixes, key)
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
    return stripASuffix(ProteinStopSuffixes, key)
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


/** Trait Companion Object allows Mixin OR Import pattern. */
object ReachKBKeyTransforms extends ReachKBKeyTransforms {

  /** List of transform methods to apply for alternate Protein Family lookups. */
  val familyKeyTransforms = Seq( stripFamilySuffixes _ )

  /** List of transform methods to apply for alternate Protein lookups. */
  val proteinKeyTransforms = Seq( stripProteinSuffixes _,
                                  stripMutantProtein _,
                                  unmutateProteinKey _ )
}
