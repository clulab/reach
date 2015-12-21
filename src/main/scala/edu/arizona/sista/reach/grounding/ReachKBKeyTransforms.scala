package edu.arizona.sista.reach.grounding

import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * REACH-related methods for transforming text strings into potential keys for lookup in KBs.
  *   Written by Tom Hicks. 11/10/2015.
  *   Last Modified: Add logic for an organ to inferred cell type transform.
  */
trait ReachKBKeyTransforms extends KBKeyTransforms {

  /** Canonicalize the given text string into a key for both storage and lookup. */
  def makeCanonicalKey (text:String): String = {
    var key:String = text.toLowerCase
    // KeyStopWords.foreach { word => key = key.replaceAll(word, "") }
    key = key.filterNot(KeyCharactersToRemove)
    return stripSuffixes(AllKeysStopSuffixes, key)
  }


  /** Return alternate lookup keys created from the given text string and transform functions. */
  def reachAlternateKeys (text:String, transformFns:KeyTransforms): Seq[String] = {
    val lcText = text.toLowerCase           // transform lower cased text only
    val allTexts = lcText +: makeAlternateKeys(lcText, transformFns)
    return allTexts.map(makeCanonicalKey(_))
  }


  /** Return the portion of the key string minus one of the protein family suffixes,
    * if found in the given key string, else return the key unchanged. */
  def stripFamilySuffixes (key:String): String = {
    return stripSuffixes(FamilyStopSuffixes, key)
  }

  /** Return the portion of the key string minus one of the organ-cell-type suffixes,
    * if found in the given key string, else return the key unchanged. */
  def stripOrganCellTypeSuffixes (key:String): String = {
    val suffixPat = """(.*)(cells?|tissues?|fluids?)""".r  // trailing context strings
    return key match {
      case suffixPat(lhs, _) => lhs
      case _ => key
    }
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
    return stripSuffixes(ProteinStopSuffixes, key)
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

  /** List of transform methods to apply for alternate Organ-CellType lookups. */
  val organCellTypeKeyTransforms = Seq( stripOrganCellTypeSuffixes _ )

  /** List of transform methods to apply for alternate Protein lookups. */
  val proteinKeyTransforms = Seq( stripProteinSuffixes _,
                                  stripMutantProtein _,
                                  unmutateProteinKey _ )
}
