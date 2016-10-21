package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * REACH-related methods for transforming text strings into potential keys for lookup in KBs.
  *   Written by Tom Hicks. 11/10/2015.
  *   Last Modified: Refactor transform constants to here.
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
    val allTexts = text +: applyTransforms(text, transformFns)
    return allTexts.map(makeCanonicalKey(_))
  }

  /** Return the portion of the text string minus one of the protein family postpositional
    * attributives, if found in the given text string, else return the text lowercased. */
  def stripFamilyPostAttributives (text:String): String = {
    val lcText = text.toLowerCase           // match lower cased text only
    stripSuffixes(FamilyPostAttributives, lcText)
  }

  /** Return the portion of the text string minus one of the organ-cell-type suffixes,
    * if found in the given text string, else return the text unchanged. */
  def stripOrganCellTypeSuffixes (text:String): String = {
    return text match {
      case OrganSuffixPat(lhs, _) => lhs
      case _ => text                        // return text unchanged
    }
  }

  /** Return the portion of the text string before a trailing mutation phrase,
    * if found in the given text string, else return the text unchanged. */
  def stripMutantProtein (text:String): String = {
    return text match {
      case PhosphorMutationPat(lhs) => lhs
      case TrailingMutationPat(lhs) => lhs
      case _ => text                        // return text unchanged
    }
  }

  /** Return the portion of the text string minus any of the PTM-related prefixes, if found
    * in the given text string, else return the text unchanged. */
  def stripPTMPrefixes (text:String): String = text match {
    case PTMPrefixPat(prefix, restOfKey) => restOfKey
    case _ => text
  }

  /** Return the portion of the text string minus one of the protein postpositional
    * attributives, if found in the given text string, else return the text lowercased. */
  def stripProteinPostAttributives (text:String): String = {
    val lcText = text.toLowerCase           // match lower cased text only
    stripSuffixes(ProteinPostAttributives, lcText)
  }

  /** Check for one of several types of hyphen-separated strings and, if found,
    * extract and return the candidate text portion, else return the text unchanged. */
  def hyphenatedProteinKey (text:String): String = {
    return text match {
      // check for RHS protein domain or LHS mutant spec: return protein portion only
      case HyphenatedNamePat(lhs, rhs) => if (isProteinDomain(rhs)) lhs else rhs
      case _ => text                        // return text unchanged
    }
  }
}


/** Trait Companion Object allows Mixin OR Import pattern. */
object ReachKBKeyTransforms extends ReachKBKeyTransforms {

  /** The set of words to remove from all keys to create a lookup key. */
  val AllKeysStopSuffixes = Seq("_human")

  /** The set of words to remove from a key to create a protein family lookup key. */
  val FamilyPostAttributives = Seq(" protein family", " family")

  /** Pattern matching 2 text strings separated by a hyphen, case insensitive. */
  val HyphenatedNamePat = """(?i)(\w+)-(\w+)""".r

  /** The set of characters to remove from the text to create a lookup key. */
  val KeyCharactersToRemove = " /-".toSet

  /** Trailing context strings for organ phrases, case insensitive. */
  val OrganSuffixPat = """(?i)(.*)(cells?|tissues?|fluids?)""".r

  /** Match protein names beginning with special PTM-related prefix characters. */
  val PTMPrefixPat = """(p|u)([A-Z0-9_-][A-Za-z0-9_-]*)""".r

  /** Match phosphorylation mutation phrases, case insensitive. */
  val PhosphorMutationPat = """(?i)phosphorylated\s+(.*)\s+\w+\s+mutant""".r

  /** The set of words to remove from a key to create a protein lookup key. */
  val ProteinPostAttributives = Seq(" mutant protein", " protein")

  /** Match mutation string at end of text string, case insensitive. */
  val TrailingMutationPat = """(?i)(.*)\s+\w+\s+mutant""".r


  /** List of transform methods to apply for alternate Protein Family lookups. */
  val familyKeyTransforms = Seq( stripFamilyPostAttributives _ )

  /** List of transform methods to apply for alternate Organ-CellType lookups. */
  val organCellTypeKeyTransforms = Seq( stripOrganCellTypeSuffixes _ )

  /** List of transform methods to apply for alternate Protein lookups. */
  val proteinKeyTransforms = Seq( stripProteinPostAttributives _,
                                  stripMutantProtein _,
                                  hyphenatedProteinKey _,
                                  stripPTMPrefixes _ )

  /** Set of short protein domain strings. */
  val ProteinDomainShortNames: Set[String] =
    ReachKBUtils.readLines(ProteinDomainShortNamesFilename)
                .map(suffix => makeCanonicalKey(suffix.trim)).toSet

  /** Tell whether the given string names a protein domain or not. */
  def isProteinDomain (domain: String): Boolean =
    ProteinDomainShortNames.contains(makeCanonicalKey(domain))

}
