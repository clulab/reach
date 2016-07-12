package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * REACH-related methods for transforming text strings into potential keys for lookup in KBs.
  *   Written by Tom Hicks. 11/10/2015.
  *   Last Modified: Restrict PTM patterns per issue #90.
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


  /** Return the portion of the text string minus one of the protein family suffixes,
    * if found in the given text string, else return the text lowercased. */
  def stripFamilySuffixes (text:String): String = {
    val lcText = text.toLowerCase           // match lower cased text only
    stripSuffixes(FamilyStopSuffixes, lcText)
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

  /** Return the portion of the text string minus one of the protein suffixes, if found
    * in the given text string, else return the text lowercased. */
  def stripProteinSuffixes (text:String): String = {
    val lcText = text.toLowerCase           // match lower cased text only
    stripSuffixes(ProteinStopSuffixes, lcText)
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

  /** Pattern matching 2 text strings separated by a hyphen, case insensitive. */
  val HyphenatedNamePat = """(?i)(\w+)-(\w+)""".r

  /** Trailing context strings for organ phrases, case insensitive. */
  val OrganSuffixPat = """(?i)(.*)(cells?|tissues?|fluids?)""".r

  /** Match protein names beginning with special PTM-related prefix characters. */
  val PTMPrefixPat = """(p|u)([A-Z0-9_-][A-Za-z0-9_-]*)""".r

  /** Match phosphorylation mutation phrases, case insensitive. */
  val PhosphorMutationPat = """(?i)phosphorylated\s+(.*)\s+\w+\s+mutant""".r

  /** Match mutation string at end of text string, case insensitive. */
  val TrailingMutationPat = """(?i)(.*)\s+\w+\s+mutant""".r


  /** List of transform methods to apply for alternate Protein Family lookups. */
  val familyKeyTransforms = Seq( stripFamilySuffixes _ )

  /** List of transform methods to apply for alternate Organ-CellType lookups. */
  val organCellTypeKeyTransforms = Seq( stripOrganCellTypeSuffixes _ )

  /** List of transform methods to apply for alternate Protein lookups. */
  val proteinKeyTransforms = Seq( stripProteinSuffixes _,
                                  stripMutantProtein _,
                                  hyphenatedProteinKey _,
                                  stripPTMPrefixes _ )

  /** Set of protein domain suffixes. */
  val proteinDomainSuffixes: Set[String] = ReachKBUtils.readLines(ProteinDomainSuffixesFilename).map(suffix => makeCanonicalKey(suffix.trim)).toSet

  def isProteinDomain (domain: String): Boolean =
    proteinDomainSuffixes.contains(makeCanonicalKey(domain))

}
