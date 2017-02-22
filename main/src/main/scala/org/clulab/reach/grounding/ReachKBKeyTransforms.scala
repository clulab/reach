package org.clulab.reach.grounding

import scala.util.matching.Regex

import org.clulab.reach.mentions._
import org.clulab.reach.grounding.KBKeyTransforms._
import org.clulab.reach.grounding.KBLookupSet._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * REACH-related methods for transforming mentions and text strings into potential keys
  * for lookup in KBs.
  *   Written by Tom Hicks. 11/10/2015.
  *   Last Modified: A little smarter gene name affix stripping.
  */
trait ReachKBKeyTransforms extends KBKeyTransforms {

  /** Canonicalize the given text string into a key for both storage and lookup. */
  def canonicalKey (text:String): String =
    text.trim.toLowerCase.filterNot(KeyCharactersToRemove)

  /** A key transform which implements a canonicalization function for Strings. */
  def canonicalKT (text:String): KeyCandidates = toKeyCandidates(canonicalKey(text))


  /** Remove suffixes from all keys candidates. */
  def stripAllKeysSuffixes (text:String): String = stripSuffixByPattern(AllKeysSuffixPat, text)

  /** Return the portion of the text string minus one of the protein family postpositional
    * attributives, if found in the given text string, else return no candidates. */
  def stripFamilyPostAttributivesKT (text:String): KeyCandidates = text.trim match {
    case UnderscoreFamilySuffixPat(ttext) => Seq(ttext)
    case FamilyPostAttributivePat(lhs) => Seq(lhs.trim)
    case _ => NoCandidates                  // signal failure
  }

  /** Try to remove some specific affixes from the given hyphenated text and return a
      dash-concatenated string of non-affixes, or no candidates if no affixes removed. */
  def stripGeneNameAffixesKT (text:String): KeyCandidates = {
    val trimText = text.trim
    val sansSuffixes = stripSuffixByPattern(GeneNameSuffixPat, trimText) // remove any suffixes
    val prefixParts = sansSuffixes.split("-")
    val sansAffixes = prefixParts.filterNot(isGeneNamePrefix(_)).mkString("-") // strip prefixes
    if (sansAffixes == trimText)            // if no affixes found anywhere
      return NoCandidates                   // signal failure
    if (sansAffixes == "")                  // if all parts were affixes
      return Seq(prefixParts.last)          // return last prefix as candidate stem
    else if (sansAffixes != sansSuffixes)   // if some prefixes were stripped
      Seq(sansAffixes)                      // return new candidate string
    else {                                  // else no prefixes were stripped: check suffixes
      if (sansSuffixes == trimText)         // if no suffixes were stripped
        NoCandidates                        // signal failure
      else Seq(sansSuffixes)                // else return de-suffixed string
    }
  }

  /** Return the portion of the text string before a trailing mutation phrase,
    * if found in the given text string, else return no candidates. */
  def stripMutantProteinKT (text:String): KeyCandidates = text.trim match {
    case PhosphorMutationPat(chunk) => Seq(chunk.trim)
    case TrailingMutationPat(lhs) => Seq(lhs.trim)
    case LeadingMutationPat(rhs) => Seq(rhs.trim)
    case _ => NoCandidates                  // signal failure
  }

  /** Return the portion of the text string minus one of the organ-cell-type suffixes,
    * if found in the given text string, else return no candidates. */
  def stripOrganPostAttributivesKT (text:String): KeyCandidates = text.trim match {
    case OrganPostAttributivePat(lhs) => Seq(lhs.trim)
    case _ => NoCandidates                  // signal failure
  }

  /** Check for hyphen-separated strings where the RHS is a protein domain specification.
    * Extract and return the candidate text portion, else return no candidates. */
  def stripProteinDomainKT (text:String): KeyCandidates = text.trim match {
    case HyphenatedNamePat(lhs, rhs) => if (isProteinDomain(rhs)) Seq(lhs.trim) else NoCandidates
    case _ => NoCandidates                  // signal failure
  }

  /** Return the portion of the text string minus one of the protein postpositional
    * attributives, if found in the given text string, else return no candidates. */
  def stripProteinPostAttributivesKT (text:String): KeyCandidates = text.trim match {
    case ProteinPostAttributivePat(lhs) => Seq(lhs.trim)
    case _ => NoCandidates                  // signal failure
  }

  /** Return the portion of the text string minus any of the PTM-related prefixes, if found
    * in the given text string, else return no candidates. */
  def stripPTMPrefixesKT (text:String): KeyCandidates = text.trim match {
    case PTMPrefixPat(prefix, restOfKey) => Seq(restOfKey.trim)
    case _ => NoCandidates                // signal failure
  }

  /** Remove suffix(es) matching given pattern from text, return matched part or text.
    * Trims matching part or text as a side effect. */
  def stripSuffixByPattern (pattern:Regex, text:String): String = text.trim match {
    case pattern(matched) => matched.trim
    case ttext:String => ttext
  }
}


/** Trait Companion Object allows Mixin OR Import pattern. */
object ReachKBKeyTransforms extends ReachKBKeyTransforms {

  /** The set of suffixes to remove from all keys to create a lookup key. */
  val AllKeysSuffixPat = """(?i)(.*)(?:_human)""".r

  /** Match patterns to create a protein family lookup key. */
  val UnderscoreFamilySuffixPat = """(?i)(.*_family)""".r
  val FamilyPostAttributivePat =  """(?i)(.*?)(?: protein family|family)""".r

  /** The set of suffixes to remove from all keys to create a lookup key. */
  val GeneNameSuffixPat = """(?i)(.*?)(?:-?e?GFP)+""".r

  /** Pattern matching 2 text strings separated by a hyphen, case insensitive. */
  val HyphenatedNamePat = """(?i)(\w+)-(\w+)""".r

  /** The set of characters to remove from the text to create a lookup key. */
  val KeyCharactersToRemove = " '/-".toSet

  /** Match mutation string at end of text string, case insensitive. */
  val LeadingMutationPat = """(?i)mutant(?: |-)+(.*)""".r

  /** Match patterns to create an organ lookup key. */
  val OrganPostAttributivePat = """(?i)(.*?)(?: cells?| tissues?| fluids?)+""".r

  /** Match protein names beginning with special PTM-related prefix characters. */
  val PTMPrefixPat = """(p|u)([A-Z0-9_-][A-Za-z0-9_-]*)""".r

  /** Match phosphorylation mutation phrases, case insensitive. */
  val PhosphorMutationPat = """(?i)phosphorylated\s+(.*)\s+\w+\s+mutant""".r

  /** Match patterns to create a protein lookup key. */
  val ProteinPostAttributivePat = """(?i)(.*?)(?: mutant protein|protein)""".r

  /** Match mutation string at end of text string, case insensitive. */
  val TrailingMutationPat = """(?i)(.*)\s+\w+\s+mutant""".r


  /** Default list of text transforms to use with a KB. */
  val DefaultKeyTransforms = Seq( canonicalKT _ )

  /** List of text transforms to use with a case-sensitive KB. */
  // val CasedKeyTransforms = Seq( identityKT _, lowercaseKT _, canonicalKT _ )
  val CasedKeyTransforms = Seq( identityKT _, canonicalKT _ )

  /** Key transform sequence containing a single identity key transform (a NOP). */
  val IdentityKeyTransforms = Seq ( identityKT _ )


  /** List of transform methods to apply for alternate Protein Family lookups. */
  val FamilyAuxKeyTransforms = Seq( stripFamilyPostAttributivesKT _ )

  /** List of transform methods to apply for alternate Organ lookups. */
  val OrganAuxKeyTransforms = Seq( stripOrganPostAttributivesKT _ )

  /** List of transform methods to apply for alternate Protein lookups. */
  val ProteinAuxKeyTransforms = Seq( stripProteinPostAttributivesKT _,
                                     stripMutantProteinKT _,
                                     stripProteinDomainKT _,
                                     stripGeneNameAffixesKT _,
                                     stripPTMPrefixesKT _ )
}
