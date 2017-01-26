package org.clulab.reach.grounding

import org.clulab.reach.mentions._
import org.clulab.reach.grounding.KBLookupSet._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * REACH-related methods for transforming mentions and text strings into potential keys
  * for lookup in KBs.
  *   Written by Tom Hicks. 11/10/2015.
  *   Last Modified: Organ pattern repeats. Use pattern for protein KT. Simplify family pattern.
  */
trait ReachKBKeyTransforms extends KBKeyTransforms {

  /** Canonicalize the given text string into a key for both storage and lookup. */
  def canonicalKey (text:String): String =
    text.trim.toLowerCase.filterNot(KeyCharactersToRemove)

  /** A key transform which implements a canonicalization function for Strings. */
  def canonicalKT (text:String): KeyCandidates = toKeyCandidates(canonicalKey(text))

  /** A key transform which implements a canonicalization function for Mentions. */
  def canonicalMKT (mention:BioTextBoundMention): KeyCandidates = canonicalKT(mention.text)


  /** Check for one of several types of hyphen-separated strings and, if found,
    * extract and return the candidate text portion, else return the text unchanged. */
  def hyphenatedProteinKT (text:String): KeyCandidates = text.trim match {
    // check for RHS protein domain or LHS mutant spec: return protein portion only
    case HyphenatedNamePat(lhs, rhs) => if (isProteinDomain(rhs)) Seq(lhs.trim) else Seq(rhs.trim)
    case _ => NoCandidates                // signal failure
  }

  /** Check for suffixes matching pattern, return matched RHS or trimmed text. */
  def stripAllKeysSuffixes (text:String): String = text.trim match {
    case AllKeysSuffixPat(lhs) => lhs.trim
    case ttext:String => ttext
  }

  /** Return the portion of the text string minus one of the protein family postpositional
    * attributives, if found in the given text string, else return no candidates. */
  def stripFamilyPostAttributivesKT (text:String): KeyCandidates = {
    text.trim match {
      case UnderscoreFamilySuffixPat(ttext) => Seq(ttext)
      case FamilyPostAttributivePat(lhs) => Seq(lhs.trim)
      case _ => NoCandidates                // signal failure
    }
  }

  /** Remove affixes from given dash-separated key, return concatenated string of non-affixes. */
  def stripGeneNameAffixesKT (text:String): KeyCandidates = {
    if (text.contains("-")) {
      val stripped = text.split("-").filterNot(isGeneNameAffix(_))
      if (!stripped.isEmpty)
        Seq(stripped.mkString("-"))
      else Seq(text)
    }
    else NoCandidates
  }

  /** Return the portion of the text string before a trailing mutation phrase,
    * if found in the given text string, else return no candidates. */
  def stripMutantProteinKT (text:String): KeyCandidates = text.trim match {
    case PhosphorMutationPat(chunk) => Seq(chunk.trim)
    case TrailingMutationPat(lhs) => Seq(lhs.trim)
    case _ => NoCandidates                  // signal failure
  }

  /** Return the portion of the text string minus one of the organ-cell-type suffixes,
    * if found in the given text string, else return no candidates. */
  def stripOrganPostAttributivesKT (text:String): KeyCandidates = text.trim match {
    case OrganPostAttributivePat(lhs) => Seq(lhs.trim)
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

}


/** Trait Companion Object allows Mixin OR Import pattern. */
object ReachKBKeyTransforms extends ReachKBKeyTransforms {

  /** The set of suffixes to remove from all keys to create a lookup key. */
  val AllKeysSuffixPat = """(?i)(.*)(?:_human)""".r

  /** Match patterns to create a protein family lookup key. */
  val UnderscoreFamilySuffixPat = """(?i)(.*_family)""".r
  val FamilyPostAttributivePat =  """(?i)(.*?)(?: protein family|family)""".r

  /** Pattern matching 2 text strings separated by a hyphen, case insensitive. */
  val HyphenatedNamePat = """(?i)(\w+)-(\w+)""".r

  /** The set of characters to remove from the text to create a lookup key. */
  val KeyCharactersToRemove = " /-".toSet

  /** Match patterns to create an organ lookup key. */
  val OrganPostAttributivePat = "(?i)(.*?)(?: cells?| tissues?| fluids?)+".r

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

  /** Default list of additional mention transforms to apply during the KB's query phase. */
  val DefaultMentionKeyTransforms = Seq( canonicalMKT _ )


  /** List of text transforms to use with a case-sensitive KB. */
  val CasedKeyTransforms = Seq( identityKT _, lowercaseKT _, canonicalKT _ )

  /** List of additional mention transforms to use with a case-sensitive KB. */
  val CasedMentionKeyTransforms = Seq( identityMKT _, lowercaseMKT _, canonicalMKT _ )


  /** List of transform methods to apply for alternate Protein Family lookups. */
  val FamilyQueryKeyTransforms = Seq( stripFamilyPostAttributivesKT _ )

  /** List of transform methods to apply for alternate Organ lookups. */
  val OrganQueryKeyTransforms = Seq( stripOrganPostAttributivesKT _ )

  /** List of transform methods to apply for alternate Protein lookups. */
  val ProteinQueryKeyTransforms = Seq( stripProteinPostAttributivesKT _,
                                       stripMutantProteinKT _,
                                       stripGeneNameAffixesKT _,
                                       hyphenatedProteinKT _,
                                       stripPTMPrefixesKT _ )
}
