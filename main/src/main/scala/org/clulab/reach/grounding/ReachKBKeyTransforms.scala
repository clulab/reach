package org.clulab.reach.grounding

import org.clulab.reach.mentions._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * REACH-related methods for transforming mentions and text strings into potential keys
  * for lookup in KBs.
  *   Written by Tom Hicks. 11/10/2015.
  *   Last Modified: Expand default transforms.
  */
trait ReachKBKeyTransforms extends KBKeyTransforms {

  /** A key transform which implements a canonicalization function for Strings. */
  def canonicalKT (text:String): KeyCandidates = {
    var key:String = text.toLowerCase
    // KeyStopWords.foreach { word => key = key.replaceAll(word, "") }
    key = key.filterNot(KeyCharactersToRemove)
    stripAllSuffixesKT(AllKeysStopSuffixes, key)
  }

  /** A key transform which implements a canonicalization function for Mentions. */
  def canonicalMKT (mention:BioTextBoundMention): KeyCandidates = canonicalKT(mention.text)


  /** Check for one of several types of hyphen-separated strings and, if found,
    * extract and return the candidate text portion, else return the text unchanged. */
  def hyphenatedProteinKey (text:String): KeyCandidates = {
    return text match {
      // check for RHS protein domain or LHS mutant spec: return protein portion only
      case HyphenatedNamePat(lhs, rhs) => if (isProteinDomain(rhs)) Seq(lhs) else Seq(rhs)
      case _ => NoCandidates                // signal failure
    }
  }

  /** Return the portion of the text string minus one of the protein family postpositional
    * attributives, if found in the given text string, else return the text lowercased. */
  def stripFamilyPostAttributives (text:String): KeyCandidates = {
    val lcText = text.toLowerCase           // match lower cased text only
    stripAllSuffixesKT(FamilyPostAttributives, lcText)
  }

  /** Remove affixes from given dash-separated key, return concatenated string of non-affixes. */
  def stripGeneNameAffixes (text:String): KeyCandidates = {
    if (text.contains("-")) {
      val stripped = text.split("-").filterNot(isGeneNameAffix(_))
      if (!stripped.isEmpty)
        Seq(stripped.mkString("-"))
      else Seq(text)
    }
    else NoCandidates
  }

  /** Return the portion of the text string before a trailing mutation phrase,
    * if found in the given text string, else return the text unchanged. */
  def stripMutantProtein (text:String): KeyCandidates = {
    return text match {
      case PhosphorMutationPat(lhs) => Seq(lhs)
      case TrailingMutationPat(lhs) => Seq(lhs)
      case _ => NoCandidates                // signal failure
    }
  }

  /** Return the portion of the text string minus one of the organ postpositional
    * attributives, if found in the given text string, else return the text. */
  def stripOrganPostAttributives (text:String): KeyCandidates = {
    val lcText = text.toLowerCase           // match lower cased text only
    stripAllSuffixesKT(OrganPostAttributives, lcText)
  }

  /** Return the portion of the text string minus one of the organ-cell-type suffixes,
    * if found in the given text string, else return the text unchanged. */
  def stripOrganSuffixes (text:String): KeyCandidates = {
    return text match {
      case OrganPostAttributivePat(lhs, _) => Seq(lhs)
      case _ => NoCandidates                // signal failure
    }
  }

  /** Return the portion of the text string minus one of the protein postpositional
    * attributives, if found in the given text string, else return the text lowercased. */
  def stripProteinPostAttributives (text:String): KeyCandidates = {
    val lcText = text.toLowerCase           // match lower cased text only
    stripAllSuffixesKT(ProteinPostAttributives, lcText)
  }

  /** Return the portion of the text string minus any of the PTM-related prefixes, if found
    * in the given text string, else return the text unchanged. */
  def stripPTMPrefixes (text:String): KeyCandidates = {
    return text match {
      case PTMPrefixPat(prefix, restOfKey) => Seq(restOfKey)
      case _ => NoCandidates                // signal failure
    }
  }


  /** Canonicalize the given text string into a key for both storage and lookup. */
  def makeCanonicalKey (text:String): String = {
    var key:String = text.toLowerCase
    // KeyStopWords.foreach { word => key = key.replaceAll(word, "") }
    key = key.filterNot(KeyCharactersToRemove)
    stripAllSuffixes(AllKeysStopSuffixes, key).getOrElse("")
  }

}


/** Trait Companion Object allows Mixin OR Import pattern. */
object ReachKBKeyTransforms extends ReachKBKeyTransforms {

  /** The set of words to remove from all keys to create a lookup key. */
  val AllKeysStopSuffixes = Seq("_human")

  /** The set of words to remove from a key to create a protein family lookup key. */
  val FamilyPostAttributives = Seq(" protein family", " family")
  val FamilyPostAttributivePat = """(?i)(.*)((protein family)?|family?)""".r

  /** Pattern matching 2 text strings separated by a hyphen, case insensitive. */
  val HyphenatedNamePat = """(?i)(\w+)-(\w+)""".r

  /** The set of characters to remove from the text to create a lookup key. */
  val KeyCharactersToRemove = " /-".toSet

  /** Trailing context strings for organ phrases, case insensitive. */
  val OrganPostAttributives = Seq(" cell", " cells", " tissue", " tissues", " fluid", " fluids")
  val OrganPostAttributivePat = """(?i)(.*)(cells?|tissues?|fluids?)""".r

  /** Match protein names beginning with special PTM-related prefix characters. */
  val PTMPrefixPat = """(p|u)([A-Z0-9_-][A-Za-z0-9_-]*)""".r

  /** Match phosphorylation mutation phrases, case insensitive. */
  val PhosphorMutationPat = """(?i)phosphorylated\s+(.*)\s+\w+\s+mutant""".r

  /** The set of words to remove from a key to create a protein lookup key. */
  val ProteinPostAttributives = Seq(" mutant protein", " protein")
  val ProteinPostAttributivePat = """(?i)(.*)((mutant protein)?|protein?)""".r

  /** Match mutation string at end of text string, case insensitive. */
  val TrailingMutationPat = """(?i)(.*)\s+\w+\s+mutant""".r


  /** List of default transforms to apply during the KB's entry creation phase. */
  // val DefaultAddKeyTransforms = Seq( identityKT _, canonicalKT _ )
  val DefaultAddKeyTransforms = Seq( identityKT _, lowercaseKT _, canonicalKT _ )

  /** List of default transforms to apply in the absence of specific transform arguments. */
  // val DefaultQueryKeyTransforms = Seq( identityKT _, canonicalKT _ )
  val DefaultQueryKeyTransforms = Seq( identityKT _, lowercaseKT _, canonicalKT _ )

  /** List of default mention transforms to apply in the absence of specific transform arguments. */
  // val DefaultMentionKeyTransforms = Seq( identityMKT _, canonicalMKT _ )
  val DefaultMentionKeyTransforms = Seq( identityMKT _, lowercaseMKT _, canonicalMKT _ )


  /** List of transform methods to apply for alternate Protein Family lookups. */
  val FamilyKeyTransforms = Seq( stripFamilyPostAttributives _ )

  /** List of transform methods to apply for alternate Organ lookups. */
  val OrganKeyTransforms = Seq( stripOrganPostAttributives _ )

  /** List of transform methods to apply for alternate Protein lookups. */
  val ProteinKeyTransforms = Seq( stripProteinPostAttributives _,
                                  stripMutantProtein _,
                                  stripGeneNameAffixes _,
                                  hyphenatedProteinKey _,
                                  stripPTMPrefixes _ )

  /** Set of gene name affix strings extracted from the Sorger bioentities file. */
  val GeneNameAffixes: Set[String] =
    ReachKBUtils.readLines(GeneNameAffixesFilename)
                .map(affix => makeCanonicalKey(affix.trim)).toSet

  /** Tell whether the given string names a gene name affix or not. */
  def isGeneNameAffix (affix: String): Boolean =
    GeneNameAffixes.contains(makeCanonicalKey(affix))


  /** Set of short protein domain strings. */
  val ProteinDomainShortNames: Set[String] =
    ReachKBUtils.readLines(ProteinDomainShortNamesFilename)
                .map(suffix => makeCanonicalKey(suffix.trim)).toSet

  /** Tell whether the given string names a protein domain or not. */
  def isProteinDomain (domain: String): Boolean =
    ProteinDomainShortNames.contains(makeCanonicalKey(domain))

}
