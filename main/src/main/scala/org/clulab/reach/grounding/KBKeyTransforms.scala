package org.clulab.reach.grounding

import org.clulab.reach.mentions._
import org.clulab.reach.grounding.KBKeyTransforms._

/**
  * Methods for transforming text strings into potential keys for lookup in KBs.
  *   Written by Tom Hicks. 10/22/2015.
  *   Last Modified: Add no transforms constant. Remove mention handling.
  */
trait KBKeyTransforms {

  /** Apply the given transform function to the given text, return any non-empty result strings. */
  def applyTransform (transformFn: KeyTransformFn, text: String): KeyCandidates =
    toKeyCandidates(transformFn.apply(text))

  /** Apply the given transform function to the given texts, return any non-empty result strings. */
  def applyTransform (transformFn: KeyTransformFn, texts: Seq[String]): KeyCandidates =
    toKeyCandidates(texts.flatMap(transformFn.apply(_)))

  /** Apply the given key transforms to the given string, returning a (possibly empty)
      sequence of potential key strings. */
  def applyAllTransforms (transformFns: KeyTransforms, text: String): KeyCandidates =
    toKeyCandidates(transformFns.flatMap(_.apply(text)))


  /** A key transform which implements an Identity function for Strings. */
  def identityKT (text:String): KeyCandidates = toKeyCandidates(text)

  /** A key transform which implements a minimal canonicalization function for Strings. */
  def lowercaseKT (text:String): KeyCandidates = toKeyCandidates(text.toLowerCase)


  /** Try to remove all of the suffixes in the given set from the given text. */
  def stripAllSuffixes (suffixes:Seq[String], text:String): String = {
    var done:Boolean = false
    var lastText = text.trim                // prepare for first round
    var modText = text.trim                 // remember text before stripping
    while (!done) {
      suffixes.foreach { suffix =>          // try all suffixes
        modText = modText.stripSuffix(suffix).trim
      }
      if (modText == lastText)              // if no suffixes were stripped in last round
        done = true                         // done: exit the loop
      else                                  // else try another round of stripping
        lastText = modText                  // update result from last round
    }
    modText                                 // return new or unchanged string
  }

  /** Try to remove all of the suffixes in the given set from the given text. */
  def stripAllSuffixesKT (suffixes:Seq[String], text:String): KeyCandidates =
    toKeyCandidates(stripAllSuffixes(suffixes, text))

  /** Transform the given string into (a possibly empty) key candidates. */
  def toKeyCandidates (text:String): KeyCandidates =
    if (text.trim.nonEmpty) Seq(text.trim) else NoCandidates

  /** Transform the given sequence of strings into (a possibly empty) key candidates. */
  def toKeyCandidates (candidates: Seq[String]): KeyCandidates =
    candidates.map(_.trim).filter(_.nonEmpty)
}


/** This object does NOT implement the selfless-trait pattern because the class is extended. */
object KBKeyTransforms {

  /** Type alias for a (possibly empty) sequence of key transform results. */
  type KeyCandidates = Seq[String]
  val NoCandidates = Seq.empty[String]

  /** Type alias for functions which take a text string and return a (possibly empty)
      list of potential key strings. */
  type KeyTransformFn = (String) => KeyCandidates
  type KeyTransforms = Seq[KeyTransformFn]
  val NoTransforms = Seq.empty[KeyTransformFn]

}
