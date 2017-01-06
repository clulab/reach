package org.clulab.reach.grounding

import org.clulab.reach.mentions._
import org.clulab.reach.grounding.KBKeyTransforms._

/**
  * Methods for transforming text strings into potential keys for lookup in KBs.
  *   Written by Tom Hicks. 10/22/2015.
  *   Last Modified: Reverse args to apply all transforms.
  */
trait KBKeyTransforms {

  /** Type alias for a (possibly empty) sequence of key transform results. */
  type KeyCandidates = Seq[String]
  val NoCandidates = Seq.empty[String]

  /** Type alias for functions which take a text string and return a (possibly empty)
      list of potential key strings. */
  type KeyTransformFn = (String) => KeyCandidates
  type KeyTransforms = Seq[KeyTransformFn]

  /** Type alias for functions which take a mention and return a (possibly empty)
      list of potential key strings. */
  type MentionKeyTransformFn = (BioTextBoundMention) => KeyCandidates
  type MentionKeyTransforms = Seq[MentionKeyTransformFn]


  /** Apply the given key transforms to the given string, returning a (possibly empty)
      sequence of potential key strings. */
  def applyAllTransforms (transformFns: KeyTransforms, text: String): KeyCandidates =
    transformFns.flatMap(_.apply(text))

  /** Apply the given mention key transforms to the given mention, returning
      a (possibly empty) sequence of potential key strings. */
  def applyAllTransforms (
    transformFns: MentionKeyTransforms,
    mention: BioTextBoundMention
  ): KeyCandidates = {
    transformFns.flatMap(_.apply(mention))
  }


  /** A key transform which implements an Identity function for Strings. */
  def identityKT (text:String): KeyCandidates = Seq(text)

  /** A key transform which implements a minimal transform function for Mentions. */
  def identityMKT (mention:BioTextBoundMention): KeyCandidates = Seq(mention.text)


  /** Try to remove all of the suffixes in the given set from the given text. */
  def stripAllSuffixes (suffixes:Seq[String], text:String): Option[String] = {
    var done:Boolean = false
    var lastText = text                     // prepare for first round
    var modText = text                      // remember text before stripping
    while (!done) {
      suffixes.foreach { suffix =>          // try all suffixes
        modText = modText.stripSuffix(suffix)
      }
      if (modText == lastText)              // if no suffixes were stripped in last round
        done = true                         // done: exit the loop
      else                                  // else try another round of stripping
        lastText = modText                  // update result from last round
    }
    if (modText.trim.equals(""))            // if no stem text left at all
      return None                           // then signal failure
    else if (modText == text)               // else if no suffixes were stripped at all
      return Some(text)                     // return unchanged text
    else                                    // else something was stripped
      return Some(modText)                  // so return the new string
  }

  /** Try to remove all of the suffixes in the given set from the given text. */
  def stripAllSuffixesKT (suffixes:Seq[String], text:String): KeyCandidates = {
    val stripped = stripAllSuffixes(suffixes, text)
    if (stripped.isDefined) Seq(stripped.get) else NoCandidates
  }

  /** Try to remove all of the suffixes in the given set from the given mention text. */
  def stripAllSuffixesMKT (suffixes:Seq[String], mention:BioTextBoundMention): KeyCandidates =
    stripAllSuffixesKT(suffixes, mention.text)

}


/** Trait Companion Object allows Mixin OR Import pattern. */
object KBKeyTransforms extends KBKeyTransforms { }
