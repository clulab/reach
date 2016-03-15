package edu.arizona.sista.reach.grounding

/**
  * Methods for transforming text strings into potential keys for lookup in KBs.
  *   Written by Tom Hicks. 10/22/2015.
  *   Last Modified: Update for case-insensitive transforms.
  */
trait KBKeyTransforms {

  /** Type alias for functions which take a text string and return a potential key string. */
  type KeyTransforms = Seq[(String) => String]


  /** Return a sequence of alternate keys, one for each of the given key transforms. */
  def applyTransforms (text:String, transformFns:KeyTransforms): Seq[String] = {
    transformFns.map(_.apply(text)).filter{ str => (str != text) && (str != text.toLowerCase) }
  }

  /** Try to remove all of the suffixes in the given set from the given text. */
  def stripSuffixes (suffixes:Seq[String], text:String): String = {
    var modText = text
    suffixes.foreach { suffix =>
      modText = modText.stripSuffix(suffix)
    }
    if (modText == text)                    // if no suffixes were stripped
      return text                           // then return the unaltered text
    else                                    // else at least one suffix was stripped
      return stripSuffixes(suffixes, modText) // so try another round of stripping
  }

}
