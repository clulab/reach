package edu.arizona.sista.reach.grounding2

/**
  * Methods for transforming text strings into potential keys for lookup in KBs.
  *   Written by Tom Hicks. 10/22/2015.
  *   Last Modified: Split off bio-specific methods leaving this base trait.
  */
trait KBKeyTransforms {

  /** Type alias for functions which take a text string and return a potential key string. */
  type KeyTransforms = Seq[(String) => String]


  /** Return a sequence of alternate keys, one for each of the given key transforms. */
  def makeAlternateKeys (key:String, transformFns:KeyTransforms): Seq[String] = {
    transformFns.map(_.apply(key)).filter(_ != key)
  }

  /** Try to remove one of the suffixes in the given set from the given text. */
  def stripASuffix (suffixes:Set[String], text:String): String = {
    var key = text
    suffixes.foreach { suffix =>
      key = key.stripSuffix(suffix)
    }
    return key
  }

}
