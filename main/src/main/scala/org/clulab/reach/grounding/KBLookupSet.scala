package org.clulab.reach.grounding

import collection.mutable.{ HashSet, Set }

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachKBUtils._

/**
  * A class to create/manipulate
  *   Written by: Tom Hicks. 1/11/2017.
  *   Last Modified: Initial creation.
  */
class KBLookupSet (

  /** Configuration record containing KB key transforms for this lookup set. */
  val keyTransforms: IMKBKeyTransforms = new IMKBKeyTransforms()

)  {

  /** The root data structure implementing this lookup set. */
  val theSet = new HashSet[String]()

  def addEntries (entries: Seq[String]): Unit = {
    entries.foreach { entry =>
      theSet ++= applyAllTransforms(keyTransforms.additionKTs, entry)
    }
  }

  def contains (text: String): Boolean = {
    true                                    // TODO: IMPLEMENT LATER
  }

  override def toString: String =
    s"<KBLookupSet: #keys=${theSet.size}, KTs=${keyTransforms.toString}>"
}


class KBLookupSetFactory {

  def makeFromFile (
    filename: String,
    keyTransforms: IMKBKeyTransforms = new IMKBKeyTransforms()
  ): KBLookupSet = {
    val kbls = new KBLookupSet(keyTransforms)
    kbls.addEntries(ReachKBUtils.readLines(filename))
    kbls
  }

}


object KBLookupSet {

  val kblsFactory = new KBLookupSetFactory

  val GeneNameAffixes: KBLookupSet = kblsFactory.makeFromFile(GeneNameAffixesFilename)

  /** Tell whether the given string names a gene name affix or not. */
  def isGeneNameAffix (affix: String): Boolean = GeneNameAffixes.contains(affix)

  // val GeneNameAffixes: Set[String] =
  //   ReachKBUtils.readLines(GeneNameAffixesFilename)
  //               .map(affix => makeCanonicalKey(affix.trim)).toSet

}
