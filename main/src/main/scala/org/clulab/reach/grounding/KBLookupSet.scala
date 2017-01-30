package org.clulab.reach.grounding

import collection.mutable.{ HashSet, Set }

import org.clulab.reach.grounding.KBKeyTransforms._
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._
import org.clulab.reach.grounding.ReachKBUtils._

/**
  * A class to create/manipulate
  *   Written by: Tom Hicks. 1/11/2017.
  *   Last Modified: Refactor protein kinase lookup set here.
  */
class KBLookupSet (

  /** Key transforms used to create and lookup set items. */
  val baseKTs: KeyTransforms = DefaultKeyTransforms

)  {

  /** The root data structure implementing this lookup set. */
  val theSet = new HashSet[String]()

  /** Use key transforms to add one or more set items for each of the given text strings. */
  def addEntries (texts: Seq[String]): Unit = texts.foreach { text =>
    theSet ++= applyAllTransforms(baseKTs, text.trim)
  }

  /** Tell whether the set contains a member item derivable from the given text string. */
  def contains (text: String): Boolean = {
    baseKTs.foreach { ktFn =>                 // try each transform in order
      ktFn.apply(text.trim).foreach { item => // generate and try candidate items
        if (theSet.contains(item))            // if item is found in set
          return true                         // signal success and exit out
      }
    }
    false                                     // no derivable items found
  }

  /** Print a textual representation of the set contents to standard output. */
  def dump: Unit = theSet.toSeq.sorted.foreach { println(_) }

  /** Return the number of items in this set. */
  def size: Int = theSet.size

  /** Return a string representation of this set for logging and debugging. */
  override def toString: String =
    s"<KBLookupSet: #keys=${theSet.size}, baseKTs=${baseKTs}>"
}


/** Companion object to define useful constants and lookup sets. */
object KBLookupSet {

  /** Create and return a KBLookupSet by reading and transforming lines from the given file. */
  def apply (
    filename: String,
    keyTransforms: KeyTransforms = DefaultKeyTransforms
  ): KBLookupSet = {
    val kbls = new KBLookupSet(keyTransforms)
    kbls.addEntries(ReachKBUtils.readLines(filename))
    kbls
  }

  /** Set of gene name prefix strings extracted from the affixes of the Sorger bioentities file. */
  val GeneNamePrefixes: KBLookupSet = {
    val gnaTransforms = Seq( identityKT _, lowercaseKT _ )
    KBLookupSet(GeneNameAffixesFilename, gnaTransforms)
  }

  /** Tell whether the given string names a gene name prefix or not. */
  def isGeneNamePrefix (prefix: String): Boolean = GeneNamePrefixes.contains(prefix)


  /** Set of short protein domain strings. */
  val ProteinDomainShortNames: KBLookupSet =
    KBLookupSet(ProteinDomainShortNamesFilename, CasedKeyTransforms)

  /** Tell whether the given string names a protein domain or not. */
  def isProteinDomain (domain: String): Boolean = ProteinDomainShortNames.contains(domain)


  /** Set of protein kinase names. */
  val ProteinKinaseIds: KBLookupSet = KBLookupSet(ProteinKinasesFilename, IdentityKeyTransforms)

  /** Tell whether the given ID string names a protein kinase or not. */
  def isProteinKinase (id: String): Boolean = ProteinKinaseIds.contains(id)

}
