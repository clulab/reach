package org.clulab.reach.grounding

import collection.mutable.{ HashMap, HashSet, Map, MultiMap, Set }

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBUtils._

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 3/10/2016
  *   Last Modified: Return immutable sets from lookup.
  */
class ReverseLookupKB (
  /** The namespace for all IDs read from the KB data file. */
  namespace: String = DefaultNamespace
) {

  /** The root data structure implementing this in-memory lookup table. */
  val theKB = new HashMap[String, Set[String]] with MultiMap[String, String]

  /** Add the given entry to this KB, if it is unique. */
  def addEntry (key:String, valu:String) = theKB.addBinding(key, valu)

  /** Return a species name set for the given key. */
  def lookup (key:String): Option[collection.immutable.Set[String]] = theKB.get(key).map(_.toSet)

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookups (allKeys:Seq[String]): Option[collection.immutable.Set[String]] = {
    allKeys.foreach { key =>
      val spNames = lookup(key)
      if (spNames.isDefined) return spNames
    }
    return None                             // tried all keys: no success
  }

}


/**
  * Factory class for creating and loading a ReverseLookupKB from a TSV file.
  */
class RLKBFactory {

  /** Main factory method to create, fill, and return an encapsulate knowledge base. */
  def make (
    namespace: String = DefaultNamespace,
    kbFilename: String = ""
  ): ReverseLookupKB = {
    val rlkb: ReverseLookupKB = new ReverseLookupKB(namespace)
    if (kbFilename != "")
      loadFromKBDir(rlkb, kbFilename, namespace) // load reverse lookup KB data
    return rlkb
  }

  private def loadFromKBDir (rlkb:ReverseLookupKB, filename:String, namespace:String) = {
    if ((filename != null) && !filename.trim.isEmpty) { // skip loading if filename missing
      val kbResourcePath = makePathInKBDir(filename)
      val source = sourceFromResource(kbResourcePath)
      source.getLines.map(tsvRowToFields(_)).filter(tsvValidateFields(_)).foreach { flds =>
        rlkb.addEntry(makeNamespaceId(namespace, flds(1)), flds(0)) // store new entry
      }
      source.close()
    }
  }

}
