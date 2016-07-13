package org.clulab.reach.grounding

import collection.mutable.{ HashMap, HashSet, Map, MultiMap, Set }

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBUtils._

/**
  * Class implementing an in-memory knowledge base indexed by key and species.
  *   Written by: Tom Hicks. 3/10/2016
  *   Last Modified: Add contains and lookup methods for ID strings.
  */
class ReverseLookupKB (
  /** The namespace for all IDs read from the KB data file. */
  namespace: String = DefaultNamespace
) {

  /** The root data structure implementing this in-memory lookup table. */
  val theKB = new HashMap[String, Set[String]] with MultiMap[String, String]

  /** Add the given entry to this KB, if the value is unique. */
  def addEntry (key:String, valu:String) = theKB.addBinding(key, valu)

  /** Tests whether this KB has a binding for the given key string. */
  def contains (key:String): Boolean = theKB.contains(key)

  /** Tests whether this KB has a binding for the given ID. */
  def containsId (id:String): Boolean =
    theKB.contains(makeNamespaceId(namespace, id))

  /** Return a species name set for the given key string. */
  def lookup (key:String): Option[collection.immutable.Set[String]] =
    theKB.get(key).map(_.toSet)

  /** Return a species name set for the given ID and namespace. */
  def lookupId (id:String): Option[collection.immutable.Set[String]] =
    lookup(makeNamespaceId(namespace, id))

  /** Try lookups for all given keys until one succeeds or all fail. */
  def lookups (allKeys:Seq[String]): Option[collection.immutable.Set[String]] = {
    allKeys.foreach { key =>
      val names = lookup(key)
      if (names.isDefined) return names
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
    // rlkb.theKB.foreach { case (k, entries) =>   // for DEBUGGING
    //   entries.foreach { ent => println(ent.toString()) }} // for DEBUGGING
    return rlkb
  }

  /**
    * Load this KB from the given 2-5 column, tab-separated-value (TSV) text file.
    *   1st column (0) is the text string,
    *   2nd column (1) is the ID string,
    *   3rd column (2) is the Species string (ignored),
    *   4th column (3) is the Namespace string (ignored),
    *   5th column (4) is the Type string (ignored).
    * If filename argument is null or the empty string, skip file loading.
    * The namespace for each entry is given as argument and any namespace column values are ignored.
    */
  private def loadFromKBDir (rlkb:ReverseLookupKB, filename:String, namespace:String) = {
    if ((filename != null) && !filename.trim.isEmpty) { // skip loading if filename missing
      val kbResourcePath = makePathInKBDir(filename)
      val source = sourceFromResource(kbResourcePath)
      source.getLines.map(tsvRowToFields(_)).filter(tsvValidateFields(_)).foreach { fields =>
        processFields(rlkb, fields, namespace)
      }
      source.close()
    }
  }

  /** Extract particular fields and process them as needed. */
  private def processFields (rlkb:ReverseLookupKB, fields:Seq[String], namespace:String): Unit = {
    val text = fields(0)
    val refId = fields(1)
    rlkb.addEntry(makeNamespaceId(namespace, refId), text) // store new entry w/ reversed key/value
  }

  /** Check for required fields in one row of a TSV input file. */
  private def tsvValidateFields (fields:Seq[String]): Boolean = {
    ((fields.size >= 2) && fields(0).nonEmpty && fields(1).nonEmpty)
  }

}
