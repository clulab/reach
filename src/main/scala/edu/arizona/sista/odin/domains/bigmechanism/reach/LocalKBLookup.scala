package edu.arizona.sista.odin.domains.bigmechanism.reach

import scala.io.Source

/**
  * A collection of classes which implement project internal knowledge base lookups.
  *   Author: by Tom Hicks. 5/18/2015.
  *   Last Modified: Redo KB lookups for manual and generated KB files.
  */

/**
  * Abstract class which reads a 2 or 3-column, tab-separated-value (TSV) text file
  * where: 1st column is the name string, 2nd column is the ID string (2-col file) or
  *  species (3-col file), and 3rd column is the ID string (3-col file).
  */
abstract class LocalKBLookup extends SpeciatedKBLookup with KnowledgeBaseConstants {
  def baseURI = "http://edu.arizona.sista.odin/uazid/"
  def namespace = "uazid"
  def resourceID = "MIR:00000000"           // mock MIRIAM registration number

  protected val theKB = scala.collection.mutable.Map[String, Map[String,String]]()

  def getLookupKey (key:String): String = {
    return LocalKBUtils.makeKBCanonKey(key) // canonicalize text for KBs
  }

  override def resolve (key:String): Option[String] = {
    val lookupKey = getLookupKey(key)       // make a lookup key from the given key
    val entry = theKB.get(lookupKey)        // look for existing entry
    return entry.flatMap(_.get("referenceID")) // return lookup value
  }

  override def resolveBySpecies (key:String, species:Set[String]): Option[String] = {
    val lookupKey = getLookupKey(key)       // make a lookup key from the given key
    val entry = theKB.get(lookupKey)        // look for existing entry
    if (entry.isDefined) {                  // if an entry is found
      val propsMap = entry.get              // get the properties map
      if (propsMap.contains("species")) {   // if properties map even has species
        val sp = propsMap("species")        // check for correct species
        return if (species.contains(sp)) propsMap.get("referenceID") else None
      }
      else                                  // no species: return lookup value
        return propsMap.get("referenceID")
    }
    else return None                        // else entry not found, so signal lookup failure
  }


  private def convertToFields (line:String): Seq[String] = {
    return line.split("\t").map(_.trim)
  }

  protected def validateFields (fields:Seq[String]): Boolean = {
    return ((fields.size == 3) && fields(0).nonEmpty && fields(2).nonEmpty) ||
           ((fields.size == 2) && fields(0).nonEmpty && fields(1).nonEmpty)
  }

  protected def readAndFillKB (kbResourcePath:String) = {
    val source: Source = LocalKBUtils.sourceFromResource(kbResourcePath)
    source.getLines.map(convertToFields(_)).filter(validateFields(_)).foreach { fields =>
      var text = ""
      var species = ""
      var refId = ""

      if (fields.size == 3) {               // with species
        text = fields(0)
        species = fields(1)
        refId = fields(2)
      }
      else if (fields.size == 2) {          // w/o species
        text = fields(0)
        refId = fields(1)
      }
      else                                  // should never happen if validation works
        println(s"BAD INPUT: missing required fields: ${fields}")

      // make new key and entry for the KB:
      val storageKey = LocalKBUtils.makeKBCanonKey(text) // make canonical storage key
      val newEntry =
        if (species == "")
          Map("referenceID" -> refId, "namespace" -> namespace,
              "baseURI" -> baseURI, "resourceID" -> resourceID,
              "key" -> storageKey, "text" -> text) // also return original text
        else
          Map("referenceID" -> refId, "namespace" -> namespace,
              "baseURI" -> baseURI, "resourceID" -> resourceID,
              "key" -> storageKey, "species" -> species,
              "text" -> text)               // also return original text

      val entry = theKB.get(storageKey)     // look for existing entry
      if (entry.isDefined) {                // if entry is already in this KB
        if (!LocalKBUtils.isHumanSpecies(entry.get.getOrElse("species",""))) // if not human
          theKB(storageKey) = newEntry      // then overwrite it with new entry
        // else ignore it: do not overwrite human entry with any other
      }
      else                                  // key not seen before
        theKB(storageKey) = newEntry
    }
    source.close()
  }
}


/** KB lookup to resolve subcellular location names via static KBs. */
class StaticCellLocationKBLookup extends LocalKBLookup {
  override def baseURI = "http://identifiers.org/go/"
  override def namespace = "go"
  override def resourceID = "MIR:00000022"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticCellLocationFilename))
}


/** KB lookup to resolve small molecule (metabolite) names via static KBs. */
class StaticMetaboliteKBLookup extends LocalKBLookup {
  override def baseURI = "http://identifiers.org/hmdb/"
  override def namespace = "hmdb"
  override def resourceID = "MIR:00000051"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticMetaboliteFilename))
}

/** KB lookup to resolve small molecule (chemical) names via static KBs. */
class StaticChemicalKBLookup extends LocalKBLookup {
  override def baseURI = "http://identifiers.org/chebi/"
  override def namespace = "chebi"
  override def resourceID = "MIR:00100009"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticChemicalFilename))
}


/** KB accessor to resolve protein names via static KBs. */
class StaticProteinKBLookup extends LocalKBLookup {
  override def baseURI = "http://identifiers.org/uniprot/"
  override def namespace = "uniprot"
  override def resourceID = "MIR:00100164"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticProteinFilename))
}


/** KB lookup to resolve protein family names via static KBs. */
class StaticProteinFamilyKBLookup extends LocalKBLookup {
  override def baseURI = "http://identifiers.org/interpro/"
  override def namespace = "interpro"
  override def resourceID = "MIR:00000011"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticProteinFamilyFilename))
}


/** KB lookup to resolve tissue type names via static KBs. */
class StaticTissueTypeKBLookup extends LocalKBLookup {
  override def baseURI = "http://identifiers.org/uniprot/"
  override def namespace = "uniprot"
  override def resourceID = "MIR:00000005"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticTissueTypeFilename))
}
