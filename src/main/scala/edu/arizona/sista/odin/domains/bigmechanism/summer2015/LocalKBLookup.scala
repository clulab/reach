package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import scala.io.Source

/**
  * A collection of classes which implement project internal knowledge base lookups.
  *   Author: by Tom Hicks. 5/18/2015.
  *   Last Modified: Sync local KB accessor & lookup.
  */

/**
  * Abstract class which reads a 2 or 3-column, tab-separated-value (TSV) text file
  * where: 1st column is the name string, 2nd column is the ID string (2-col file) or
  *  species (3-col file), and 3rd column is the ID string (3-col file).
  */
abstract class LocalKBLookup extends SpeciatedKBLookup {
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


/** KB lookup to resolve protein names in mentions. */
class AzProteinKBLookup extends LocalKBLookup {
  def baseURI = "http://identifiers.org/uniprot/"
  def namespace = "uniprot"
  def resourceID = "MIR:00100164"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/kb/uniprot-proteins.tsv.gz")
}


/** KB lookup to resolve protein family names in mentions. */
class AzProteinFamilyKBLookup extends LocalKBLookup {
  def baseURI = "http://identifiers.org/interpro/"
  def namespace = "interpro"
  def resourceID = "MIR:00000011"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/kb/ProteinFamilies.tsv.gz")
}


/** KB lookup to resolve small molecule (metabolite) names in mentions. */
class AzSmallMoleculeKBLookup extends LocalKBLookup {
  def baseURI = "http://identifiers.org/hmdb/"
  def namespace = "hmdb"
  def resourceID = "MIR:00000051"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/kb/hmdb.tsv.gz")
}

/** KB lookup to resolve small molecule (chemical) names in mentions. */
class AzSmallMoleculeKBLookup2 extends LocalKBLookup {
  def baseURI = "http://identifiers.org/chebi/"
  def namespace = "chebi"
  def resourceID = "MIR:00100009"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/kb/chebi.tsv.gz")
}


/** KB lookup to resolve subcellular location names in mentions using GeneOntology DB. */
class AzSubcellularLocationKBLookup extends LocalKBLookup {
  def baseURI = "http://identifiers.org/go/"
  def namespace = "go"
  def resourceID = "MIR:00000022"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/kb/GO-subcellular-locations.tsv")
}

/** KB lookup to resolve tissue type names in mentions. */
class AzTissueTypeKBLookup extends LocalKBLookup {
  def baseURI = "http://identifiers.org/uniprot/"
  def namespace = "uniprot"
  def resourceID = "MIR:00000005"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/kb/tissue-type.tsv")
}
