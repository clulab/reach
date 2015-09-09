package edu.arizona.sista.odin.domains.bigmechanism.reach

import scala.io.Source

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.extern.inward._

/**
  * A collection of classes which implement project internal knowledge base accessors.
  *   Written by Tom Hicks. 4/10/2015.
  *   Last Modified: Redo KB accessors for manual and generated KB files.
  */

/**
  * Abstract class which reads a 2 or 3-column, tab-separated-value (TSV) text file
  * where: 1st column is the name string, 2nd column is the ID string (2-col file) or
  *  species (3-col file), and 3rd column is the ID string (3-col file).
  */
abstract class LocalKBAccessor extends SpeciatedKBAccessor with KnowledgeBaseConstants {
  def baseURI = "http://edu.arizona.sista.odin/uazid/"
  def namespace = "uazid"
  def resourceID = "MIR:00000000"           // mock MIRIAM registration number

  protected val theKB = scala.collection.mutable.Map[String, Map[String,String]]()

  override def getLookupKey (mention:Mention): String = {
    return LocalKBUtils.makeKBCanonKey(mention.text)   // canonicalize text for KBs
  }

  override def resolve (mention:Mention): Map[String,String] = {
    val key = getLookupKey(mention)         // make a key from the mention
    theKB.getOrElse(key, Map.empty)         // return properties map or signal lookup failure
  }

  override def resolveBySpecies (mention:Mention, species:Set[String]): Map[String,String] = {
    val key = getLookupKey(mention)         // make a key from the mention
    val entry = theKB.get(key)              // look for existing entry
    if (entry.isDefined) {                  // if an entry is found
      val propsMap = entry.get              // get the properties map
      if (species.contains(propsMap.getOrElse("species",""))) // if present and a desired species
        return propsMap                     // return the properties map
      else
        return Map.empty                    // else signal lookup failure
    }
    else                                    // else entry not found
      return Map.empty                      // so signal lookup failure
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


  /** For each of the given key transforms, try the KB lookup in sequence. Return the optioned
    * result from the first lookup which resolves a transformed key, or None otherwise. */
  def tryAlternateKeys (key:String,
                        transformFns:Seq[(String) => String]): Option[Map[String,String]] = {
    transformFns.foreach { xFormFN =>
      val xfKey = xFormFN.apply(key)        // create new, transformed key
      if (xfKey != key) {                   // if new key is really different
        val resInfo = theKB.get(xfKey)      // lookup new key in the KB
        if (resInfo.isDefined)
          return resInfo                    // return info for first matching key
      }
    }
    return None                             // else signal lookup failure
  }

}


//
// Subcellular Location Accessors
//

/** KB accessor to resolve subcellular location names via KBs generated from the BioPax model. */
class GendCellLocationKBAccessor extends LocalKBAccessor {
  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(GendCellLocationFilename))
}

/** KB accessor to resolve subcellular location names via manually maintained KBs. */
class ManualCellLocationKBAccessor extends LocalKBAccessor {
  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(ManualCellLocationFilename))
}

/** KB accessor to resolve subcellular location names via static KBs. */
class StaticCellLocationKBAccessor extends LocalKBAccessor {
  override def baseURI = "http://identifiers.org/go/"
  override def namespace = "go"
  override def resourceID = "MIR:00000022"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticCellLocationFilename))
}

/** KB accessor to resolve subcellular location names via static KBs. */
// class StaticCellLocationKBAccessor2 extends LocalKBAccessor {
//   def baseURI = "http://identifiers.org/uniprot/"
//   def namespace = "uniprot"
//   def resourceID = "MIR:00000005"

//   // MAIN: load KB to initialize class
//   readAndFillKB(LocalKBUtils.makePathInKBDir("uniprot-subcellular-locations.tsv"))
// }


//
// Small Molecule (Chemical and Metabolite) Accessors
//

/** KB accessor to resolve small molecule (chemical) names via KBs generated from the BioPax model. */
class GendChemicalKBAccessor extends LocalKBAccessor {
  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(GendChemicalFilename))
}

/** KB accessor to resolve small molecule (chemical) names via manually maintained KBs. */
class ManualChemicalKBAccessor extends LocalKBAccessor {
  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(ManualChemicalFilename))
}

/** KB accessor to resolve small molecule (metabolite) names via static KBs. */
class StaticMetaboliteKBAccessor extends LocalKBAccessor {
  override def baseURI = "http://identifiers.org/hmdb/"
  override def namespace = "hmdb"
  override def resourceID = "MIR:00000051"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticMetaboliteFilename))
}

/** KB accessor to resolve small molecule (chemical) names via static KBs. */
class StaticChemicalKBAccessor extends LocalKBAccessor {
  override def baseURI = "http://identifiers.org/chebi/"
  override def namespace = "chebi"
  override def resourceID = "MIR:00100009"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticChemicalFilename))
}


//
// Protein Accessors
//

/** Base KB accessor to resolve protein names in mentions. */
class LocalProteinKBAccessor extends LocalKBAccessor {
  /** Overridden to perform alternate key lookups. */
  override def resolve (mention:Mention): Map[String,String] = {
    val key = getLookupKey(mention)         // make a key from the mention
    val props = theKB.get(key)              // lookup key
    return if (props.isDefined) props.get   // find it or try alternate keys
           else tryAlternateKeys(key, LocalKeyTransforms.proteinKeyTransforms).getOrElse(Map.empty)
  }
}

/** KB accessor to resolve protein names via KBs generated from the BioPax model. */
class GendProteinKBAccessor extends LocalProteinKBAccessor {
  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(GendProteinFilename))
}

/** KB accessor to resolve protein names via manually maintained KBs. */
class ManualProteinKBAccessor extends LocalProteinKBAccessor {
  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(ManualProteinFilename))
}

/** KB accessor to resolve protein names via static KBs. */
class StaticProteinKBAccessor extends LocalProteinKBAccessor {
  override def baseURI = "http://identifiers.org/uniprot/"
  override def namespace = "uniprot"
  override def resourceID = "MIR:00100164"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticProteinFilename))
}


//
// Protein Family Accessors
//

/** Base KB accessor to resolve protein family names in mentions. */
class LocalProteinFamilyKBAccessor extends LocalKBAccessor {
  /** Override to perform alternate key lookups. */
  override def resolve (mention:Mention): Map[String,String] = {
    val key = getLookupKey(mention)         // make a key from the mention
    val props = theKB.get(key)              // lookup key
    return if (props.isDefined) props.get   // find it or try alternate keys
           else tryAlternateKeys(key, LocalKeyTransforms.proteinKeyTransforms).getOrElse(Map.empty)
  }

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir("ProteinFamilies.tsv.gz"))
}

/** KB accessor to resolve protein family names via KBs generated from the BioPax model. */
class GendProteinFamilyKBAccessor extends LocalProteinFamilyKBAccessor {
  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(GendProteinFilename))
}

/** KB accessor to resolve protein names via manually maintained KBs. */
class ManualProteinFamilyKBAccessor extends LocalProteinFamilyKBAccessor {
  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(ManualProteinFamilyFilename))
}

/** KB accessor to resolve protein family names via static KBs. */
class StaticProteinFamilyKBAccessor extends LocalProteinFamilyKBAccessor {
  override def baseURI = "http://identifiers.org/interpro/"
  override def namespace = "interpro"
  override def resourceID = "MIR:00000011"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticProteinFamilyFilename))
}


//
// Tissue Type Accessor
//

/** KB accessor to resolve tissue type names via static KBs. */
class StaticTissueTypeKBAccessor extends LocalKBAccessor {
  override def baseURI = "http://identifiers.org/uniprot/"
  override def namespace = "uniprot"
  override def resourceID = "MIR:00000005"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(StaticTissueTypeFilename))
}


//
// Failsafe Accessor
//

/** KB accessor implementation which always resolves each mention with a local, fake ID. */
class AzFailsafeKBAccessor extends SpeciatedKBAccessor {
  def baseURI = "http://edu.arizona.sista.odin/uazid/"
  def namespace = "uazid"
  def resourceID = "MIR:00000000"           // mock MIRIAM registration number

  private val idCntr = new IncrementingCounter() // counter sequence class
  private val seenIt = scala.collection.mutable.Map[String, Map[String,String]]()

  override def resolve (mention:Mention): Map[String,String] = {
    val key = getLookupKey(mention)         // make a key from the mention
    seenIt.getOrElseUpdate(key, newResolution(key))  // return existing entry or new one
  }

  // override of resolveBySpecies not necessary since trait default is to use resolve anyway

  // override def resolveBySpecies (mention:Mention, species:Set[String]): Map[String,String] = {
  //   return resolve(mention)                 // ignore species argument: irrelevant here
  // }

  private def newResolution (key:String): Map[String,String] = {
    return Map(
      "referenceID" -> "UAZ%05d".format(idCntr.next),
      "namespace" -> namespace,
      "baseURI" -> baseURI,
      "key" -> key
    )
  }
}
