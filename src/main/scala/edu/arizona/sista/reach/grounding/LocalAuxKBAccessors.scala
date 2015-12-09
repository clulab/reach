package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._

import scala.io.Source

/**
  * A collection of classes which implement auxiliary knowledge base accessors.
  *   Written by Tom Hicks. 11/13/2015.
  *   Last Modified: Enable chemicals using ChemIDplus KB.
  */

/** KB accessor to resolve biological process names via an auxiliary KB. */
class AuxBioProcessKBAccessor extends LocalKBAccessor {
  override def baseURI = "http://identifiers.org/go/"
  override def namespace = "go"
  override def resourceID = "MIR:00000022"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(AuxBioProcessFilename))
}


/** KB accessor to resolve subcellular location names via an auxiliary KB. */
// class AuxCellLocationKBAccessor extends LocalKBAccessor {
//   override def baseURI = "http://identifiers.org/go/"
//   override def namespace = "go"
//   override def resourceID = "MIR:00000022"

//   // MAIN: load KB to initialize class
//   readAndFillKB(LocalKBUtils.makePathInKBDir(AuxCellLocationFilename))
// }


/** KB accessor to resolve small molecule (chemical) names via an auxiliary KB. */
class AuxChemicalKBAccessor extends LocalKBAccessor {
  override def baseURI = "http://identifiers.org/chemidplus/"
  override def namespace = "chemidplus"
  override def resourceID = "MIR:00000096"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(AuxChemicalFilename))
}


/** KB accessor to resolve small molecule (metabolite) names via an auxiliary KB. */
class AuxMetaboliteKBAccessor extends LocalKBAccessor {
  override def baseURI = "http://identifiers.org/hmdb/"
  override def namespace = "hmdb"
  override def resourceID = "MIR:00000051"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(AuxMetaboliteFilename))
}


/** KB accessor to resolve protein names via an auxiliary KB. */
class AuxProteinKBAccessor extends LocalProteinKBAccessor {
  override def baseURI = "http://identifiers.org/uniprot/"
  override def namespace = "uniprot"
  override def resourceID = "MIR:00100164"

  // MAIN: load KB to initialize class
  readAndFillKB(LocalKBUtils.makePathInKBDir(AuxProteinFilename))
}


/** KB accessor to resolve protein family names via an auxiliary KB. */
// class AuxProteinFamilyKBAccessor extends LocalProteinFamilyKBAccessor {
//   override def baseURI = "http://identifiers.org/interpro/"
//   override def namespace = "interpro"
//   override def resourceID = "MIR:00000011"

//   // MAIN: load KB to initialize class
//   readAndFillKB(LocalKBUtils.makePathInKBDir(AuxProteinFamilyFilename))
// }


/** KB accessor to resolve tissue type names via an auxiliary KB. */
// class AuxTissueTypeKBAccessor extends LocalKBAccessor {
//   override def baseURI = "http://identifiers.org/uniprot/"
//   override def namespace = "uniprot"
//   override def resourceID = "MIR:00000005"

//   // MAIN: load KB to initialize class
//   readAndFillKB(LocalKBUtils.makePathInKBDir(AuxTissueTypeFilename))
// }
