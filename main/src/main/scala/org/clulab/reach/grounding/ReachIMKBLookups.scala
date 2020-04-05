package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Object which implements all Reach KB Lookup instances.
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Revert to using default (canonical) key transforms.
  */
object ReachIMKBLookups {

  /** Single factory instance to generate Tsv IMKB classes. */
  val TsvIMKBFactory = new TsvIMKBFactory


  /** KB lookup to resolve subcellular location names via static KB. */
  def staticCellLocationKBLookup: IMKBLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "go",
      kbFilename = Some(StaticCellLocationFilename),
      baseURI = "http://identifiers.org/go/",
      resourceId = "MIR:00000022"
    )
    new IMKBLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve subcellular location names via static KB. */
  def staticCellLocation2KBLookup: IMKBLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticCellLocation2Filename),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00000005"
    )
    new IMKBLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve small molecule (metabolite) names via static KB. */
  def staticMetaboliteKBLookup: IMKBLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "hmdb",
      kbFilename = Some(StaticMetaboliteFilename),
      baseURI = "http://identifiers.org/hmdb/",
      resourceId = "MIR:00000051"
    )
    new IMKBLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve small molecule (chemical) names via static KB. */
  def staticChemicalKBLookup: IMKBLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticChemicalFilename),
      namespace = "pubchem",
      baseURI = "http://identifiers.org/pubchem.compound/",
      resourceId = "MIR:00000034"
    )
    new IMKBLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve small molecule (drug) names via static KB. */
  def staticDrugKBLookup: IMKBLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticDrugFilename),
      namespace = "pubchem",
      baseURI = "http://identifiers.org/pubchem.compound/",
      resourceId = "MIR:00000034"
    )
    new IMKBLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve small molecule (chemical) names via static KB. */
  // def staticChemicalKBLookup: IMKBLookup = {
  //   val metaInfo = new IMKBMetaInfo(
  //     namespace = "chebi",
  //     kbFilename = Some(StaticChemicalFilename),
  //     baseURI = "http://identifiers.org/chebi/",
  //     resourceId = "MIR:00100009"
  //   )
  //   new IMKBLookup(TsvIMKBFactory.make(metaInfo))
  // }



  /** KB accessor to resolve protein names via static KB. */
  def staticProteinKBLookup: IMKBLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticProteinFilename),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00100164",
      hasSpeciesInfo = true,
      isProteinKB = true
    )
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, ProteinAuxKeyTransforms, DefaultKeyTransforms)
    new IMKBLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }
  
  /** KB accessor to resolve human gene names via static KB. */
  def staticGeneKBLookup: IMKBLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticGeneFilename),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00100164",
      hasSpeciesInfo = true,
      isProteinKB = true
    )
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, ProteinAuxKeyTransforms, DefaultKeyTransforms)
    new IMKBLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve protein family and complex names via static KBs with alternate lookups. */
  def staticFamilyOrComplexKBLookup: IMKBLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "fplx",
      kbFilename = Some(StaticProteinFamilyOrComplexFilename),
      baseURI = "https://identifiers.org/fplx/",
      isProteinKB = true
    )
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, ProteinAuxKeyTransforms, DefaultKeyTransforms)
    new IMKBLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB lookup to resolve protein family names via static KBs with alternate lookups. */
  def staticProteinFamilyKBLookup: IMKBLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticProteinFamilyFilename),
      namespace = "pfam",
      baseURI = "http://identifiers.org/pfam/",
      resourceId = "MIR:00000028",
      isFamilyKB = true
    )
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, FamilyAuxKeyTransforms, DefaultKeyTransforms)
    new IMKBLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB lookup to resolve protein family names via static KBs with alternate lookups. */
  def staticProteinFamily2KBLookup: IMKBLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticProteinFamily2Filename),
      namespace = "interpro",
      baseURI = "http://identifiers.org/interpro/",
      resourceId = "MIR:00000011",
      hasSpeciesInfo = true,
      isFamilyKB = true
    )
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, FamilyAuxKeyTransforms, DefaultKeyTransforms)
    new IMKBLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }
}
