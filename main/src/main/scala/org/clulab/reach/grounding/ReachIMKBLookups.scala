package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBConstants._

/**
  * Object which implements all Reach KB Lookup instances.
  *   Written by: Tom Hicks. 10/23/2015.
  *   Last Modified: Update for refactor of KB meta info.
  */
object ReachIMKBLookups {

  /** Single factory instance to generate Tsv IMKB classes. */
  val tsvIMKBFactory = new TsvIMKBFactory


  /** KB lookup to resolve subcellular location names via static KB. */
  def staticCellLocationKBLookup: IMKBLookup = {
    // val metaInfo = new IMKBMetaInfo("http://identifiers.org/go/", "MIR:00000022")
    // metaInfo.put("file", StaticCellLocationFilename)
    // new IMKBLookup(tsvIMKBFactory.make("go", StaticCellLocationFilename, metaInfo))
    val metaInfo = new IMKBMetaInfo(
      namespace = "go",
      kbFilename = Some(StaticCellLocationFilename),
      baseURI = "http://identifiers.org/go/",
      resourceId = "MIR:00000022"
    )
    new IMKBLookup(tsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve subcellular location names via static KB. */
  def staticCellLocation2KBLookup: IMKBLookup = {
    // val metaInfo = new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005")
    // metaInfo.put("file", StaticCellLocation2Filename)
    // new IMKBLookup(TsvIMKBFactory.make("uniprot", StaticCellLocation2Filename, metaInfo))
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticCellLocation2Filename),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00000005"
    )
    new IMKBLookup(tsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve small molecule (metabolite) names via static KB. */
  def staticMetaboliteKBLookup: IMKBLookup = {
    // val metaInfo = new IMKBMetaInfo("http://identifiers.org/hmdb/", "MIR:00000051")
    // metaInfo.put("file", StaticMetaboliteFilename)
    // new IMKBLookup(tsvIMKBFactory.make("hmbd", StaticMetaboliteFilename, metaInfo))
    val metaInfo = new IMKBMetaInfo(
      namespace = "hmdb",
      kbFilename = Some(StaticMetaboliteFilename),
      baseURI = "http://identifiers.org/hmdb/",
      resourceId = "MIR:00000051"
    )
    new IMKBLookup(tsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve small molecule (chemical) names via static KB. */
  def staticChemicalKBLookup: IMKBLookup = {
    // val metaInfo = new IMKBMetaInfo("http://identifiers.org/pubchem.compound/", "MIR:00000034")
    // metaInfo.put("file", StaticChemicalFilename)
    // new IMKBLookup(tsvIMKBFactory.make("pubchem", StaticChemicalFilename, metaInfo))
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticChemicalFilename),
      namespace = "pubchem",
      baseURI = "http://identifiers.org/pubchem.compound/",
      resourceId = "MIR:00000034"
    )
    new IMKBLookup(tsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve small molecule (drug) names via static KB. */
  def staticDrugKBLookup: IMKBLookup = {
    // val metaInfo = new IMKBMetaInfo("http://identifiers.org/pubchem.compound/", "MIR:00000034")
    // metaInfo.put("file", StaticDrugFilename)
    // new IMKBLookup(tsvIMKBFactory.make("pubchem", StaticDrugFilename, metaInfo))
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticDrugFilename),
      namespace = "pubchem",
      baseURI = "http://identifiers.org/pubchem.compound/",
      resourceId = "MIR:00000034"
    )
    new IMKBLookup(tsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve small molecule (chemical) names via static KB. */
  // def staticChemicalKBLookup: IMKBLookup = {
  //   val metaInfo = new IMKBMetaInfo(
  //     namespace = "chebi",
  //     kbFilename = Some(StaticChemicalFilename),
  //     baseURI = "http://identifiers.org/chebi/",
  //     resourceId = "MIR:00100009"
  //   )
  //   new IMKBLookup(tsvIMKBFactory.make(metaInfo))
  // }


  /** KB accessor to resolve protein names via static KBs with alternate lookups. */
  def staticProteinKBLookup: IMKBProteinLookup = {
    // val metaInfo = new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00100164")
    // metaInfo.put("file", StaticProteinFilename)
    // metaInfo.put("protein", "true")         // mark as from a protein KB
    // new IMKBProteinLookup(tsvIMKBFactory.make("uniprot", StaticProteinFilename, true, metaInfo))
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticProteinFilename),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00100164",
      hasSpeciesInfo = true,
      isProteinKB = true
    )
    new IMKBProteinLookup(tsvIMKBFactory.make(metaInfo))
  }

  /** KB accessor to resolve protein complex names via static KBs with alternate lookups. */
  def staticProteinComplexKBLookup: IMKBProteinLookup = {
    // val metaInfo = new IMKBMetaInfo("https://github.com/sorgerlab/bioentities")
    // metaInfo.put("file", StaticProteinComplexFilename)
    // metaInfo.put("protein", "true")         // mark as from a protein KB
    // new IMKBProteinLookup(tsvIMKBFactory.make("be", StaticProteinComplexFilename, metaInfo))
    val metaInfo = new IMKBMetaInfo(
      namespace = "be",
      kbFilename = Some(StaticProteinComplexFilename),
      baseURI = "https://github.com/sorgerlab/bioentities",
      isProteinKB = true
    )
    new IMKBProteinLookup(tsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve protein family names via static KBs with alternate lookups. */
  def staticProteinFamilyKBLookup: IMKBFamilyLookup = {
    // val metaInfo = new IMKBMetaInfo("http://identifiers.org/pfam/", "MIR:00000028")
    // metaInfo.put("file", StaticProteinFamilyFilename)
    // metaInfo.put("family", "true")          // mark as from a protein family KB
    // new IMKBFamilyLookup(tsvIMKBFactory.make("pfam", StaticProteinFamilyFilename, metaInfo))
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticProteinFamilyFilename),
      namespace = "pfam",
      baseURI = "http://identifiers.org/pfam/",
      resourceId = "MIR:00000028",
      isFamilyKB = true
    )
    new IMKBFamilyLookup(tsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve protein family names via static KBs with alternate lookups. */
  def staticProteinFamily2KBLookup: IMKBFamilyLookup = {
    // val metaInfo = new IMKBMetaInfo("http://identifiers.org/interpro/", "MIR:00000011")
    // metaInfo.put("file", StaticProteinFamily2Filename)
    // metaInfo.put("family", "true")          // mark as from a protein family KB
    // new IMKBFamilyLookup(tsvIMKBFactory.make("interpro", StaticProteinFamily2Filename, true, metaInfo))
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticProteinFamily2Filename),
      namespace = "interpro",
      baseURI = "http://identifiers.org/interpro/",
      resourceId = "MIR:00000011",
      hasSpeciesInfo = true,
      isFamilyKB = true
    )
    new IMKBFamilyLookup(tsvIMKBFactory.make(metaInfo))
  }

  /** KB lookup to resolve protein family names via static KBs with alternate lookups. */
  def staticProteinFamily0KBLookup: IMKBFamilyLookup = {
    // val metaInfo = new IMKBMetaInfo("https://github.com/sorgerlab/bioentities")
    // metaInfo.put("file", StaticProteinFamily0Filename)
    // metaInfo.put("family", "true")          // mark as from a protein family KB
    // new IMKBFamilyLookup(tsvIMKBFactory.make("be", StaticProteinFamily0Filename, metaInfo))
    val metaInfo = new IMKBMetaInfo(
      namespace = "be",
      kbFilename = Some(StaticProteinFamily0Filename),
      baseURI = "https://github.com/sorgerlab/bioentities",
      isFamilyKB = true
    )
    new IMKBFamilyLookup(tsvIMKBFactory.make(metaInfo))
  }

}
