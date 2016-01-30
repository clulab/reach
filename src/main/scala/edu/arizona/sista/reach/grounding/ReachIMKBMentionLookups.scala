package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Object which implements all Reach KB Mention Lookup instances.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Rewrite as singleton for all KBMLs.
  */

object ReachIMKBMentionLookups {

  /** Single factory instance to generate AdHoc IMKB classes. */
  val adHocIMKBFactory = new AdHocIMKBFactory

  /** Single factory instance to generate Tsv IMKB classes. */
  val tsvIMKBFactory = new TsvIMKBFactory

  //
  // Bio Processes Accessors
  //

  /** KB accessor to resolve bio process names via static KB. */
  def staticBioProcessKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", StaticBioProcessFilename)
    new IMKBMentionLookup(adHocIMKBFactory.make(StaticBioProcessFilename, metaInfo))
  }

  //
  // Subcellular Location Accessors
  //

  /** KB accessor to resolve subcellular location names via KBs generated from the BioPax model. */
  def gendCellLocationKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", GendCellLocationFilename)
    new IMKBMentionLookup(tsvIMKBFactory.make(GendCellLocationFilename, metaInfo))
  }

  /** KB accessor to resolve subcellular location names via manually maintained KBs. */
  def manualCellLocationKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ManualCellLocationFilename)
    new IMKBMentionLookup(tsvIMKBFactory.make(ManualCellLocationFilename, metaInfo))
  }

  /** KB accessor to resolve subcellular location names via static KB. */
  def staticCellLocationKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/go/", "MIR:00000022")
    metaInfo.put("file", StaticCellLocationFilename)
    new IMKBMentionLookup(tsvIMKBFactory.make("go", StaticCellLocationFilename, metaInfo))
  }

  /** KB accessor to resolve alternate subcellular location names via static KB. */
  def staticCellLocationKBML2: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005")
    metaInfo.put("file", StaticCellLocation2Filename)
    new IMKBMentionLookup(tsvIMKBFactory.make("uniprot", StaticCellLocation2Filename, metaInfo))
  }

  //
  // Small Molecule (Chemical and Metabolite) Accessors
  //

  /** KB accessor to resolve small molecule (chemical) names via KBs generated from the BioPax model. */
  def gendChemicalKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", GendChemicalFilename)
    new IMKBMentionLookup(tsvIMKBFactory.make(GendChemicalFilename, metaInfo))
  }

  /** KB accessor to resolve small molecule (chemical) names via manually maintained KBs. */
  def manualChemicalKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ManualChemicalFilename)
    new IMKBMentionLookup(tsvIMKBFactory.make(ManualChemicalFilename, metaInfo))
  }

  /** KB accessor to resolve small molecule (metabolite) names via static KB. */
  def staticMetaboliteKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/hmdb/", "MIR:00000051")
    metaInfo.put("file", StaticMetaboliteFilename)
    new IMKBMentionLookup(tsvIMKBFactory.make("hmdb", StaticMetaboliteFilename, metaInfo))
  }

  /** KB accessor to resolve small molecule (chemical) names via static KB. */
  def staticChemicalKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/chebi/", "MIR:00100009")
    metaInfo.put("file", StaticChemicalFilename)
    new IMKBMentionLookup(tsvIMKBFactory.make("chebi", StaticChemicalFilename, metaInfo))
  }

  //
  // Protein Accessors
  //

  /** KB accessor to resolve protein names via KBs generated from the BioPax model. */
  def gendProteinKBML: IMKBProteinMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", GendProteinFilename)
    new IMKBProteinMentionLookup(tsvIMKBFactory.make(GendProteinFilename, metaInfo))
  }

  /** KB accessor to resolve protein names via manually maintained KBs. */
  def manualProteinKBML: IMKBProteinMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ManualProteinFilename)
    new IMKBProteinMentionLookup(tsvIMKBFactory.make(ManualProteinFilename, metaInfo))
  }

  /** KB accessor to resolve protein names via static KB. */
  def staticProteinKBML: IMKBProteinMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00100164")
    metaInfo.put("file", StaticProteinFilename)
    new IMKBProteinMentionLookup(tsvIMKBFactory.make("uniprot", StaticProteinFilename, true, metaInfo))
  }

  //
  // Protein Family Accessors
  //   These extend from LocalAltKBMentionLookup because protein & protein family
  //   alternate lookups are the same for now.
  //

  /** KB accessor to resolve protein family names via KBs generated from the BioPax model. */
  def gendProteinFamilyKBML: IMKBFamilyMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", GendProteinFilename)
    new IMKBFamilyMentionLookup(tsvIMKBFactory.make(GendProteinFilename, metaInfo))
  }

  /** KB accessor to resolve protein names via manually maintained KBs. */
  def manualProteinFamilyKBML: IMKBFamilyMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ManualProteinFilename)
    new IMKBFamilyMentionLookup(tsvIMKBFactory.make(ManualProteinFilename, metaInfo))
  }

  /** KB accessor to resolve protein family names via static KB. */
  def staticProteinFamilyKBML: IMKBFamilyMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/interpro/", "MIR:00000011")
    metaInfo.put("file", StaticProteinFamilyFilename)
    new IMKBFamilyMentionLookup(tsvIMKBFactory.make("interpro",
                                                    StaticProteinFamilyFilename, true, metaInfo))
  }

  //
  // Tissue Type Accessor
  //

  /** KB accessor to resolve tissue type names via static KB. */
  def staticTissueTypeKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005")
    metaInfo.put("file", StaticTissueTypeFilename)
    new IMKBMentionLookup(tsvIMKBFactory.make("uniprot", StaticTissueTypeFilename, metaInfo))
  }

}
