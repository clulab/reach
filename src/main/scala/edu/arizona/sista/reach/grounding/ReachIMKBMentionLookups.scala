package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBConstants._

/**
  * Object which implements all Reach KB Mention Lookup creators and instances.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Refactor singleton instances of KBMLs here.
  */
object ReachIMKBMentionLookups {

  /** Single factory instance to generate AdHoc IMKB classes. */
  val AdHocIMKBFactory = new AdHocIMKBFactory

  /** Single factory instance to generate Tsv IMKB classes. */
  val TsvIMKBFactory = new TsvIMKBFactory

  // Singleton instances of the Reach KBs

  val ContextCellLine = contextCellLineKBML
  val ContextCellType = contextCellTypeKBML
  val ContextOrgan = contextOrganKBML
  val ContextSpecies = contextSpeciesKBML

  val StaticBioProcess = staticBioProcessKBML
  val StaticCellLocation = staticCellLocationKBML   // GO subcellular KB
  val StaticCellLocation2 = staticCellLocationKBML2 // Uniprot subcellular KB
  val StaticChemical = staticChemicalKBML
  val StaticMetabolite = staticMetaboliteKBML
  val StaticProtein = staticProteinKBML
  val StaticProteinFamily = staticProteinFamilyKBML
  // val StaticTissueType = staticTissueTypeKBML()   // CURRENTLY UNUSED

  val ManualCellLocation = manualCellLocationKBML
  val ManualChemical = manualChemicalKBML
  val ManualProtein = manualProteinKBML
  val ManualProteinFamily = manualProteinFamilyKBML

  val ModelGendCellLocation = gendCellLocationKBML
  val ModelGendChemical = gendChemicalKBML
  val ModelGendProteinAndFamily = gendProteinKBML // families included in generated KB


  //
  // Bio Processes Accessors
  //

  /** KB accessor to resolve bio process names via static KB. */
  def staticBioProcessKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", StaticBioProcessFilename)
    new IMKBMentionLookup(AdHocIMKBFactory.make(StaticBioProcessFilename, metaInfo))
  }

  //
  // Subcellular Location Accessors
  //

  /** KB accessor to resolve subcellular location names via KBs generated from the BioPax model. */
  def gendCellLocationKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", GendCellLocationFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make(GendCellLocationFilename, metaInfo))
  }

  /** KB accessor to resolve subcellular location names via manually maintained KBs. */
  def manualCellLocationKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ManualCellLocationFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make(ManualCellLocationFilename, metaInfo))
  }

  /** KB accessor to resolve subcellular location names via static KB. */
  def staticCellLocationKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/go/", "MIR:00000022")
    metaInfo.put("file", StaticCellLocationFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make("go", StaticCellLocationFilename, metaInfo))
  }

  /** KB accessor to resolve alternate subcellular location names via static KB. */
  def staticCellLocationKBML2: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005")
    metaInfo.put("file", StaticCellLocation2Filename)
    new IMKBMentionLookup(TsvIMKBFactory.make("uniprot", StaticCellLocation2Filename, metaInfo))
  }

  //
  // Small Molecule (Chemical and Metabolite) Accessors
  //

  /** KB accessor to resolve small molecule (chemical) names via KBs generated from the BioPax model. */
  def gendChemicalKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", GendChemicalFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make(GendChemicalFilename, metaInfo))
  }

  /** KB accessor to resolve small molecule (chemical) names via manually maintained KBs. */
  def manualChemicalKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ManualChemicalFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make(ManualChemicalFilename, metaInfo))
  }

  /** KB accessor to resolve small molecule (metabolite) names via static KB. */
  def staticMetaboliteKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/hmdb/", "MIR:00000051")
    metaInfo.put("file", StaticMetaboliteFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make("hmdb", StaticMetaboliteFilename, metaInfo))
  }

  /** KB accessor to resolve small molecule (chemical) names via static KB. */
  def staticChemicalKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/chebi/", "MIR:00100009")
    metaInfo.put("file", StaticChemicalFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make("chebi", StaticChemicalFilename, metaInfo))
  }

  //
  // Protein Accessors
  //

  /** KB accessor to resolve protein names via KBs generated from the BioPax model. */
  def gendProteinKBML: IMKBProteinMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", GendProteinFilename)
    metaInfo.put("protein", "true")         // mark as from a protein KB
    new IMKBProteinMentionLookup(TsvIMKBFactory.make(GendProteinFilename, metaInfo))
  }

  /** KB accessor to resolve protein names via manually maintained KBs. */
  def manualProteinKBML: IMKBProteinMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ManualProteinFilename)
    metaInfo.put("protein", "true")         // mark as from a protein KB
    new IMKBProteinMentionLookup(TsvIMKBFactory.make(ManualProteinFilename, metaInfo))
  }

  /** KB accessor to resolve protein names via static KB. */
  def staticProteinKBML: IMKBProteinMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00100164")
    metaInfo.put("file", StaticProteinFilename)
    metaInfo.put("protein", "true")         // mark as from a protein KB
    new IMKBProteinMentionLookup(TsvIMKBFactory.make("uniprot", StaticProteinFilename, true, metaInfo))
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
    metaInfo.put("family", "true")          // mark as from a protein family KB
    new IMKBFamilyMentionLookup(TsvIMKBFactory.make(GendProteinFilename, metaInfo))
  }

  /** KB accessor to resolve protein names via manually maintained KBs. */
  def manualProteinFamilyKBML: IMKBFamilyMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ManualProteinFilename)
    metaInfo.put("family", "true")          // mark as from a protein family KB
    new IMKBFamilyMentionLookup(TsvIMKBFactory.make(ManualProteinFilename, metaInfo))
  }

  /** KB accessor to resolve protein family names via static KB. */
  def staticProteinFamilyKBML: IMKBFamilyMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/interpro/", "MIR:00000011")
    metaInfo.put("file", StaticProteinFamilyFilename)
    metaInfo.put("family", "true")          // mark as from a protein family KB
    new IMKBFamilyMentionLookup(TsvIMKBFactory.make("interpro",
                                                    StaticProteinFamilyFilename, true, metaInfo))
  }

  //
  // Tissue Type Accessor
  //

  /** KB accessor to resolve tissue type names via static KB. */
  def staticTissueTypeKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/uniprot/", "MIR:00000005")
    metaInfo.put("file", StaticTissueTypeFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make("uniprot", StaticTissueTypeFilename, metaInfo))
  }

  //
  // Context-related Accessors
  //

  /** KB accessor to resolve cell lines via a context KB. */
  def contextCellLineKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ContextCellLineFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make(ContextCellLineFilename, metaInfo))
  }

  /** KB accessor to resolve cell types via a context KB.
    Uses alternate key lookups for organ to cell type inference. */
  def contextCellTypeKBML: IMKBOrganCellTypeMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ContextCellTypeFilename)
    new IMKBOrganCellTypeMentionLookup(TsvIMKBFactory.make(ContextCellTypeFilename, metaInfo))
  }

  /** KB accessor to resolve organ names via a context KB. */
  def contextOrganKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo()
    metaInfo.put("file", ContextOrganFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make(ContextOrganFilename, metaInfo))
  }

  /** KB accessor to resolve species names via a context KB. */
  def contextSpeciesKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo("http://identifiers.org/taxonomy/", "MIR:00000006")
    metaInfo.put("file", ContextSpeciesFilename)
    new IMKBMentionLookup(TsvIMKBFactory.make("taxonomy", ContextSpeciesFilename, metaInfo))
  }

}
