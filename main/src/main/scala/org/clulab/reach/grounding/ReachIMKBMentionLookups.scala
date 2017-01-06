package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Object which implements all Reach KB Mention Lookup creators and instances.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Remove old, commented IMKB instantiations.
  */
object ReachIMKBMentionLookups {

  /** Single factory instance to generate AdHoc IMKB classes. */
  val AdHocIMKBFactory = new AdHocIMKBFactory

  /** Single factory instance to generate Tsv IMKB classes. */
  val TsvIMKBFactory = new TsvIMKBFactory

  // Singleton instances of the Reach KBs

  val ContextCellLine = contextCellLineKBML
  val ContextCellLine2 = contextCellLine2KBML
  val ContextCellType = contextCellTypeKBML
  val ContextOrgan = contextOrganKBML
  val ContextSpecies = contextSpeciesKBML
  val ContextTissueType = contextTissueTypeKBML

  val StaticBioProcess = staticBioProcessKBML
  val StaticCellLocation = staticCellLocationKBML   // GO subcellular KB
  val StaticCellLocation2 = staticCellLocation2KBML // Uniprot subcellular KB
  val StaticChemical = staticChemicalKBML
  val StaticDrug = staticDrugKBML
  // val StaticMetabolite = staticMetaboliteKBML    // REPLACED by PubChem
  val StaticProtein = staticProteinKBML
  val StaticProteinComplex = staticProteinComplexKBML
  val StaticProteinFamily0 = staticProteinFamily0KBML
  val StaticProteinFamily = staticProteinFamilyKBML
  val StaticProteinFamily2 = staticProteinFamily2KBML

  val ModelGendCellLocation = gendCellLocationKBML
  val ModelGendChemical = gendChemicalKBML
  val ModelGendProteinAndFamily = gendProteinKBML // families included in generated KB


  //
  // Bio Processes Accessors
  //

  /** KB accessor to resolve bio process names via static KB. */
  def staticBioProcessKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(kbFilename = Some(StaticBioProcessFilename))
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(AdHocIMKBFactory.make(metaInfo, keyTransforms))
  }

  //
  // Subcellular Location Accessors
  //

  /** KB accessor to resolve subcellular location names via KBs generated from the BioPax model. */
  def gendCellLocationKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(kbFilename = Some(GendCellLocationFilename))
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve subcellular location names via static KB. */
  def staticCellLocationKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "go",
      kbFilename = Some(StaticCellLocationFilename),
      baseURI = "http://identifiers.org/go/",
      resourceId = "MIR:00000022"
    )
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve alternate subcellular location names via static KB. */
  def staticCellLocation2KBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticCellLocation2Filename),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00000005"
    )
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  //
  // Small Molecule (Chemical and Metabolite) Accessors
  //

  /** KB accessor to resolve small molecule (chemical) names via KBs generated from the BioPax model. */
  def gendChemicalKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(kbFilename = Some(GendChemicalFilename))
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve small molecule (metabolite) names via static KB. */
  def staticMetaboliteKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "hmdb",
      kbFilename = Some(StaticMetaboliteFilename),
      baseURI = "http://identifiers.org/hmdb/",
      resourceId = "MIR:00000051"
    )
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve small molecule (chemical) names via static KB. */
  def staticChemicalKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticChemicalFilename),
      namespace = "pubchem",
      baseURI = "http://identifiers.org/pubchem.compound/",
      resourceId = "MIR:00000034"
    )
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve small molecule (drug) names via static KB. */
  def staticDrugKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticDrugFilename),
      namespace = "pubchem",
      baseURI = "http://identifiers.org/pubchem.compound/",
      resourceId = "MIR:00000034"
    )
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve small molecule (chemical) names via static KB. */
  // def staticChemicalKBML: IMKBMentionLookup = {
  //   val metaInfo = new IMKBMetaInfo(
  //     namespace = "chebi",
  //     kbFilename = Some(StaticChemicalFilename),
  //     baseURI = "http://identifiers.org/chebi/",
  //     resourceId = "MIR:00100009"
  //   )
  //   val keyTransforms = new IMKBKeyTransforms()
  //   new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  // }


  //
  // Protein Accessors
  //

  /** KB accessor to resolve protein names via KBs generated from the BioPax model. */
  def gendProteinKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(GendProteinFilename),
      isProteinKB = true
    )
    // val keyTransforms = new IMKBKeyTransforms(ProteinKeyTransforms, ProteinKeyTransforms)
    val keyTransforms = new IMKBKeyTransforms() // REPLACE LATER WITH ABOVE
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve protein names via static KB. */
  def staticProteinKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticProteinFilename),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00100164",
      hasSpeciesInfo = true,
      isProteinKB = true
    )
    // val keyTransforms = new IMKBKeyTransforms(ProteinKeyTransforms, ProteinKeyTransforms)
    val keyTransforms = new IMKBKeyTransforms() // REPLACE LATER WITH ABOVE
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve protein complex names via static KB. */
  def staticProteinComplexKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "be",
      kbFilename = Some(StaticProteinComplexFilename),
      baseURI = "https://github.com/sorgerlab/bioentities",
      isProteinKB = true                    // treat complexes as a protein KB
    )
    // val keyTransforms = new IMKBKeyTransforms(ProteinKeyTransforms, ProteinKeyTransforms)
    val keyTransforms = new IMKBKeyTransforms() // REPLACE LATER WITH ABOVE
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }


  //
  // Protein Family Accessors
  //

  /** KB accessor to resolve protein family names via KBs generated from the BioPax model. */
  def gendProteinFamilyKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(GendProteinFilename),
      isFamilyKB = true
    )
    // val keyTransforms = new IMKBKeyTransforms(FamilyKeyTransforms, FamilyKeyTransforms)
    val keyTransforms = new IMKBKeyTransforms() // REPLACE LATER WITH ABOVE
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve protein family names via static KB. */
  def staticProteinFamilyKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticProteinFamilyFilename),
      namespace = "pfam",
      baseURI = "http://identifiers.org/pfam/",
      resourceId = "MIR:00000028",
      isFamilyKB = true
    )
    // val keyTransforms = new IMKBKeyTransforms(FamilyKeyTransforms, FamilyKeyTransforms)
    val keyTransforms = new IMKBKeyTransforms() // REPLACE LATER WITH ABOVE
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve protein family names via static KB. */
  def staticProteinFamily2KBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticProteinFamily2Filename),
      namespace = "interpro",
      baseURI = "http://identifiers.org/interpro/",
      resourceId = "MIR:00000011",
      hasSpeciesInfo = true,
      isFamilyKB = true
    )
    // val keyTransforms = new IMKBKeyTransforms(FamilyKeyTransforms, FamilyKeyTransforms)
    val keyTransforms = new IMKBKeyTransforms() // REPLACE LATER WITH ABOVE
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve protein family names via static KB. */
  def staticProteinFamily0KBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "be",
      kbFilename = Some(StaticProteinFamily0Filename),
      baseURI = "https://github.com/sorgerlab/bioentities",
      isFamilyKB = true
    )
    // val keyTransforms = new IMKBKeyTransforms(FamilyKeyTransforms, FamilyKeyTransforms)
    val keyTransforms = new IMKBKeyTransforms() // REPLACE LATER WITH ABOVE
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }


  //
  // Context-related Accessors
  //

  /** KB accessor to resolve cell lines via a context KB. */
  def contextCellLineKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "cellosaurus",
      kbFilename = Some(ContextCellLineFilename),
      hasSpeciesInfo = true
    )
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve alternate cell lines via a context KB. */
  def contextCellLine2KBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "atcc",
      kbFilename = Some(ContextCellLine2Filename)
    )
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve cell types via a context KB. */
  def contextCellTypeKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "cl",
      kbFilename = Some(ContextCellTypeFilename),
      baseURI = "http://identifiers.org/cl/",
      resourceId = "MIR:00000110"
    )
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve organ names via a context KB.
      Uses alternate key lookups for organ to cell type inference. */
  def contextOrganKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "uberon",
      kbFilename = Some(ContextOrganFilename),
      baseURI = "http://identifiers.org/uberon/",
      resourceId = "MIR:00000446"
    )
    // val keyTransforms = new IMKBKeyTransforms(OrganKeyTransforms, OrganKeyTransforms)
    val keyTransforms = new IMKBKeyTransforms() // REPLACE LATER WITH ABOVE
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve species names via a context KB. */
  def contextSpeciesKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "taxonomy",
      kbFilename = Some(ContextSpeciesFilename),
      baseURI = "http://identifiers.org/taxonomy/",
      resourceId= "MIR:00000006"
    )
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve tissue type names via context KB. */
  def contextTissueTypeKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "tissuelist",
      kbFilename = Some(ContextTissueTypeFilename),
      baseURI= "http://identifiers.org/tissuelist/",
      resourceId = "MIR:00000360"
    )
    val keyTransforms = new IMKBKeyTransforms()
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

}
