package org.clulab.reach.grounding

import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

/**
  * Object which implements all Reach KB Mention Lookup creators and instances.
  *   Written by: Tom Hicks. 10/28/2015.
  *   Last Modified: Revert to using default (canonical) key transforms.
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
  val StaticChemicalChebi = staticChemicalKBMLChebi
  val StaticDrug = staticDrugKBML
  // val StaticMetabolite = staticMetaboliteKBML    // Replaced by PubChem
  val StaticProtein = staticProteinKBML
  val StaticGene = staticGeneKBML
  val staticProteinFamilyOrComplex = staticProteinFamilyOrComplexKBML

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
    new IMKBMentionLookup(AdHocIMKBFactory.make(metaInfo))
  }

  //
  // Subcellular Location Accessors
  //

  /** KB accessor to resolve subcellular location names via KBs generated from the BioPax model. */
  def gendCellLocationKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(kbFilename = Some(GendCellLocationFilename))
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB accessor to resolve subcellular location names via static KB. */
  def staticCellLocationKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "go",
      kbFilename = Some(StaticCellLocationFilename),
      baseURI = "http://identifiers.org/go/",
      resourceId = "MIR:00000022"
    )
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB accessor to resolve alternate subcellular location names via static KB. */
  def staticCellLocation2KBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticCellLocation2Filename),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00000005"
    )
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

  //
  // Small Molecule (Chemical and Metabolite) Accessors
  //

  /** KB accessor to resolve small molecule (chemical) names via KBs generated from the BioPax model. */
  def gendChemicalKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(kbFilename = Some(GendChemicalFilename))
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB accessor to resolve small molecule (metabolite) names via static KB. */
  def staticMetaboliteKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "hmdb",
      kbFilename = Some(StaticMetaboliteFilename),
      baseURI = "http://identifiers.org/hmdb/",
      resourceId = "MIR:00000051"
    )
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB accessor to resolve small molecule (chemical) names via static KB. */
  def staticChemicalKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticChemicalFilename),
      namespace = "pubchem",
      baseURI = "http://identifiers.org/pubchem.compound/",
      resourceId = "MIR:00000034"
    )
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB accessor to resolve small molecule (drug) names via static KB. */
  def staticDrugKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(StaticDrugFilename),
      namespace = "pubchem",
      baseURI = "http://identifiers.org/pubchem.compound/",
      resourceId = "MIR:00000034"
    )
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB accessor to resolve small molecule (chemical) names via static KB. */
  def staticChemicalKBMLChebi: IMKBMentionLookup = {
     val metaInfo = new IMKBMetaInfo(
       namespace = "chebi",
       kbFilename = Some(StaticChemicalFilename),
       baseURI = "http://identifiers.org/chebi/",
       resourceId = "MIR:00100009"
     )
     new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
   }


  //
  // Protein Accessors
  //

  /** KB accessor to resolve protein names via KBs generated from the BioPax model. */
  def gendProteinKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(GendProteinFilename),
      isProteinKB = true
    )
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
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
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, ProteinAuxKeyTransforms, DefaultKeyTransforms)
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve human gene names via static KB. */
  def staticGeneKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticGeneFilename),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00100164",
      hasSpeciesInfo = true,
      isProteinKB = true
    )
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, ProteinAuxKeyTransforms, DefaultKeyTransforms)
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
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
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
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, FamilyAuxKeyTransforms, DefaultKeyTransforms)
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
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, FamilyAuxKeyTransforms, DefaultKeyTransforms)
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve protein family names via static KB. */
  def staticProteinFamilyOrComplexKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "fplx",
      kbFilename = Some(StaticProteinFamilyOrComplexFilename),
      baseURI = "https://identifiers.org/fplx/",
      isFamilyKB = true
    )
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, FamilyAuxKeyTransforms, DefaultKeyTransforms)
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
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB accessor to resolve alternate cell lines via a context KB. */
  def contextCellLine2KBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "atcc",
      kbFilename = Some(ContextCellLine2Filename)
    )
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB accessor to resolve cell types via a context KB. */
  def contextCellTypeKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "cl",
      kbFilename = Some(ContextCellTypeFilename),
      baseURI = "http://identifiers.org/cl/",
      resourceId = "MIR:00000110"
    )
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
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
    val keyTransforms = KBKeyTransformsGroup(DefaultKeyTransforms, OrganAuxKeyTransforms, DefaultKeyTransforms)
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
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

  /** KB accessor to resolve tissue type names via context KB. */
  def contextTissueTypeKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "tissuelist",
      kbFilename = Some(ContextTissueTypeFilename),
      baseURI= "http://identifiers.org/tissuelist/",
      resourceId = "MIR:00000360"
    )
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
  }

}
