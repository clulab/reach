package org.clulab.reach.grounding

import ai.lum.common.ConfigFactory
import com.typesafe.config.{Config, ConfigObject}
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.convert.ImplicitConversions._
import scala.collection.mutable

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
  val StaticDisease = staticDiseaseKBML
  val StaticCellLocation = staticCellLocationKBML   // GO subcellular KB
  val StaticCellLocation2 = staticCellLocation2KBML // Uniprot subcellular KB
  val StaticChemical = staticChemicalKBML
  val StaticChemicalChebi = staticChemicalKBMLChebi
  val StaticDrug = staticDrugKBML
  // val StaticMetabolite = staticMetaboliteKBML    // Replaced by PubChem
  val StaticProtein = staticProteinKBML
  val StaticProtein2 = staticProteinKBML2
  val StaticProtein3 = staticProteinKBML3
  val StaticProteinFragment = staticProteinFragmentKBML
  val staticProteinFamilyOrComplex = staticProteinFamilyOrComplexKBML

  val StaticProteinFamily = staticProteinFamilyKBML
  val StaticProteinFamily2 = staticProteinFamily2KBML

  val ModelGendCellLocation = gendCellLocationKBML
  val ModelGendChemical = gendChemicalKBML
  val ModelGendProteinAndFamily = gendProteinKBML // families included in generated KB


  /**
    * Utility class to reduce boilerplate. Extends the Config objects with getters with default values.
    * Tagged as implicit to be able to use nicely w/o wrapping the objects explicitly
    * @param obj instance to decorate with the convenience methods
    */
  implicit class RichConfig(obj:Config){

    /**
      * Convenience method to reduce boiler-plate to get the value of a Conf object or a default if not present
      * @param path to the value to fetch
      * @param default value to return if path not present
      * @tparam T Type of the return value
      * @return Value from the conf object or default provided
      */
    def getConfValue[T](path:String, default:T):T = {
      if(obj.hasPath(path))
        obj.getAnyRef(path).asInstanceOf[T]
      else
        default
    }

    /**
      * Convenience method to reduce boiler-plate to get the list of values of a Conf object or a default if not present
      * @param path to the value to fetch
      * @param default value to return if path not present
      * @tparam T Type of the return value
      * @return Value from the conf object or default provided
      */
    def getConfList[T](path:String, default:List[T]):List[T] = {
      if(obj.hasPath(path))
        obj.getAnyRefList(path).asScala.toList.map(_.asInstanceOf[T])
      else
        default
    }
  }



  /**
    * Builds an IMKBMentionLookup instance from a subsection of a Conf object
    * @param obj Sub-section of the conf object with the data
    * @return Tuple containing the configured labels and a reference to the KB instance
    */
  def buildKb(obj: Config): (Seq[String], IMKBMentionLookup) = {

    val path = obj.getString("path") // This one must exist in obj

    val namespace = obj.getConfValue("namespace", DefaultNamespace)
    val baseURI = obj.getConfValue("baseURI", "")
    val resourceId = obj.getConfValue("resourceId", "")
    val hasSpeciesInfo = obj.getConfValue("hasSpeciesInfo", false)
    val isFamilyKB = obj.getConfValue("isFamilyKB", false)
    val isProteinKB = obj.getConfValue("isProteinKB", false)
    val isAdHoc = obj.getConfValue("isAdHoc", false)
    val priority = obj.getConfValue("priority", 1) // TODO Not used yet, might remove later

    // Fetch the labels.
    val labels = obj.getConfList("labels", List("BioEntity"))

    // Get the transform function names, if specified
    val transforms = obj.getConfList[String]("keyTransforms", Nil)

    // Map the transform names to the hard-coded properties
    val transformsGroup =
      transforms.size match {
        case 0 => KBKeyTransformsGroup()
        case _ =>
          val trs = transforms map {
            case "DefaultKeyTransforms" => DefaultKeyTransforms
            case "ProteinAuxKeyTransforms" => ProteinAuxKeyTransforms
            case "CasedKeyTransforms" => CasedKeyTransforms
            case "IdentityKeyTransforms" => IdentityKeyTransforms
            case "FamilyAuxKeyTransforms" => FamilyAuxKeyTransforms
            case "OrganAuxKeyTransforms" => OrganAuxKeyTransforms
            case _ => throw new RuntimeException("Unspecified KBTransforms group")
          }

          assert(trs.size == 3, "There must be only three transforms if specified in the config file")

          KBKeyTransformsGroup(trs.head, trs(1), trs(2))

      }


    // Build metadata object
    val metaInfo = new IMKBMetaInfo(
      kbFilename = Some(path),
      namespace = namespace,
      baseURI = baseURI,
      resourceId = resourceId,
      hasSpeciesInfo = hasSpeciesInfo,
      isFamilyKB = isFamilyKB,
      isProteinKB = isProteinKB
    )

    // Create the KB instance
    val kb =
      if(isAdHoc)
        AdHocIMKBFactory.make(metaInfo, transformsGroup)
      else
        TsvIMKBFactory.make(metaInfo, transformsGroup)

    // Return the labels assigned to this kb and the actual reference to the kb
    (labels, new IMKBMentionLookup(kb))
  }

  // Dynamically load the KBs from the config file, by Enrique
  val configuredKBML:Map[String, Seq[IMKBMentionLookup]] = {
    val conf = ConfigFactory.load()
    val kbConf = conf.getConfig("KnowledgeBases")

    val loadedKBs = mutable.ListBuffer[(String, IMKBMentionLookup)]()
    // Load all the KBs specified in the configuraction file of bioresources
    //val loadedKBs =
      (kbConf.root() foreach  {
        case (_, obj:ConfigObject) =>
          val (labels, kb) = buildKb(obj.toConfig)
          loadedKBs ++= (labels map (l => (l, kb)))
        case _ =>
          throw new RuntimeException("Error in the configuration file of bioresources")
      })

    // Make them a dictionary where the key is label and the value are the references to KBs with this label
    loadedKBs groupBy {
      case (label, _) => label
    } mapValues {
      y => y.map(_._2)
    }
  }


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
       kbFilename = Some(StaticChemicalFilenameChebi),
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

  def staticProteinKBML2: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticProteinFilename2),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00100164",
      hasSpeciesInfo = true,
      isProteinKB = true
    )
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, ProteinAuxKeyTransforms, DefaultKeyTransforms)
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  def staticProteinKBML3: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "uniprot",
      kbFilename = Some(StaticProteinFilename3),
      baseURI = "http://identifiers.org/uniprot/",
      resourceId = "MIR:00100164",
      hasSpeciesInfo = true,
      isProteinKB = true
    )
    val keyTransforms = new KBKeyTransformsGroup(DefaultKeyTransforms, ProteinAuxKeyTransforms, DefaultKeyTransforms)
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo, keyTransforms))
  }

  /** KB accessor to resolve protein fragment names via static KB. */
  def staticProteinFragmentKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "proonto", // the Protein Ontology
      kbFilename = Some(StaticProteinFragmentFilename),
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

  /** KB accessor to resolve disease names via static KB. */
  def staticDiseaseKBML: IMKBMentionLookup = {
    val metaInfo = new IMKBMetaInfo(
      namespace = "mesh",
      kbFilename = Some(StaticDiseaseFilename),
      baseURI = "http://identifiers.org/mesh/",
      resourceId = "MIR:00000560"
    )
    new IMKBMentionLookup(TsvIMKBFactory.make(metaInfo))
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
