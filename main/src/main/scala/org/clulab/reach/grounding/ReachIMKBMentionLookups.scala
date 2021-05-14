package org.clulab.reach.grounding

import ai.lum.common.ConfigFactory
import com.typesafe.config.{Config, ConfigObject}
import org.clulab.reach.grounding.ReachKBConstants._
import org.clulab.reach.grounding.ReachKBKeyTransforms._

import java.io.File
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
    val priority = obj.getConfValue("priority", 1)

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
      kbFilename = Some(new File(path).getName),
      namespace = namespace,
      baseURI = baseURI,
      resourceId = resourceId,
      hasSpeciesInfo = hasSpeciesInfo,
      isFamilyKB = isFamilyKB,
      isProteinKB = isProteinKB,
      priority = priority
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
  lazy val configuredKBML:Map[String, Seq[IMKBMentionLookup]] = {
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
      _.map(_._2).sortBy(_.metaInfo.priority).reverse
    }
  }

}
