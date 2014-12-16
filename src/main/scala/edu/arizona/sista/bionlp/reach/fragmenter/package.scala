package edu.arizona.sista.bionlp.reach

import scala.reflect.ClassTag
import scala.collection.JavaConverters._
import org.biopax.paxtools.model.BioPAXElement
import org.biopax.paxtools.model.level3._
import org.biopax.paxtools.model.level3.ConversionDirectionType._

package object fragmenter {
  implicit class BioPAXElementUtils(elem: BioPAXElement) {
    def rdfId = elem.getRDFId
  }

  implicit class NamedUtils(named: Named) {
    def name = named.getDisplayName match {
      case null => ""
      case name => name
    }

    def allNames = named.getName.asScala.toSet

    def standardName = named.getStandardName match {
      case null => None
      case name => Some(name)
    }
  }

  implicit class XrefUtils(xref: Xref) {
    def db = xref.getDb
    def id = xref.getId
    def xrefOf = xref.getXrefOf.asScala.toSet
  }

  implicit class XReferrableUtils(referrable: XReferrable) {
    def xrefs = referrable.getXref.asScala.toSet
    def unifications = filterByClass[UnificationXref]
    def publications = filterByClass[PublicationXref]
    def relationships = filterByClass[RelationshipXref]

    private def filterByClass[T <: Xref: ClassTag] = xrefs flatMap { ref =>
      ref match {
        case x if implicitly[ClassTag[T]].runtimeClass.isInstance(x) => Some(x.asInstanceOf[T])
        case _ => None
      }
    }
  }

  implicit class EntityReferenceUtils(entityRef: EntityReference) {
    def entityReferenceOf = entityRef.getEntityReferenceOf.asScala.toSet
    def entityFeatures = entityRef.getEntityReferenceOf.asScala.toSet
  }

  implicit class EntityUtils(entity: Entity) {
    def participantOf = entity.getParticipantOf.asScala.toSet
  }

  implicit class PhysicalEntityUtils(entity: PhysicalEntity) {
    def features = entity.getFeature.asScala.toSet
    def notFeatures = entity.getNotFeature.asScala.toSet
    def bindingFeatures = filterByClass[BindingFeature]
    def covalentBindingFeatures = filterByClass[CovalentBindingFeature]
    def fragmentFeatures = filterByClass[FragmentFeature]
    def modificationFeatures = filterByClass[ModificationFeature]
    def cellularLocation = entity.getCellularLocation

    def expandEntity = entity match {
      case complex: Complex => complex.expandComplex
      case _ => Set(entity)
    }

    private def filterByClass[T <: EntityFeature: ClassTag] = features flatMap { feat =>
      feat match {
        case x if implicitly[ClassTag[T]].runtimeClass.isInstance(x) => Some(x.asInstanceOf[T])
        case _ => None
      }
    }
  }

  implicit class SimplePhysicalEntityUtils(entity: SimplePhysicalEntity) {
    def entityReference = entity.getEntityReference
    def genericEntityReferences = entity.getGenericEntityReferences.asScala.toSet
  }

  implicit class ConversionUtils(conv: Conversion) {
    def conversionDirection = conv.getConversionDirection
    def left = conv.getLeft.asScala.toSet
    def right = conv.getRight.asScala.toSet

    def input = conversionDirection match {
      case LEFT_TO_RIGHT => left
      case RIGHT_TO_LEFT => right
      case REVERSIBLE => throw new Error("no input for REVERSIBLE Conversion")
    }

    def output = conversionDirection match {
      case LEFT_TO_RIGHT => right
      case RIGHT_TO_LEFT => left
      case REVERSIBLE => throw new Error("no output for REVERSIBLE Conversion")
    }

    def conversionLabels = Labeler.conversionLabels(conv)

    def entityDiff = {
      val inputEntities = expandEntities(input).keySet
      val outputEntities = expandEntities(output).keySet
      Map("added" -> (outputEntities diff inputEntities), "removed" -> (inputEntities diff outputEntities))
    }
    def featureDiff =
      calcDiff(expandEntities(input) mapValues (_.features), expandEntities(output) mapValues (_.features))

    def notFeatureDiff =
      calcDiff(expandEntities(input) mapValues (_.notFeatures), expandEntities(output) mapValues (_.notFeatures))

    def locationDiff =
      calcDiff(expandEntities(input) mapValues (v => Set(v.cellularLocation)), expandEntities(output) mapValues (v => Set(v.cellularLocation)))

    private def calcDiff[T](input: Map[EntityReference, Set[T]], output: Map[EntityReference, Set[T]]) = {
      val entityRefs = input.keySet & output.keySet
      val changes = entityRefs map { eRef =>
        eRef -> Map(
          "added" -> (output(eRef) diff input(eRef)),
          "removed" -> (input(eRef) diff output(eRef))
        )
      }
      changes.toMap
    }

    private def expandEntities(entities: Set[PhysicalEntity]) =
      entities flatMap (_.expandEntity) flatMap { e =>
        e match {
          case spe: SimplePhysicalEntity => Some(spe)
          case _ => None
        }
      } groupBy (_.getEntityReference) mapValues (_.head)
 }

  implicit class ControlUtils(ctrl: Control) {
    def controller = ctrl.getController.asScala.toSet
    def controlled = ctrl.getControlled.asScala.toSet
    def controlType = ctrl.getControlType
  }

  implicit class CatalysisUtils(cat: Catalysis) {
    def cofactors = cat.getCofactor.asScala.toSet
    def catalysisDirection = cat.getCatalysisDirection
  }

  implicit class PathwayUtils(pathway: Pathway) {
    def pathwayComponents = pathway.getPathwayComponent.asScala.toSet
  }

  implicit class ProcessUtils(proc: Process) {
    def pathwayComponentOf = proc.getPathwayComponentOf.asScala.toSet
    def controlledOf = proc.getControlledOf.asScala.toSet
  }

  implicit class ComplexUtils(complex: Complex) {
    def components = complex.getComponent.asScala.toSet
    def simpleMembers = complex.getSimpleMembers.asScala.toSet

    def expandComplex: Set[PhysicalEntity] = components flatMap { component =>
      component match {
        case complex: Complex => complex.expandComplex
        case entity: PhysicalEntity => Set(entity)
        case _ => throw new Error("invalid component")
      }
    }
  }
}
